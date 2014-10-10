{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}
-- | DNA monad and helper functions
module DNA.DNA (
      -- * DNA monad
      DNA(..)
    , runDNA
    , GroupID
    , S(..)
    , liftP
    , getNodes
    , getNCrashed
    , registerPID
      -- * Promises
    , Promise(..)
    , await
    , Group(..)
    , GroupType(..)
    , gather
      -- * Scattering
    , Scatter
    , runScatter
    , same
    , scatter
      -- * Spawning of actors
    , Actor(..)
    , actor
    , runActor
    , spawnActor
    , eval
    , forkLocal
    , forkRemote
    , forkGroup
    , forkGroupFailout
      -- * CH
    , __remoteTable
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Distributed.Static (closureApply)
import Control.Distributed.Process hiding (say)
import Control.Distributed.Process.Closure
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as Unsafe
import Control.Distributed.Process.Serializable (Serializable)

import Data.Binary   (Binary)
import Data.Int
import Data.Typeable (Typeable)
import Data.Monoid   (Monoid(..))
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map
import GHC.Generics  (Generic)

import DNA.Logging
import DNA.Types


----------------------------------------------------------------
-- Monad for building programs and data types
----------------------------------------------------------------

-- | Monad for defining DNA programs
newtype DNA a = DNA (StateT S Process a)
                deriving (Functor,Applicative,Monad,MonadIO)

runDNA :: [NodeId] -> DNA a -> Process a
runDNA nodes (DNA dna) = evalStateT dna (S 0 nodes mempty mempty mempty)


-- | ID of group of processes
type GroupID = Int

-- | State of DNA program. We track PIDs of all spawned processes.
data S = S
    { stCounter :: !Int
      -- ^ Counter for obtaining unique keys
    , stNodes   :: [NodeId]
      -- ^ Set of nodes which process could use.
    , stPIDS    :: !(Set.Set ProcessId)
      -- ^ PIDs for which crashing is fatal error
    , stGroups  :: !(Map.Map ProcessId GroupID)
      -- ^ PIDs for which crashing is not fatal error. Thus we have to
      --   track state
    , stGroupFailures :: !(Map.Map GroupID Int)
      -- ^ Number of failures for given process group
    }

liftP :: Process a -> DNA a
liftP = DNA . lift

getNCrashed :: GroupID -> DNA Int
getNCrashed gid = DNA $ do
  st <- get
  return $ stGroupFailures st Map.! gid

getNodes :: DNA [NodeId]
getNodes = DNA $ stNodes <$> get

registerPID :: ProcessId -> DNA ()
registerPID pid = DNA $ do
  st <- get
  put $! st { stPIDS = Set.insert pid (stPIDS st) }

freshGID :: DNA GroupID
freshGID = DNA $ do
  st <- get
  put $ st { stCounter = stCounter st + 1 }
  return $ stCounter st


----------------------------------------------------------------
-- Promises
----------------------------------------------------------------

-- | Normally subprocess will only return value once. Fork* function
--   return promise which is hadle for obtaining result of
--   computation.
--
--   We use cloud haskell channels for commutincations instead of
--   send\/expect because this way we cannot mix up messages from
--   different sources.
--
--   FIXME: If we await more than once we await more than once we wll
--          deadlock and we have no way to detect such behaviour. Nor
--          we can detect if we do not await for promise. In this case
--          child process will be termintaed forcefully when parent
--          dies.
newtype Promise a = Promise (ReceivePort a)


-- | Set of values which is produces by group of processes which
--   execute same code.
data Group a = Group
    !GroupType       -- Type of group of processes
    (ReceivePort a)  -- Port for reading data from


-- | Type of group
data GroupType
    -- | Child processes' crashes are fatal. Fields are number of
    --   pending results.
    = Reliable !Int
    -- | Children may crash and we simply discard their results.
    --   Fields are
    | FailOut  !Int !GroupID

-- Process termination
data ProcTerm
    = TermOK    ProcessId
    | Crashed   ProcessId

-- Wait for data from channel. We may receive message about process
-- termination instead
waitForChan :: ReceivePort a -> Process (Either ProcTerm a)
waitForChan ch = receiveWait
    [ matchChan ch (return . Right)
    , match $ \(ProcessMonitorNotification _ pid reason) ->
        case reason of
          DiedNormal -> return $ Left $ TermOK pid
          _          -> return $ Left $ Crashed pid
    ]


-- Handle process termination message
handleTermination :: ProcTerm -> DNA ()
handleTermination (TermOK pid) = DNA $ do
    -- We simply delete PID from every list of running processes if it
    -- terminates normally.
    st <- get
    put $! st { stPIDS   = Set.delete pid (stPIDS st)
              , stGroups = Map.delete pid (stGroups st)
              }
handleTermination (Crashed pid) = DNA $ do
    -- If process is allowed to termiante we update number of failures
    -- for curresponding group. Otherwise we just die violently.
    st <- get
    case Map.lookup pid (stGroups st) of
      Just gid -> put $! st
                    { stGroups = Map.delete pid (stGroups st)
                    , stGroupFailures = Map.adjust (+1) gid (stGroupFailures st)
                    }
      Nothing -> lift terminate


getval :: ReceivePort a -> DNA a
getval ch = do
    ea <- liftP $ waitForChan ch
    case ea of
        Left  t -> handleTermination t >> getval ch
        Right a -> return a


-- | Await result from promise. Function will block.
--
--   This function will as well receive messages about termination of
--   monitored processes.
--
--   FIXME: Currently we terminate when child dies abnormally. But for
--          some processes we may want to use different strategy.
--          We need to keep some data about known childs etc.
await :: Serializable a => Promise a -> DNA a
await (Promise ch) = getval ch


-- | Gather all results from child processes.
gather :: Group a -> (a -> a -> a) -> a -> DNA a
gather (Group gType ch) op = case gType of
    Reliable n     -> loopR n
    FailOut  n gid -> loopF gid n
  where
    -- Reliable subprocesses
    loopR n a
        | n <= 0    = return a
        | otherwise = do a' <- getval ch
                         loopR (n-1) (op a a')
    -- Fail-out
    loopF gid n a = do
        nCrash <- getNCrashed gid
        case n <= nCrash of
          True  -> return a
          False -> do a' <- getval ch
                      loopF gid (n-1) (op a a')



----------------------------------------------------------------
-- Scattering values to children
----------------------------------------------------------------

-- | Very important question is how to pass parameters to multiple
--   childs. We have two primary way to divide data.
--
--    1. Divide data on parent processa nd send values to childs.
--
--    2. Let childs to select suitable part of data by themselves
--       using their rank.
--
--   We provide applicative interface for data splitting. It's a bit
--   awkward to use. Ideally we'd want to use idiom bracket but
--   upcoming applicative-do in GHC 7.10 will be useful as well.
data Scatter a
    = Same a
    | Scatter (Int -> [a])
    deriving (Functor)

instance Applicative Scatter where
    pure = same
    Same    f <*> Same a    = Same (f a)
    Same    f <*> Scatter a = Scatter $ (fmap . fmap) f a
    Scatter f <*> Same a    = Scatter $ (fmap . fmap) ($ a) f
    Scatter f <*> Scatter a = Scatter $ \n -> zipWith ($) (f n) (a n)

runScatter :: Int -> Scatter a -> [a]
runScatter n (Same a) = replicate n a
runScatter n (Scatter f)
    | length xs == n = xs
    | otherwise      = error "runScatter: list length doesn't match!"
  where xs = f n


-- | Send same value to all nodes.
same :: a -> Scatter a
same = Same

-- | Scatter value
scatter :: (Int -> a -> [b]) -> a -> Scatter b
scatter f a = Scatter (\n -> f n a)



----------------------------------------------------------------
-- Spawning of actors
----------------------------------------------------------------

-- | Actor which receive messages of type @a@ and produce result of
--   type @b@. It's phantom-typed and could only be constructed by
--   'startProcess' which ensures that types are indeed correct.
newtype Actor a b = Actor (DNA ())
                    deriving (Typeable)

-- | Smart constructor for actors. Here we receive parameters and
--   output channel for an actor
actor :: (Serializable a, Serializable b)
      => (a -> DNA b)
      -> Actor a b
actor action = Actor $ do
    sendCh  <- liftP expect
    Param a <- liftP expect
    b       <- action a
    liftP $ sendChan sendCh b

-- | Start execution of actor. We receive list of nodes which process
--   could use as message.
runActor :: Actor a b -> Process ()
runActor (Actor dna) = do
  nodes <- expect
  runDNA nodes dna

remotable [ 'runActor ]

-- | Spawn actor on remote node and set up monitoring.
spawnActor :: (Typeable a, Typeable b)
           => NodeId -> Closure (Actor a b) -> Process ProcessId
spawnActor nid child = do
    (pid,_) <- spawnSupervised nid
             $ $(mkStaticClosure 'runActor) `closureApply` child
    return pid


-- | Evaluate actor in the same thread
eval :: (Serializable a, Serializable b) => Actor a b
     -> a
     -> DNA b
eval (Actor dna) a = do
    -- FIXME this is rather ugly. We don't need to go though sending
    --       messages to self nor new need to send result though channel
    (chSend,chRecv) <- liftP $ newChan
    me <- liftP getSelfPid
    liftP $ do send me chSend
               send me (Param a)
    dna
    liftP $ receiveChan chRecv

-- | Fork process on local node
forkLocal :: (Serializable a, Serializable b)
          => [NodeId]           -- ^ List of nodes process allowed to use
          -> Actor a b          -- ^ Actor
          -> a                  -- ^ Parameters for an actor
          -> DNA (Promise b)
forkLocal nodes child a = do
    me  <- liftP getSelfPid
    pid <- liftP $ spawnLocal $ link me >> runActor child
    registerPID pid
    (chSend,chRecv) <- liftP $ newChan
    liftP $ do send pid chSend
               send pid nodes
               send pid (Param a)
    return $ Promise chRecv


-- | Fork process on remote node
forkRemote :: (Serializable a, Serializable b)
           => [NodeId]             -- ^ List of nodes process allowed to use
           -> NodeId               -- ^ Node to spawn on
           -> Closure (Actor a b)  -- ^ Actor
           -> a                    -- ^ Parameters for an actor
           -> DNA (Promise b)
forkRemote nodes nid child a = do
    pid <- liftP $ spawnActor nid child
    registerPID pid
    (chSend,chRecv) <- liftP $ newChan
    liftP $ do send pid chSend
               send pid nodes
               send pid (Param a)
    return $ Promise chRecv


-- | Create group of nodes
--
--   FIXME: at the moment child processes are not allowed to spawn
--          code on remote nodes.
forkGroup
    :: (Serializable a, Serializable b)
    => [NodeId]                 -- ^ List of nodes to spawn on
    -> Closure (Actor a b)      -- ^ Actor
    -> Scatter a                -- ^ Parameters to actors.
    -> DNA (Group b)
forkGroup nodes child scat = do
    when (null nodes) $
        error "Empty list of nodes"
    let n  = length nodes
        xs = runScatter n scat
    (chSend,chRecv) <- liftP $ newChan
    forM_ (nodes `zip` xs) $ \(nid,a) -> do
        pid <- liftP $ spawnActor nid child
        liftP $ do send pid chSend
                   send pid (CAD [])       -- FIXME: How to allow children??
                   send pid (Param a)
        registerPID pid
    return $ Group (Reliable n) chRecv


-- | Create group of nodes and allow failout
--
--   FIXME: at the moment child processes are not allowed to spawn
--          code on remote nodes.
forkGroupFailout
    :: (Serializable a, Serializable b)
    => [NodeId]                 -- ^ List of nodes to spawn on
    -> Closure (Actor a b)      -- ^ Actor
    -> Scatter a                -- ^ Parameters to actors.
    -> DNA (Group b)
forkGroupFailout nodes child scat = do
    when (null nodes) $
        error "Empty list of nodes"
    let n  = length nodes
        xs = runScatter n scat
    (chSend,chRecv) <- liftP $ newChan
    forM_ (nodes `zip` xs) $ \(nid,a) -> do
        pid <- liftP $ spawnActor nid child
        liftP $ do send pid chSend
                   send pid (CAD [])       -- FIXME: How to allow children??
                   send pid (Param a)
        registerPID pid
    gid <- freshGID
    return $ Group (FailOut n gid) chRecv
