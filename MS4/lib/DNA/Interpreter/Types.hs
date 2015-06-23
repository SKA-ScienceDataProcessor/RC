{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | Data types for interpretation of DNA DSL using cloud haskell
module DNA.Interpreter.Types where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Exception
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Data.Monoid
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import qualified Data.Foldable as T
import qualified Data.Map as Map
import           Data.Map   (Map)
import qualified Data.Set as Set
import           Data.Set   (Set)
import GHC.Generics  (Generic)
import Lens.Family.TH

import DNA.Types
import DNA.Lens
import DNA.DSL
import DNA.CH
import DNA.Logging


----------------------------------------------------------------
-- Extra functions for matching on messages
----------------------------------------------------------------

-- | Handlers for events which could be used with State monad with state s
data MatchS = forall a. Serializable a => MatchS (a -> Controller ())

-- | Wait for message from Match' and handle auxiliary messages
handleRecieve
    :: [MatchS]
    -> [Match' a]
    -> DnaMonad a
handleRecieve auxMs mA
    = loop
  where
    matches e s
        =  map toMatch ((fmap . fmap) Right mA)
        ++ [ match $ \a -> Left <$> (flip runReaderT e . flip runStateT s . unDnaMonad . runController) (f a)
           | MatchS f <- auxMs]
    loop = do
        e <- ask
        s <- get
        r <- liftP $ receiveWait $ matches e s
        case r of
          Right a      -> return a
          Left ((),s') -> put s' >> loop

-- | Wait for message
blockForMessage :: [MatchS] -> DnaMonad ()
blockForMessage auxMs = do
    e  <- ask
    s  <- get
    s' <- liftP $ receiveWait $ matches e s
    put s'
  where
    matches e s
        = [ match $ \a -> (flip runReaderT e . flip execStateT s . unDnaMonad . runController) (f a)
          | MatchS f <- auxMs
          ]




----------------------------------------------------------------
-- Monads
----------------------------------------------------------------

-- | Type synonym for monad which is used for interpretation of DNA
--   programs.
--
--   It have custom definition of fail which is used to raise panic
--   level error.
newtype DnaMonad a = DnaMonad
    { unDnaMonad :: StateT StateDNA (ReaderT Env Process) a }
    deriving (Functor,Applicative,MonadIO,MonadProcess,MonadState StateDNA,MonadReader Env)

instance Monad DnaMonad where
    return           = DnaMonad . return
    DnaMonad m >>= f = DnaMonad $ m >>= fmap unDnaMonad f
    fail             = doPanic


instance MonadLog DnaMonad where
    logSource    = liftP logSource
    logLoggerOpt = _stLogOpt <$> get


-- | Monad for event handlers. It adds explicit error handling
type Controller = ExceptT DnaError DnaMonad

-- | Error encountered during DNA computation
data DnaError
    = FatalErr String              -- ^ Fatal error
    | PanicErr String              -- ^ Internal error
    | Except SomeException         -- ^ Uncaught exception

data PanicException = PanicException String
             deriving (Show,Typeable)
instance Exception PanicException

data FatalException = FatalException String
             deriving (Show,Typeable)
instance Exception FatalException
      
-- | Fatal error
fatal :: MonadError DnaError m => String -> m a
fatal = throwError . FatalErr

-- | Violation of internal invariant which should not ever happen
--   unless implementation is buggy.
panic :: MonadError DnaError m => String -> m a
panic = throwError . PanicErr

-- | Actually raise panic
doPanic :: MonadLog m => String -> m a
doPanic msg = do
    panicMsg msg
    liftIO $ throw $ PanicException msg

-- | Actually 
doFatal :: MonadLog m => String -> m a
doFatal msg = do
    fatalMsg msg
    liftIO $ throw $ FatalException msg

    
-- | Run controller monad. On failure will terminate process forefully
runController :: Controller a -> DnaMonad a
runController m = do
    r <- runExceptT m
    case r of
      Left err -> case err of
        PanicErr e -> doPanic e
        FatalErr e -> doFatal e
        Except   e -> liftIO $ throw e
      Right a -> return a

-- | Run DNA monad
runDnaMonad
    :: AID
    -> Rank
    -> GroupSize
    -> Closure DnaInterpreter
    -> [NodeInfo]
    -> [DebugFlag]
    -> LoggerOpt
    -> FilePath
    -> DnaMonad a
    -> Process a
runDnaMonad aid (Rank rnk) (GroupSize grp) interp nodes flags logopt workDir =
    flip runReaderT env . flip evalStateT s0 . unDnaMonad
  where
    env = Env { envRank        = rnk
              , envGroupSize   = grp
              , envInterpreter = interp
              , envAID         = aid
              , envWorkDir     = workDir
              }
    s0 = StateDNA
           { _stCounter       = 0
           , _stDebugFlags    = flags
           , _stLogOpt        = logopt
           , _stNodePool      = Set.fromList nodes
           , _stUsedResources = Map.empty
           , _stChildren      = Map.empty
           , _stActorSrc      = Map.empty
           , _stActorDst      = Map.empty
           , _stVars          = Map.empty
           , _stActorRecvAddr = Map.empty
           , _stAid2Pid       = Map.empty
           , _stAllAid2Pid    = Map.empty
           , _stPid2Aid       = Map.empty
           , _stActorClosure  = Map.empty
           }

-- | Run DNA monad using ActorParam as source of input parameters and
--   interpreter
runDnaParam :: ActorParam -> DNA a -> Process a
runDnaParam p action = do
  interpreter <- unClosure $ actorInterpreter p
  runDnaMonad (actorAID p)
              (actorRank p)
              (actorGroupSize p)
              (actorInterpreter p)
              (vcadNodePool $ actorNodes       p)
              (actorDebugFlags  p)
              (actorLogOpt p)
              (actorWorkDir p)
    $ dnaInterpreter interpreter action

-- | Evaluator for DNA monad. We have to pass closure with function
--   for interpreting DNA monad manually since we have to break
--   dependency loop. We also want to wrap function into
newtype DnaInterpreter = DnaInterpreter { dnaInterpreter :: forall a. DNA a -> DnaMonad a }
                       deriving Typeable


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Parameter send to actor on startup
data ActorParam = ActorParam
    { actorParent      :: Maybe ProcessId
      -- ^ Parent of an actor. Nothing for top level actor
    , actorInterpreter :: Closure DnaInterpreter
      -- ^ Interpreter for DNA DSL
    , actorRank        :: Rank
      -- ^ Rank of an actor
    , actorGroupSize   :: GroupSize
      -- ^ Size of group of actors
    , actorNodes       :: VirtualCAD
      -- ^ Nodes allocated to an actor
    , actorDebugFlags  :: [DebugFlag]
      -- ^ Extra flags for debugging
    , actorSendBack    :: SendPort (RecvAddr,[SendPortId])
      -- ^ Send receive address and list of port ID's back to the parent process
    , actorAID         :: AID
      -- ^ AID of an actor as viewed by parent
    , actorLogOpt      :: LoggerOpt
      -- ^ Logging preferences
    , actorWorkDir     :: FilePath
      -- ^ Actor working directory
    }
    deriving (Typeable,Generic)
instance Binary ActorParam

-- | Parameters of an actor which are constant during its execution
data Env = Env
    { envRank        :: !Int
    , envGroupSize   :: !Int
    , envInterpreter :: !(Closure DnaInterpreter)
    , envAID         :: !AID
    , envWorkDir     :: !FilePath
    }

-- | State of interpreter
--
--   Dataflow graph is append-only in the sense that we don't remove
--   actors which completed execution from graph but mark them as
--   completed instead.
--
--   Note that resources are allocated to individual CH processes not
--   to logical CH actors
--
--   Invariants:
--
--    * For every actor ID exist entry stChildren and stActorRecvAddr
--
--
data StateDNA = StateDNA
    { _stCounter     :: !Int
      -- Counter for generation of unique IDs
    , _stDebugFlags  :: [DebugFlag]
    , _stLogOpt      :: LoggerOpt

      -- Resources
    , _stNodePool      :: !(Set NodeInfo)
      -- ^ Unused nodes which could be reused
    , _stUsedResources :: !(Map ProcessId VirtualCAD)
      -- ^ Resources used by child actors. Note that resources are
      --   allocated per CH process not per actor.

      -- Append only dataflow graph
    , _stChildren      :: !(Map AID ActorState)
      -- ^ State of child actors
    , _stActorSrc :: !(Map AID (Either (RecvAddr -> Process ()) AID))
      -- ^ Source of an actor. It's either another actor or parent
      --   actor. In latter case we store encoded message.
      --   message
    , _stActorDst :: !(Map AID (Either VID AID))
      -- ^ Destination of an actor. It could be either other actor or
      --   variable local to parent actor.

      -- State of connections
    , _stVars          :: !(Map VID RecvAddr)
      -- ^ Mapping for local variables
    , _stActorRecvAddr :: !(Map AID (Maybe (RecvAddr,[SendPortId])))
      -- ^ Receive address of an actor. When actor terminated it's set
      --   to Nothing

      -- Mapping ProcessID <-> AID
    , _stPid2Aid    :: !(Map ProcessId (Rank,GroupSize,AID))
    , _stAid2Pid    :: !(Map AID (Set ProcessId))
    , _stAllAid2Pid :: !(Map AID (Set ProcessId))
      -- ^ All PIDs ever associated with given AID

      -- Restarts
    , _stActorClosure   :: !(Map AID SpawnCmd)
      -- ^ Closure for the actor. All restartable actors have closure
      -- stored.
    }

-- | Command for spawning actor
data SpawnCmd
    = SpawnSingle (Closure (Process ())) Res RecvAddrType [SpawnFlag]
    deriving (Show)

-- | State of actor.
data ActorState
    = Failed                    -- ^ Actor failed
    | Running RunInfo           -- ^ Actor is still running
    | Completed Int             -- ^ Actor finished execution successfully
    deriving (Show)

-- | Information about running process. We store number of completed
--   processes and number of allowed failures. Number of still running
--   processes is accessible via AID<->{PID} mapping
data RunInfo = RunInfo
    { nCompleted     :: Int
      -- ^ Number of CH processes that complete execution 
    , allowedFailures :: Int
      -- ^ Number of allowed failures
    }
    deriving (Show)


$(makeLenses ''StateDNA)

-- | Destination of actor.
actorDestinationAddr :: AID -> Controller (Maybe RecvAddr)
actorDestinationAddr aid = do
    maidDst <- use $ stActorDst . at aid
    case maidDst of
      Nothing             -> return Nothing
      Just (Left var)     -> use $ stVars . at var
      Just (Right aidDst) -> do Just m <- use $ stActorRecvAddr . at aidDst
                                -- FIXME: Pattern matching??
                                return $ fmap fst m



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Terminate actor forcefully
terminateActor :: (MonadProcess m, MonadState StateDNA m) => AID -> m ()
terminateActor aid = do
    mpids <- use $ stAid2Pid . at aid
    liftP $ T.forM_ mpids $ T.mapM_ $ \p ->
        send p (Terminate "TERMINATE")

-- | Drop actor from the registry
dropActor :: (MonadState StateDNA m) => AID -> m ()
dropActor aid = do
    mpids <- use $ stAid2Pid . at aid
    stAid2Pid       . at aid .= Nothing
    stActorRecvAddr . at aid .= Just Nothing
    T.forM_ mpids $ T.mapM_ $ \p ->
        stPid2Aid . at p .= Nothing

-- | Put resources associated with PID to the pool
freeResouces :: (MonadState StateDNA m) => ProcessId -> m ()
freeResouces pid = do
    mr <- use $ stUsedResources . at pid
    case mr of
      Nothing -> return ()
      Just (VirtualCAD Local  _ ns) -> stNodePool %= (Set.fromList ns <>)
      Just (VirtualCAD Remote n ns) -> stNodePool %= (\xs -> Set.singleton n <> Set.fromList ns <> xs)

-- | Put resources associated with PID to the pool
freeActorResouces :: (MonadState StateDNA m) => AID -> m ()
freeActorResouces aid = do
    mpids <- use $ stAid2Pid . at aid
    T.forM_ mpids $ T.mapM_ freeResouces


sendToActor :: (MonadState StateDNA m, MonadProcess m, Serializable a) => AID -> a -> m ()
sendToActor aid a = do
    pids <- use $ stAid2Pid . at aid
    liftP $ T.forM_ pids $ T.mapM_ (\p -> send p a)

-- Generate unique ID
uniqID :: MonadState StateDNA m => m Int
uniqID = do
    i <- use stCounter
    stCounter .= (i + 1)
    return i
