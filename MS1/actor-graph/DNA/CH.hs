{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Library functions for the
module DNA.CH (
    -- * Array functions
    zipArray
  , foldArray
  , generateArray
  , scatterShape
    -- * CH utils
  , Result(..)
  , RemoteMap(..)
  , sendToI
  , sendResult
  , handleRule
  , producer
  , startActor
  , defaultMain
  , monitorActors
  , scatterGather
  , worker
  , spawnActor
    -- * Reexports
  , Shape(..)
  , Slice(..)
  ) where

import Control.Monad
import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Platform.ManagedProcess
import Control.Distributed.Process.Platform.Time (Delay(..))
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)

import Data.Typeable (Typeable)
import Data.Binary   (Binary)
import qualified Data.Vector.Storable as S
import qualified Data.IntMap   as IntMap
import qualified Data.Sequence as Seq
import           Data.Sequence   (ViewL(..),(|>))
import System.Environment (getArgs)
import Text.Printf

import GHC.Generics (Generic)

import DNA.AST



----------------------------------------------------------------
-- Array manipulations
----------------------------------------------------------------

zipArray :: (S.Storable a, S.Storable b, S.Storable c, Eq sh)
         => (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipArray f (Array shA va) (Array shB vb)
  | shA /= shB = error "Bad vector shape"
  | otherwise  = Array shA (S.zipWith f va vb)

foldArray :: (S.Storable a)
          => (a -> a -> a) -> a -> (Array sh a) -> a
foldArray f x0 (Array _ v) = S.foldl' f x0 v

generateArray :: (IsShape sh, S.Storable a)
              => sh -> (Int -> a) -> Array sh a
generateArray sh f =
  case reifyShape sh of
    ShShape -> case sh of
                 Shape n -> Array sh (S.generate n f)
    ShSlice -> case sh of
                 Slice off n -> Array sh (S.generate n (\i -> f (i + off)))
    
scatterShape :: Int -> Shape -> [Slice]
scatterShape n (Shape size)
  = zipWith Slice chunkOffs chunkSizes
  where
    (chunk,rest) = size `divMod` n
    extra        = replicate rest 1 ++ repeat 0
    chunkSizes   = zipWith (+) (replicate n chunk) extra
    chunkOffs    = scanl (+) 0 chunkSizes
  
  

----------------------------------------------------------------
-- CH combinators
----------------------------------------------------------------

newtype Result a = Result a
                   deriving (Eq,Ord,Show,Typeable,Binary)

data RemoteMap = RemoteMap
  { rmapResult :: ProcessId
  , rmapConns  :: IntMap.IntMap ProcessId
  }
  deriving (Typeable,Show,Generic)
instance Binary RemoteMap

-- | Send value to remote process
sendToI :: Serializable a => RemoteMap -> Int -> a -> Process ()
sendToI (RemoteMap _ m) i a = cast (m IntMap.! i) a

-- | Send result of computation to master process
sendResult :: Serializable a => RemoteMap -> a -> Process ()
sendResult (RemoteMap p _) a = send p (Result a)

-- | Handle for single transition rule for state machine
handleRule :: (Serializable a) => (s -> a -> (s, Process ())) -> Dispatcher s
handleRule f
  = handleCast
  $ \s a -> case f s a of
              (s',m) -> m >> return (ProcessContinue s')

-- | Helper for starting state machine actor
startActor :: s -> ProcessDefinition s -> Process ()
startActor s = serve () (\_ -> return (InitOk s NoDelay))

-- | Helper for starting data producer actor
producer :: s -> (s -> (s,Process())) -> Process ()
producer s0 step = loop s0
  where
    loop s = let (s',m) = step s in m >> loop s'

-- | Default main
defaultMain :: (RemoteTable -> RemoteTable) -> ([NodeId] -> Process ()) -> IO ()
defaultMain remotes master = do
  args <- getArgs
  let rtable = remotes initRemoteTable
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port rtable
      startMaster backend $ \nids -> do me <- getSelfPid
                                        master (processNodeId me : nids)
    ["slave", host, port] -> do
      backend <- initializeBackend host port rtable
      startSlave backend
    _ -> do putStrLn $ unlines
              [ "usage: '"++executableName++" (master|slave) host port"
              , "   or: '"++executableName++" write-tests"
              , ""
              , "'"++executableName++" write-tests' will write file 'start-ddp' into current directory."
              , "make it executable and run to test the program."
              ]
  where
    executableName = "EXE"

spawnActor :: NodeId -> Closure (Process ()) -> Process ProcessId
spawnActor nid clos = do
  (pid,_) <- spawnSupervised nid clos
  return pid

-- | Set up monitoring of slave processes
monitorActors :: Process ()
monitorActors = loop
  where
    loop = receiveWait
             [ match $ \(Result x) -> say $ "RESULT = " ++ show (x :: Double)
             , match $ \(Result x) -> say $ "RESULT = " ++ show (x :: Int)
             , match $ \(ProcessMonitorNotification _ pid reason) ->
                say $ printf "Process %s down: %s" (show pid) (show reason)
             ]



----------------------------------------------------------------
-- Scatter-gather
----------------------------------------------------------------

-- | Process for doing scatter\/gather.
scatterGather
  :: (Serializable a, Serializable b, Serializable c)
  => (c, c -> c -> c, c -> Process ())
     -- ^ Functions for doing gather
  -> (Int -> a -> [b])
     -- ^ Function for doing scatter
  -> Process ()
scatterGather (x0,merge,sendRes) scatter = do
  -- Get necessary parameters
  workerNodes <- expect :: Process [NodeId]
  workerProc  <- expect
  let nWorker = length workerNodes
  workers <- forM workerNodes $ \nid -> spawn nid workerProc
  forM_ workers $ \pid -> send pid =<< getSelfPid
  -- Handler for gather message
  let handleGather (SGState _ NoAcc) _ = error "Internal error!"
      handleGather (SGState queue (Await acc n)) (Gather c) = do
        let acc' = merge acc c
        case n of
          1 -> do sendRes acc'
                  case Seq.viewl queue of
                    EmptyL  -> return $ ProcessContinue $ SGState queue NoAcc
                    a :< as -> do let bs = scatter nWorker a
                                  forM_ (zip bs workers) $ \(b,pid) -> cast pid (Scatter b)
                                  return $ ProcessContinue $ SGState as (Await x0 nWorker)
          _ -> return $ ProcessContinue $ SGState queue (Await acc' (n-1))
  -- Handler for scatter messages
  let handleScatter (SGState queue NoAcc) a = do
        let bs = scatter nWorker a
        forM_ (zip bs workers) $ \(b,pid) -> cast pid (Scatter b)
        return $ ProcessContinue $ SGState queue (Await x0 nWorker)
      handleScatter (SGState queue acc) a = do
        return $ ProcessContinue $ SGState (queue |> a) acc
  -- Definition of server
  startActor (SGState Seq.empty NoAcc) $ defaultProcess
    { apiHandlers =
         [ handleCast handleGather
         , handleCast handleScatter
         ]
    }

-- | Function for worker
worker :: (Serializable a, Serializable b) => (a -> b) -> Process ()
worker f = do
  master <- expect :: Process ProcessId
  startActor () $ defaultProcess
    { apiHandlers = [ handleCast $ \() (Scatter a) -> do
                         cast master (Gather $ f a)
                         return $ ProcessContinue ()
                    ]
    }


newtype Scatter a = Scatter a
                   deriving (Eq,Ord,Show,Typeable,Binary)
newtype Gather a = Gather a
                   deriving (Eq,Ord,Show,Typeable,Binary)

-- | Scatter-gather state
data SGState a c = SGState (Seq.Seq a) (GatherAcc c)

-- | Accumulator for gather
data GatherAcc a
  = NoAcc                       -- ^ Accumulator is empty
  | Await !a !Int               -- ^ Accumulator holding value and we expect n more answers
  
