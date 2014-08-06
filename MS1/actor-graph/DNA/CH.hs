{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
-- | Library functions for the 
module DNA.CH where

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Platform.ManagedProcess
import Control.Distributed.Process.Platform.Time (Delay(..))
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)

import Data.Typeable (Typeable)
import Data.Binary   (Binary)
import qualified Data.Vector.Storable as S
import qualified Data.IntMap as IntMap
import System.Environment (getArgs)

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
              => (Int -> a) -> sh -> Array sh a
generateArray f sh =
  case reifyShape sh of
    ShShape -> case sh of
                 Shape n -> Array sh (S.generate n f)
    ShSlice -> case sh of
                 Slice off n -> Array sh (S.generate n (\i -> f (i + off)))
    


----------------------------------------------------------------
-- CH combinators
----------------------------------------------------------------

newtype Result a = Result a
                   deriving (Eq,Ord,Show,Typeable,Binary)

data RemoteMap = RemoteMap
  { rmapResult :: ProcessId
  , rmapConns  :: IntMap.IntMap ProcessId
  }

sendToI :: Serializable a => RemoteMap -> Int -> a -> Process ()
sendToI (RemoteMap _ m) i a = send (m IntMap.! i) a

sendResult :: Serializable a => RemoteMap -> a -> Process ()
sendResult (RemoteMap p _) a = send p a

handleRule :: (Serializable a) => (s -> a -> (s, Process ())) -> Dispatcher s
handleRule f
  = handleCast
  $ \s a -> case f s a of
              (s',m) -> m >> return (ProcessContinue s')

startActor :: s -> ProcessDefinition s -> Process ()
startActor s = serve () (\_ -> return (InitOk s NoDelay))

producer :: s -> (s -> (s,Process())) -> Process ()
producer s0 step = loop s0
  where
    loop s = let (s',m) = step s in m >> loop s'


defaultMain :: (RemoteTable -> RemoteTable) -> ([NodeId] -> Process ()) -> IO ()
defaultMain remotes master = do
  args <- getArgs
  let rtable = remotes initRemoteTable
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port rtable
      startMaster backend master
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


-- | Set up monitoring of slave processes
monitor :: Process ()
monitor = loop
  where
    loop = receiveWait
             [ match $ \(Result x) -> say $ "RESULT = " ++ show (x :: Double)
             , match $ \(Result x) -> say $ "RESULT = " ++ show (x :: Int)
             ]
