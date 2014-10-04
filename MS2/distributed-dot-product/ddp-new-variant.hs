{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main(main) where

import GHC.Generics (Generic)
import Data.Typeable
import Control.DeepSeq
import Control.Monad
import System.Posix.Files
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process hiding (say)
import Control.Distributed.Process.Closure
--import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Platform (resolve)
import qualified Control.Distributed.Process.Platform.Service.SystemLog as Log
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as Unsafe
import qualified Data.Vector.Storable as S
import Text.Printf
import Control.Distributed.Process.Debug
import qualified Control.Distributed.Process.Platform.Time as Time
import qualified Control.Distributed.Process.Platform.Timer as Timer
import Data.Int
import Data.Binary
import Data.Vector.Binary
import System.IO

import DNA.Channel.File (readDataMMap)



----------------------------------------------------------------
-- Helper data types for 
----------------------------------------------------------------


-- | Message type which carry PID of master process.
newtype Master = Master ProcessId
                 deriving (Show,Eq,Typeable,Binary)

-- | Newtype for encoding order of function parameters.  It's possible
--   to rely on order of sending but it's easy to mix up and not
--   reliable in hypothetical case when we send parameters from
--   different processes.
--
--   Parameters are wrapper into layer of S newtypes.
--
--   > S p1     <- expect    -- parameter 1
--   > S (S p2) <- expect    -- parameter 2
--   > ...
--
--   It's somewhat tiresome to use manually but easy to generate.
newtype S a = S a
              deriving (Show,Eq,Typeable,Binary)


-- | Type synonym to make explicit type signatures shorter
type MakeChan a = Process (SendPort a, ReceivePort a)

-- | Cluster architecture description. Currently it's simply list of
--   nodes process can use.
newtype CAD = CAD [NodeId]
              deriving (Show,Eq,Typeable,Binary)


-- | Split vector into set of slices.
scatterShape :: Int -> Int64 -> [(Int64,Int64)]
scatterShape n size
  = zipWith (,) chunkOffs chunkSizes
  where
    (chunk,rest) = size `divMod` n
    extra        = replicate rest 1 ++ repeat 0
    chunkSizes   = zipWith (+) (replicate n chunk) extra
    chunkOffs    = scanl (+) 0 chunkSizes



----------------------------------------------------------------
-- Distributed dot product
--
-- Note that actors which do not spawn actors on other nodes do not
-- receive CAD.
----------------------------------------------------------------

-- | Compute vector and send it back to master using unsafe send.
ddpComputeVector :: Process ()
ddpComputeVector = do
  result    <- expect :: Process (SendPort (S.Vector Double)) 
  S (off,n) <- expect :: Process (S (Int,Int))
  -- Generate vector.
  --
  -- NOTE: we need strictness here otherwise we will pass unevaluated
  --       thunk to parent process
  let !vec = S.generate n (\i -> fromIntegral (i + off)) :: S.Vector Double
  Unsafe.sendChan result vec


-- | Read vector slice from the data file.
ddpReadVector :: Process ()
ddpReadVector = do
  result        <- expect :: Process (SendPort (S.Vector Double))
  S fname       <- expect :: Process (S FilePath)
  S (S (off,n)) <- expect :: Process (S (S (Int64,Int64)))
  --
  me   <- getSelfPid
  !vec <- liftIO $ readDataMMap n off fname "FIXME"
  Unsafe.sendChan result vec


-- | Caclculate dot product of slice of vector
ddpProductSlice :: Process ()
ddpProductSlice = do
  -- Receive PID of master
  me            <- getSelfPid
  Master parent <- expect
  -- Function parameters
  S fname       <- expect :: Process (S FilePath)
  S (S (off,n)) <- expect :: Process (S (S (Int64,Int64)))
  -- Spawn local workers for generating vectors
  (computeSend,computeRecv) <- newChan :: MakeChan (S.Vector Double)
  computePID <- spawnLocal ddpComputeVector
  send computePID computeSend
  send computePID (S (off,n))
  --
  (fileSend,fileRecv) <- newChan :: MakeChan (S.Vector Double)
  filePID <- spawnLocal ddpReadVector
  send filePID fileSend
  send filePID (S fname)
  send filePID (S (S (off,n)))
  -- This is synchronization point. We need vector from both helper
  -- actors to proceed.
  va <- receiveChan computeRecv
  vb <- receiveChan fileRecv
  let r = S.sum $ S.zipWith (*) va vb
  -- Send partial result to the parent
  send parent r



remotable [ 'ddpComputeVector
          , 'ddpProductSlice
          ]


-- | Actor for calculating dot product 
ddpDotProduct :: [NodeId] -> Process ()
ddpDotProduct cad = do
  -- PID of master
  me            <- getSelfPid
  Master parent <- expect
  -- Get function parameters
  S fname    <- expect :: Process (S FilePath)
  S (S size) <- expect :: Process (S (S Int64)) -- For simplicity let get vector size externally.
  -- Divide work between child processes
  let n      = length cad
      slices = scatterShape n size
  -- Start up N worker processes one for every node in CAD. This
  -- process will act as collector.
  pids <- forM (cad `zip` slices) $ \(nid, slice) -> do
    pid <- spawn nid $ $(mkStaticClosure 'ddpProductSlice)
    send pid (Master me)
    send pid (S fname)
    send pid (S (S slice))
  -- Collect data from workers. Here we assume that nor workers nor
  -- nodes do not crash.
  let loop 0 !acc = return acc
      loop k !acc = do
        x <- expect :: Process Double
        loop (k-1) (acc + x) 
  res <- loop n
  --
  send parent res




main :: IO ()
main = do
  return ()
