{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main(main) where

import Control.Monad
import Control.Distributed.Process hiding (say)
import Control.Distributed.Process.Closure
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as Unsafe
import Control.Distributed.Process.Serializable (Serializable)

import Data.Binary   (Binary)
import Data.Int
import Data.Typeable (Typeable)
import qualified Data.Vector.Storable as S
import GHC.Generics  (Generic)

import DNA.Channel.File (readDataMMap)
import DNA.Logging
import DNA.Run


----------------------------------------------------------------
-- Helper data types for 
----------------------------------------------------------------


-- | Message type which carry PID of master process.
newtype Master = Master ProcessId
                 deriving (Show,Eq,Typeable,Binary)

newtype Param a = Param a
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


data Promise a = Promise (ReceivePort a)

-- | Obtain value from promise. This function should be called only once per promise
promise :: Promise a -> Process a
promise (Promise ch) = do
    -- Here we wait for either value from channel or for message that
    -- child died.
    receiveWait
        [ matchChan ch return
        , match $ \(ProcessMonitorNotification _ _ reason) ->
            case reason of
              DiedNormal -> promise (Promise ch)
              _          -> terminate
        ]


-- | Cluster architecture description. Currently it's simply list of
--   nodes process can use.
newtype CAD = CAD [NodeId]
              deriving (Show,Eq,Typeable,Binary)


-- | Split vector into set of slices.
scatterShape :: Int64 -> Int64 -> [(Int64,Int64)]
scatterShape n size
  = zipWith (,) chunkOffs chunkSizes
  where
    (chunk,rest) = size `divMod` n
    extra        = replicate (fromIntegral rest) 1 ++ repeat 0
    chunkSizes   = zipWith (+) (replicate (fromIntegral n) chunk) extra
    chunkOffs    = scanl (+) 0 chunkSizes


-- | Start process
startProcess :: (Serializable a, Serializable b)
             => ([NodeId] -> a -> Process b)
             -> Process ()   
startProcess action = do
    sendCh  <- expect
    nodes   <- expect
    Param a <- expect
    b       <- action nodes a
    sendChan sendCh b

-- | Fork process on local node
forkLocal :: (Serializable a, Serializable b)
          => [NodeId]           -- ^ List of nodes process allowed to use
          -> Process ()         -- ^ Process command
          -> a                  -- ^ Parameters to process
          -> Process (Promise b)
forkLocal nodes child a = do
    undefined


-- | Fork process on remote node
forkRemote :: (Serializable a, Serializable b)
           => [NodeId]             -- ^ List of nodes process allowed to use
           -> NodeId               -- ^ Node to spawn on
           -> Closure (Process ()) -- ^ Sub process command
           -> a                    -- ^ Parameters sent to process
           -> Process (Promise b)
forkRemote nodes nid child a = do
    (pid,_) <- spawnSupervised nid child
    (chSend,chRecv) <- newChan
    send pid chSend
    send pid (CAD nodes)
    send pid (Param a)
    return $ Promise chRecv



----------------------------------------------------------------
-- Distributed dot product
--
-- Note that actors which do not spawn actors on other nodes do not
-- receive CAD.
----------------------------------------------------------------

-- | Compute vector and send it back to master using unsafe send.
ddpComputeVector :: Process ()
ddpComputeVector = startProcess $ \_ (off,n) ->
    return $ (S.generate n (\i -> fromIntegral (i + off)) :: S.Vector Double)

-- | Read vector slice from the data file.
ddpReadVector :: Process ()
ddpReadVector = startProcess $ \_ (fname, (off,n)) -> do
    liftIO $ readDataMMap n off fname "FIXME"


-- | Caclculate dot product of slice of vector
ddpProductSlice :: Process ()
ddpProductSlice = startProcess $ \_ (fname, slice) -> do
    futVA <- forkLocal [] ddpComputeVector slice
    futVB <- forkLocal [] ddpReadVector (fname :: String, slice :: (Int64,Int64))
    va <- promise futVA
    vb <- promise futVB
    return $ (S.sum $ S.zipWith (*) va vb :: Double)


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
      slices = scatterShape (fromIntegral n) size
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
  res <- loop n 0
  --
  send parent res



main :: IO ()
main = dnaRun __remoteTable undefined
