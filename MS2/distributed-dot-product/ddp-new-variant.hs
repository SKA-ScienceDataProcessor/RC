{-# LANGUAGE ScopedTypeVariables #-}

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
import DNA



-- | Split vector into set of slices.
scatterShape :: Int64 -> Int64 -> [(Int64,Int64)]
scatterShape n size
  = zipWith (,) chunkOffs chunkSizes
  where
    (chunk,rest) = size `divMod` n
    extra        = replicate (fromIntegral rest) 1 ++ repeat 0
    chunkSizes   = zipWith (+) (replicate (fromIntegral n) chunk) extra
    chunkOffs    = scanl (+) 0 chunkSizes



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
    va <- await futVA
    vb <- await futVB
    return $ (S.sum $ S.zipWith (*) va vb :: Double)


remotable [ 'ddpComputeVector
          , 'ddpProductSlice
          ]


-- | Actor for calculating dot product 
ddpDotProduct :: Process ()
ddpDotProduct = startProcess $ \nodes (fname::String,size::Int64) -> do
    return ()
  -- -- PID of master
  -- me            <- getSelfPid
  -- Master parent <- expect
  -- -- Get function parameters
  -- S fname    <- expect :: Process (S FilePath)
  -- S (S size) <- expect :: Process (S (S Int64)) -- For simplicity let get vector size externally.
  -- -- Divide work between child processes
  -- let n      = length cad
  --     slices = scatterShape (fromIntegral n) size
  -- -- Start up N worker processes one for every node in CAD. This
  -- -- process will act as collector.
  -- pids <- forM (cad `zip` slices) $ \(nid, slice) -> do
  --   pid <- spawn nid $ $(mkStaticClosure 'ddpProductSlice)
  --   send pid (Master me)
  --   send pid (S fname)
  --   send pid (S (S slice))
  -- -- Collect data from workers. Here we assume that nor workers nor
  -- -- nodes do not crash.
  -- let loop 0 !acc = return acc
  --     loop k !acc = do
  --       x <- expect :: Process Double
  --       loop (k-1) (acc + x) 
  -- res <- loop n 0
  -- --
  -- send parent res



main :: IO ()
main = dnaRun __remoteTable undefined
