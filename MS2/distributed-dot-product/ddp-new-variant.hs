{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main(main) where

import Control.Applicative
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
import DNA hiding (__remoteTable)
import qualified DNA 


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
ddpComputeVector :: Actor (Int64,Int64) (S.Vector Double)
ddpComputeVector = startProcess $ \_ (off,n) ->
    return $ S.generate (fromIntegral n)
               (\i -> fromIntegral (fromIntegral i + fromIntegral off))

-- | Read vector slice from the data file.
ddpReadVector :: Actor (String,(Int64,Int64)) (S.Vector Double)
ddpReadVector = startProcess $ \_ (fname, (off,n)) -> do
    liftIO $ readDataMMap n off fname "FIXME"


-- | Caclculate dot product of slice of vector
ddpProductSlice :: Actor (String,(Int64,Int64)) Double
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
ddpDotProduct :: Actor (String,Int64) Double
ddpDotProduct = startProcess $ \nodes (fname,size) -> do
    partials <- forkGroup nodes $(mkStaticClosure 'ddpProductSlice)
                  ((,) <$> same fname <*> scatter (\n -> scatterShape (fromIntegral n)) size)
    gather partials (+) 0



main :: IO ()
main = dnaRun __remoteTable undefined
