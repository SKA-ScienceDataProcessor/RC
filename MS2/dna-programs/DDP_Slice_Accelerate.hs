{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module DDP_Slice_Accelerate where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO as A
import qualified Data.Array.Accelerate.CUDA as CUDA

import qualified Data.Vector.Storable as S
import System.Directory ( removeFile )
import DNA

import DDP

import System.Environment ( withArgs )

-- | Accelerate worker function
ddpWorker :: A.Acc (A.Vector Double) -> A.Acc (A.Vector Double)
          -> A.Acc (A.Scalar Double)
ddpWorker as bs = A.fold (+) 0 (A.zipWith (*) as bs)

-- | Wrapper for accelerate function above
calculateDotP :: S.Vector Double -> S.Vector Double -> IO Double
calculateDotP va vb
    | S.length vb /= n  = error "Vector sizes do not match"
    | otherwise
    = -- Uncomment for accelerate-cuda debugging (with -fdebug)
      -- withArgs ["-fflush-cache", "-ddump-cc"] $
      do -- Convert to accelerate arrays
         let sh = A.Z A.:. n
             va' = A.fromVectors sh ((), va) :: A.Vector Double
             vb' = A.fromVectors sh ((), vb) :: A.Vector Double

         -- Run!
         let res = CUDA.run (ddpWorker (A.use va') (A.use vb'))
         return $! head $ A.toList res
  where n = S.length va

-- | Calculate dot product of slice of vector.
--
--  * Input:  slice of vectors which we want to use
--  * Output: dot product of slice
ddpProductSlice :: Actor Slice Double
ddpProductSlice = actor $ \(fullSlice) -> duration "vector slice" $ do
    -- Calculate offsets
    nProc <- groupSize
    rnk   <- rank
    -- FIXME: Bad!
    let slice@(Slice _ n) = scatterSlice (fromIntegral nProc) fullSlice !! rnk
    -- First we need to generate files on tmpfs
    fname <- duration "generate" $ eval ddpGenerateVector n
    -- Start local processes
    resVA <- select Local (N 0)
    resVB <- select Local (N 0)
    shellVA <- startActor resVA $(mkStaticClosure 'ddpComputeVector)
    shellVB <- startActor resVB $(mkStaticClosure 'ddpReadVector   )
    -- Connect actors
    sendParam slice              shellVA
    sendParam (fname, Slice 0 n) shellVB
    --
    futVA <- delay Local shellVA
    futVB <- delay Local shellVB
    --
    va <- duration "receive compute" $ await futVA
    vb <- duration "receive read"    $ await futVB
    -- Clean up
    liftIO $ removeFile fname
    duration "compute sum" $ liftIO $ calculateDotP va vb


remotable [ 'ddpProductSlice
          ]
