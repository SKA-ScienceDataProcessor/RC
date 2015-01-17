{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module DDP_Slice_CUDA where

import qualified Data.Vector.Storable as S
import System.Directory ( removeFile )
import Foreign.Ptr     (Ptr)
import Foreign.C.Types (CInt(..))
import DNA

import DDP


-- Here we're wrapping C function 
foreign import ccall safe "calculate_dot_p"
  c_calculate_dot_p :: Ptr Double -> Ptr Double -> CInt -> IO Double

-- | Wrapper function which calls C functions @calculate_dot_p@
calculateDotP :: S.Vector Double -> S.Vector Double -> IO Double
calculateDotP va vb
    | S.length vb /= n = error "Vector sizes do not match"
    | otherwise        = do
        S.unsafeWith va $ \pa ->
            S.unsafeWith vb $ \pb ->
                c_calculate_dot_p pa pb (fromIntegral n)
  where
    n = S.length va



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
