{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns    #-}
module DDP_Slice where

import Data.Int
import qualified Data.Vector.Storable as S

import DNA

import DDP


-- | Caclculate dot product of slice of vector
ddpProductSlice :: Actor (String,Slice) Double
ddpProductSlice = actor $ \(fname, slice) -> duration "vector slice" $ do
    -- Calculate offsets
    nProc <- groupSize
    rnk   <- rank
    -- FIXME: Bad!
    let Slice off n = scatterSlice (fromIntegral nProc) slice !! rnk
    -- Start local processes
    resVA <- select Local (N 0)
    resVB <- select Local (N 0)
    shellVA <- startActor resVA $(mkStaticClosure 'ddpComputeVector)
    shellVB <- startActor resVB $(mkStaticClosure 'ddpReadVector   )
    -- Connect actors
    sendParam (off,n)          shellVA 
    sendParam (fname, (off,n)) shellVB
    --
    futVA <- delay Local shellVA
    futVB <- delay Local shellVB
    --
    va <- duration "receive compute" $ await futVA
    vb <- duration "receive read"    $ await futVB
    --
    duration "compute sum" $
      return $ (S.sum $ S.zipWith (*) va vb :: Double)

remotable [ 'ddpProductSlice
          ]
