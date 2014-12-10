{-# LANGUAGE TemplateHaskell #-}
module DDP where

import Control.Applicative
import Control.Monad
import Data.Int
import qualified Data.Vector.Storable as S

import DNA.Channel.File (readDataMMap)
import DNA

----------------------------------------------------------------
-- Workers for distributed dot product
----------------------------------------------------------------

-- | Compute vector and send it back to master using unsafe send.
ddpComputeVector :: Actor (Int64,Int64) (S.Vector Double)
ddpComputeVector = actor $ \(off,n) -> do
    return $ S.generate (fromIntegral n)
               (\i -> fromIntegral (fromIntegral i + off))

-- | Read vector slice from the data file.
ddpReadVector :: Actor (String,(Int64,Int64)) (S.Vector Double)
ddpReadVector = actor $ \(fname, (off,n)) -> do
    liftIO $ readDataMMap n off fname "FIXME"

remotable [ 'ddpComputeVector
          , 'ddpReadVector
          ]
