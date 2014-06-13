{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
module Worker where

import Control.Concurrent (threadDelay)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable (Serializable)

import Text.Printf
import GHC.Generics (Generic)


import Types



----------------------------------------------------------------
-- Dot product
----------------------------------------------------------------

-- | Worker for dot product
dotProductWorker :: (MasterProtocol, BoundedProtocol Double) -> Process ()
dotProductWorker (master,upstream)
  = idle master >> loop
  where
    loop = do
      msg <- expect :: Process (Maybe (Int,Int))
      case msg of
        Nothing    -> return ()
        Just (i,j) -> do me <- getSelfPid
                         logMsg master $ printf "[%s]: Fold received work %s" (show me) (show (i,j))
                         liftIO $ threadDelay (1000*1000)
                         sendBoundedStream upstream (1 :: Double)
                         idle master
                         loop



----------------------------------------------------------------
-- Fold
----------------------------------------------------------------

-- | Worker for summing partial results
foldWorker :: (MasterProtocol,ResultProtocol Double) -> Process ()
foldWorker (master,result)
  = loop Nothing 0 0
  where
    loop :: Maybe Int -> Int -> Double -> Process ()
    loop (Just tot) !n !x
      | n >= tot = sendResult result x
    loop tot n x = do
      msg <- receiveWait [ match $ return . Val   . (\(BoundedV x) -> x)
                         , match $ return . Total . (\(Count x) -> x)
                         ]
      logMsg master $ show ("FOLD",msg,tot,n,x)
      case msg of
        Val   y -> loop tot (n+1) (x+y)
        Total k | n >= k    -> sendResult result x
                | otherwise -> loop (Just k) n x

data FoldTypes
  = Val   Double
  | Total Int
    deriving Show

remotable [ 'dotProductWorker
          , 'foldWorker
          ]
