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

import GHC.Generics (Generic)


import Types



dotProductWorker :: (ProcessId,ProcessId) -> Process ()
dotProductWorker (pidMaster,pidUpstream) = do
  myPid <- getSelfPid
  send pidMaster (Idle myPid)
  loop
  where
    loop = do
      msg <- expect :: Process (Maybe (Int,Int))
      case msg of
        Nothing    -> return ()
        Just (i,j) -> do liftIO $ threadDelay (1000*1000)
                         send pidUpstream (1 :: Double)
                         myPid <- getSelfPid
                         send pidMaster (Idle myPid)
                         loop

foldWorker :: (SendPort String,ProcessId) -> Process ()
foldWorker (logCh,pid)
  = loop Nothing 0 0
  where
    loop :: Maybe Int -> Int -> Double -> Process ()
    loop (Just tot) !n !x
      | n >= tot = send pid (Result x)
    loop tot n x = do
      msg <- receiveWait [ match $ return . Val
                         , match $ return . Total . (\(Count x) -> x)
                         ]
      sendChan logCh $ show (msg,tot,n,x)
      case msg of
        Val   y -> loop tot (n+1) (x+y)
        Total k | n >= k    -> send pid (Result x)
                | otherwise -> loop (Just k) n x

data FoldTypes
  = Val   Double
  | Total Int
    deriving Show

remotable [ 'dotProductWorker
          , 'foldWorker
          ]
