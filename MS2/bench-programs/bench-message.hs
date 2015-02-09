{-# LANGUAGE TemplateHaskell, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Main(main) where

import Control.Monad

import Data.Bits (shiftL)
import qualified Data.ByteString as B
import Data.Time.Clock
import Data.Ratio
import Data.Maybe (mapMaybe)
import Data.Binary (Binary)

import System.Posix.Process
import System.Posix.Types

import DNA

deriving instance Binary CPid -- ProcessID

----------------------------------------------------------------
-- Distributed dot product
----------------------------------------------------------------

-- | Actor receiving data
ddpReceive :: Actor (ProcessID, B.ByteString) String
ddpReceive = actor $ \(procId, msg) -> msg `seq` do
  endTime <- liftIO getCurrentTime
  procId' <- liftIO getProcessID
  return $ show $
    if procId == procId'
    then Nothing
    else Just endTime

remotable [ 'ddpReceive ]

-- | Actor sending data
ddpSend :: Actor Int Rational
ddpSend = actor $ \size -> do
    -- Allocate child actor
    res   <- selectMany (Frac 1) (NNodes 1) []
    shell <- startGroup res Failout $(mkStaticClosure 'ddpReceive)
    -- Take time, then send parameter and wait for result
    procId <- liftIO getProcessID
    startTime <- liftIO $ getCurrentTime
    sendParam (procId, B.take size vector) (broadcast shell)
    -- Collect results
    partials <- delayGroup shell
    endTimes <- gather partials (flip (:)) []
    let diffs = map (toRational . (`diffUTCTime` startTime)) (mapMaybe read endTimes)
    liftIO $ putStrLn $ show (length endTimes) ++ " answers"
    return $ toRational size / sum diffs / fromIntegral (length diffs)

-- | Data to send
vectorSize :: Int
vectorSize = 1 * 1024 * 1024
vector :: B.ByteString
vector = B.replicate vectorSize 0

main :: IO ()
main =  dnaRun __remoteTable $ do

    let sizes = takeWhile (<= vectorSize) $ map (shiftL 1) [12..]
    forM_ sizes $ \fsize -> do

      -- Would love to use criterion, but can't do that easily with a
      -- nested monad...
      time <- eval ddpSend fsize
      let ratioToReal :: Rational -> Double
          ratioToReal x = fromIntegral (numerator x) /
                          fromIntegral (denominator x)
      liftIO $ putStrLn $ concat [ show (fsize `div` 1024), " kB: "
                                 , show (ratioToReal time / 1024 / 1024), " MB/s" ]

    liftIO $ putStrLn " done!"
