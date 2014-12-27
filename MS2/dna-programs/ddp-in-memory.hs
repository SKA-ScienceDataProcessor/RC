{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Main(main) where

import Data.Int
import qualified Data.Vector.Storable as S

import System.IO        ( openTempFile, hClose )
import System.Directory ( removeFile )

import DNA

import DDP
import DDP_Slice


----------------------------------------------------------------
-- Distributed dot product
--
-- Note that actors which do not spawn actors on other nodes do not
-- receive CAD.
----------------------------------------------------------------

-- | Actor for calculating dot product
ddpDotProduct :: Actor (String,Slice) Double
ddpDotProduct = actor $ \(fname, vec@(Slice 0 size)) -> do
    -- Run generator & delay
    resG   <- select Local (N 0)
    shellG <- startActor resG $(mkStaticClosure 'ddpGenerateVector)
    sendParam (fname, size) shellG
    _ <- duration "generate" . await =<< delay Remote shellG

    -- Chunk & send out
    res   <- selectMany (Frac 1) (NNodes 1) [UseLocal]
    shell <- startGroup res $(mkStaticClosure 'ddpProductSlice)
    broadcastParam (fname,vec) shell
    partials <- delayGroup shell
    x <- duration "collecting vectors" $ gather partials (+) 0
    return x

main :: IO ()
main = do
    (fname, h) <- openTempFile "." "temp.dat"
    dnaRun rtable $ do
        b <- eval ddpDotProduct (fname,Slice 0 20000000)
        liftIO $ putStrLn $ "RESULT: " ++ show b
    hClose h
    removeFile fname
  where
    rtable = DDP.__remoteTable
           . DDP_Slice.__remoteTable
