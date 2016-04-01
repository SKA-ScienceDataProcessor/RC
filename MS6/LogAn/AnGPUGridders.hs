{-# LANGUAGE BangPatterns #-}

module Main where

import GHC.RTS.Events

import Logging.ProcessLog
import Logging.Analyze.Cuda
import Driver

anGpuGridders :: String -> KIDBlock -> [GpuMtxs]
anGpuGridders kname kidb = map cudaAn $ filter (\kid -> drop 1 (kidName kid) == kname) (kidbMsgs kidb)

dumpAn ::  EventLog -> IO [String]
dumpAn el =
  let
    mplist = processEventLog el
    gridMtxs = concatMap (anGpuGridders "ridKernelGPU") mplist
    degridMtxs = concatMap (anGpuGridders "egridKernelGPU") mplist
    !gridDops = map dlops gridMtxs
    !degridDops = map dlops degridMtxs
    ns :: Double
    ns = 1000000000
    dlops !m = fromIntegral (gmHintCudaDoubleOps m) * ns / fromIntegral (gmKernelTime m)
    -- (Event _ (EventBlock _ _ evts)) = head $ drop 1 $ events $ dat el
  in return $! {- (show . fst . suckDescr $ evts): -} "Gridder FLOPS: ":(map show gridDops) ++ "DeGridder FLOPS: ":(map show degridDops)


main :: IO ()
main = drive dumpAn
