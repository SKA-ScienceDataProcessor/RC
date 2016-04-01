{-# LANGUAGE BangPatterns #-}

module Main where

import GHC.RTS.Events

import Logging.ProcessLog
import Logging.Analyze.CPU
import Driver

anCpuGridders :: String -> KIDBlock -> [CPUMtxs]
anCpuGridders kname kidb = map cpuAn $ filter (\kid -> drop 1 (kidName kid) == kname) (kidbMsgs kidb)

dumpAn ::  EventLog -> IO [String]
dumpAn el =
  let
    mplist = processEventLog el
    gridMtxs = concatMap (anCpuGridders "ridKernelCPU") mplist
    degridMtxs = concatMap (anCpuGridders "egridKernelCPU") mplist
    !gridDops = map dlops gridMtxs
    !degridDops = map dlops degridMtxs
    ns :: Double
    ns = 1000000000
    dlops !m = let
                  invt = ns / fromIntegral (cmTime m)
                  hdlops = fromIntegral (cmHintDoubleOps m) * invt
                  rdlops = fromIntegral (cmScalarDoubleOps m + cmSseDoubleOps m * 2 + cmAvxDoubleOps m * 4) * invt
               in (hdlops, rdlops)
    (Event _ (EventBlock _ _ evts)) = head $ drop 1 $ events $ dat el
  in return $! (show . fst . suckDescr $ evts):"Gridder FLOPS: ":(map show gridDops) ++ "DeGridder FLOPS: ":(map show degridDops)
  -- in return $! (show . fst . suckDescr $ evts):"Gridder FLOPS: ":(map show gridMtxs) ++ "DeGridder FLOPS: ":(map show degridMtxs)

main :: IO ()
main = drive dumpAn
