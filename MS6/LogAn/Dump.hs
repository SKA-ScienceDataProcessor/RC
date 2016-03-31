{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified Data.DList as DL
import System.Environment

import GHC.RTS.Events

import Logging.ProcessLog
import Driver

showBlock :: UsrMsgBlock -> DL.DList String
showBlock ub =
  let
    beg = ">>>>>>> Block started at " ++ show (umbBTime ub) ++ " on " ++ show (umbCap ub) ++ " r:" ++ show (umbRemote ub)
    body = map show $ umbMsgs ub
    end = "<<<<<<< Block ended at " ++ show (umbETime ub)
  in (beg `DL.cons` DL.fromList body) `DL.snoc` end

showKID :: KIDBlock -> DL.DList String
showKID kidb =
  let
    beg = ">>>>>>> Block started at " ++ show (kidbBTime kidb) ++ " on " ++ show (kidbCap kidb) ++ " r:" ++ show (kidbRemote kidb)
    body = map show (kidbMsgs kidb)
    end = "<<<<<<< Block ended at " ++ show (kidbETime kidb)
  in (beg `DL.cons` DL.fromList body) `DL.snoc` end

showAPairs :: KIDBlock -> [String]
showAPairs = map show . concatMap kidAPairs . filter (not . null . kidAPairs) . kidbMsgs

showLog, showKIDs, showAPairss :: EventLog -> [String]
showLog = DL.toList . DL.concat . map showBlock . preProcessEventLog
showKIDs = DL.toList . DL.concat . map showKID . processEventLog
showAPairss = concatMap showAPairs . processEventLog

dumpElog :: Int -> EventLog -> IO [String]
dumpElog op el =
  if | op == 0 -> return $! showLog el
     | op == 1 -> return $! showKIDs el
     | otherwise -> return $! showAPairss el

main :: IO ()
main = do
  op <- fmap (read . head) getArgs
  drive (dumpElog op)
