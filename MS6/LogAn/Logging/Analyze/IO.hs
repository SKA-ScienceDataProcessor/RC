{-# LANGUAGE CPP #-}

module Logging.Analyze.IO where

import Data.List

import Logging.ProcessLog

data IOMtxs = IOMtxs {
    -- Hints
    imHintReadBytes  :: !Int
  , imHintWriteBytes :: !Int
    -- Proc
  , imReadBytes      :: !Int
  , imWriteBytes     :: !Int
    --
  , imTime           :: !Int
  } deriving Show

zim :: IOMtxs
zim = IOMtxs 0 0 0 0 0

uih :: IOMtxs -> ProfileHint -> IOMtxs
uih mtx (IOHint rbs wbs) =
  mtx {
      imHintReadBytes  = rbs
    , imHintWriteBytes = wbs
  }
uih mtx _ = mtx

uip :: IOMtxs -> (Attr, Attr) -> IOMtxs
uip mtx (Proc pt1 n1, Proc pt2 n2) = check pt1 pt2 $
#define _U(f) f -> mtx{im/**/f = (im/**/f mtx) + n2 - n1}
   case pt1 of
     _U(ReadBytes )
     _U(WriteBytes)
#undef _U
uip _ (p1, p2) = error $ "Internal error in uip: " ++ show p1 ++ '/':show p2

ioAn :: KernInvDescr -> IOMtxs
ioAn kid =
    flip (foldl' uih) (kidHints kid)
  . flip (foldl' uip) (kidAPairs kid)
  $ zim
