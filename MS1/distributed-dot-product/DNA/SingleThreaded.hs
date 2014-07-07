{-# LANGUAGE GADTs #-}
-- | Single-threaded interpreter for the DNA AST.
module DNA.SingleThreaded where

import Control.Applicative
import Data.Vector.Binary ()
import qualified Data.Vector.Storable as S

import DNA.AST
import DNA.Util


interpret :: DNA a -> IO a
interpret (DotProduct a b) = do
  DVector va <- interpret a
  DVector vb <- interpret b
  return $ S.sum $ S.zipWith (*) va vb
interpret (ReadFile fname) = do
  DVector <$> readPackedFile fname
interpret (FileLength fname) = do
  packedFileSize fname
interpret (Generate exprN) = do
  n <- interpret exprN
  return $ DVector $ S.generate n (const 1)
interpret (Literal a) = return a
