{-# LANGUAGE GADTs #-}
-- | Single-threaded interpreter for the DNA AST.
module DNA.SingleThreaded where

import Data.Binary
import Data.Vector.Binary ()
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as S

import DNA.AST


interpret :: DNA a -> IO a
interpret (DotProduct a b) = do
  DVector va <- interpret a
  DVector vb <- interpret b
  return $ S.sum $ S.zipWith (*) va vb
interpret (ReadFile fname) = do
  bs <- BS.readFile fname
  return $ DVector $ decode bs
interpret (Generate n) = do
  return $ DVector $ S.generate n (const 1)

