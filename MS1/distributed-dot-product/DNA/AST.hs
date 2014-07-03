{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
-- | Syntax tree for DNA program
module DNA.AST where

import Data.Typeable
import qualified Data.Vector.Storable as S



----------------------------------------------------------------
-- 
----------------------------------------------------------------

-- | Representation of the DNA program
--
--
data DNA a where
  DotProduct :: DNA (DVector Double) -> DNA (DVector Double) -> DNA Double
  ReadFile   :: FilePath -> DNA (DVector Double)
  Generate   :: Int      -> DNA (DVector Double)
  deriving Typeable

-- | Distributed vector
data DVector a = DVector (S.Vector a)

