{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- | Very simple scheduler and node monitor
module Main where


import System.Environment (getArgs)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Platform.ManagedProcess
import Control.Distributed.Process.Platform.Async

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Typeable
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Vector.Storable as S

import Text.Printf
import GHC.Generics (Generic)



----------------------------------------------------------------
-- 
----------------------------------------------------------------

-- | Representation of the DNA program
--
--
data DNA a where
  DotProduct :: DNA (DVector Double) -> DNA (DVector Double) -> DNA Double
  ReadFile   :: FilePath -> DNA (DVector Double)
  Generate   :: DNA (DVector Double)
  deriving Typeable


-- | Distributed vector
data DVector a = DVector (S.Vector a)


{-
instance Binary a => Binary (DNA a) where
  get = do
    tag <- getWord8
    case tag of
  put (DotProduct a b) = undefined
-}


----------------------------------------------------------------
-- Interpretation of DNA program
----------------------------------------------------------------


controller :: ProcessDefinition ()
controller = defaultProcess
  { apiHandlers = []
  }


----------------------------------------------------------------
-- Actors
----------------------------------------------------------------

-- | Single threaded interpretation of a program
interpret :: DNA a -> Process a
interpret (DotProduct a b) = do
  -- We create 
  asyncA <- asyncLinkedSTM $ task $ interpret a
  asyncB <- asyncLinkedSTM $ task $ interpret b
  ra <- wait asyncA
  rb <- wait asyncB
  case (ra,rb) of
    (AsyncDone (DVector va), AsyncDone (DVector vb)) -> return $ S.sum $ S.zipWith (*) va vb
interpret (ReadFile fname) = undefined
interpret (Generate) = undefined
