{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
-- | Description of distributed program
module Chan where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Except
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Serializable (Serializable)

import qualified Data.Foldable as T
import Data.Binary
import Data.Typeable
import Data.Time.Clock
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import           Data.Map (Map)
import Text.Printf
import GHC.Generics (Generic)
import Prelude hiding (id,(.))

import Types
import Worker


----------------------------------------------------------------
-- Chan
----------------------------------------------------------------

data Waiting
data Running


data ActorFold tag a b where
  ActorFoldWaiting
    :: (ResultProtocol b -> Closure (Process ()))
    -> ActorFold Waiting a b
  ActorFoldRunning
    :: ProcessId                -- PID of running fold process
    -> BoundedProtocol a
    -> ActorFold Running a b

data ActorDotProduct tag a where
  ActorDotProductWaiting
    :: (Int,Int)                -- Full range of indices
    -> ActorDotProduct Waiting a
  ActorDotProductRunning
    :: Set ProcessId            -- Set of running processes
    -> BoundedProtocol a
    -> !Int                     -- Number of completed jobs
    -> [(Int,Int)]              -- Remaining work
    -> ActorDotProduct Running a

-- | Description of distributed program
data Chan tag a b where
  -- | Execution completed
  Noop :: Chan Running a b
  -- | Identity
  Id   :: Chan tag a a
  -- | Category composition
  Compose :: Chan tag a x -> Chan tag x b -> Chan tag a b
  -- | Sum bounded number of values
  FoldSum :: ActorFold tag a b
          -> Chan tag (BoundedV a) (Result b)
  -- | Primitive for calculation of dot product
  DotProduct :: ActorDotProduct tag a
             -> Chan tag X (BoundedV a)

instance Category (Chan tag) where
  id = Id
  Id   . a    = a
  a    . Id   = a
  Noop . Noop = Noop
  a    . b    = Compose b a



----------------------------------------------------------------
-- Starting of processes
----------------------------------------------------------------

-- | Start execution of program
startChan :: MasterProtocol -> [NodeId] -> Chan Waiting X (Result b) -> Process (Chan Running X (Result b))
-- Composition is probably hardest part. I need to factor out
-- communication protocols.
startChan mP nodes (Compose Id x) = startChan mP nodes x
startChan mP nodes (Compose x Id) = startChan mP nodes x
startChan mP nodes (Compose (DotProduct dot) (FoldSum fold)) = do
  let ActorFoldWaiting foldActor     = fold
      ActorDotProductWaiting (i1,i2) = dot
  -- Create actor for fold
  case nodes of
    []  -> error "Not enough nodes"
    [_] -> error "Not enough nodes"
    (nid:nids) -> do
      me   <- getSelfPid
      -- Start fold process
      (pidF,_) <- spawnSupervised nid $ foldActor (ResultProtocol me)
      let boundP = BoundedProtocol pidF
      -- Split workload into equal pieces
      let n = length nids
          l = (i2 - i1 + 1) `div` n
          work = [ (i1 + i*n, if i == n-2 then i2 else i1 + i*(n+1) - 1)
                 | i <- [0 .. n-1] ]
      -- Spawn dot product workers
      pids <- forM nids $ \n -> do
        (p,_) <- spawnSupervised n $ $(mkClosure 'dotProductWorker) (mP,boundP)
        return p
      -- Return reassembled pipeline
      return $ Compose
        (DotProduct $ ActorDotProductRunning (Set.fromList pids) boundP 0 work)
        (FoldSum    $ ActorFoldRunning pidF boundP)
