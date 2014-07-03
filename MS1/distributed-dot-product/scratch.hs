{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test where


import System.Environment (getArgs)
import Control.Applicative
import Control.Monad
import Control.Category
import Control.Monad.Trans.Except
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Serializable (Serializable)

import Data.Binary
import Data.Typeable
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Foldable as T
import Text.Printf
import GHC.Generics (Generic)
import Prelude hiding  ((.),id)

import Chan
import Worker
-- import Types
import RC.Combinators
import RC.Types

----------------------------------------------------------------
-- Main supervisor
----------------------------------------------------------------

-- | Status of node collections
data Cluster = Cluster
  { idleNodes   :: [NodeId]       -- ^ List of idle nodes
  , failedNodes :: [NodeId]       -- ^ List of nodes which crashed. 
  , leasedNodes :: Map ProcessId [NodeId]
  }


-- | Supervisor process which does almost nothing but keeps list of
--   known nodes
supervisor :: [NodeId] -> Process ()
supervisor
  = foldMessages step . (\nodes -> Cluster nodes [] Map.empty)
  where
    step cl msg =
      case msg of
        () -> return cl



----------------------------------------------------------------
-- Data processor
----------------------------------------------------------------

-- | Master process which manages set of workers.
--
--   Master sends work to workers which a
unorderedFold
  :: (Serializable a, Serializable b)
  => ProcessId                  -- ^ Supervisor
  -> [NodeId]                   -- ^ Initial list of worker nodes
  -> Closure (b -> b -> b)      -- ^ Fold function
  -> Closure (a -> b)           -- ^ Function to run on workers
  -> [a]                        -- ^ List of work to fold
  -> Process ()
unorderedFold parent nodes closFold closMap work = do
  f <- unClosure closFold
  let step Nothing b = b
      step b Nothing = b
      step (Just b) (Just b') = Just $! f b b'  
  Just b <- finiFoldMatch
       [ match2 ufoldFail
       , match2 ufoldIdle
       , match2 (ufoldResult step)
       ] (MasterState parent (Map.fromList $ zip [WorkID 1 ..] $ map ((,) Pending) work) Nothing)
  send parent (Result b)
  return ()



ufoldFail :: MasterState a b -> ProcessMonitorNotification -> Process (Either r (MasterState a b))
ufoldFail (MasterState svPid work b) (ProcessMonitorNotification _ pid e) =
  case e of
    -- Process died normally. We need to release node it runs on to
    -- the supervisor
    DiedNormal -> do send svPid (ReleaseNode $ processNodeId pid)
                     return $ Right $ (MasterState svPid work b)
    -- Process died abnormally. We still release its node. If node
    -- itself died
    _          -> do send svPid (ReleaseNode $ processNodeId pid)
                     return $ Right $ (MasterState svPid work b)


ufoldResult :: (b -> b -> b) -> MasterState a b -> WorkResult b -> Process (Either b (MasterState a b))
ufoldResult f ms@(MasterState svPid work b0) (WorkResult wid b) =
  case Map.lookup wid work of
    -- Not found. In retransmission strategy it means that we
    -- duplicated some computation. We do nothing in this case.
    Nothing -> return $ Right ms
    -- We got result for work we didn't send which is impossible.
    Just (Pending,_) -> error "Internal error"
    -- We got result for sent work. Remove it from list of pending work
    Just _ | allDone work' -> return $ Left b'
           | otherwise     -> return $ Right $ MasterState svPid work' b'
  where
    b'    = f b0 b
    work' = completed wid work

ufoldIdle :: forall a b. Serializable a
          => MasterState a b -> Idle -> Process (Either b (MasterState a b))
ufoldIdle ms@(MasterState svPid work b0) (Idle pid) =
  case getNewJob pid work of
    Nothing -> do send pid (Nothing :: Maybe a)
                  return $ Right $ ms
    Just (batch,work') -> do send pid batch
                             return $ Right (MasterState svPid work' b0)
    





completed :: WorkID -> Map WorkID (WorkState,a) -> Map WorkID (WorkState,a)
completed = Map.delete

jobFailed :: WorkID -> Map WorkID (WorkState,a) -> Map WorkID (WorkState,a)
jobFailed wid = Map.adjust (\(_,a) -> (MaybeLost,a)) wid

getNewJob :: ProcessId -> Map WorkID (WorkState,a) -> Maybe (Batch a, Map WorkID (WorkState,a))
getNewJob pid m = do
  (wid,(_,a)) <- T.find (\(_,(s,_)) -> s == Pending) $ Map.toList m
  return $ (Batch wid a, Map.adjust (\(_,x) -> (Sent pid, x)) wid m)

allDone :: Map WorkID (WorkState,a) -> Bool
allDone = all (==MaybeLost) . map fst . Map.elems

-- State of work to be sent to the workers
data WorkState
  = Pending                   -- Work wasn't started yet
  | Sent ProcessId            -- Sent to worker
  | MaybeLost                 -- Possibly lost
  deriving (Show,Eq)

-- State of master process
data MasterState a b = MasterState
  ProcessId                     -- Parent process
  -- (Set NodeId)                  -- Set of leased nodes
  -- (Set ProcessId)               -- Set of worker processes
  (Map WorkID (WorkState,a))       -- Remaining work for
  b

  
