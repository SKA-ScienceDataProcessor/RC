{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
-- | Description of the
module DNA.Actor (
    -- * Actor representation
    Actor(..)
  , ActorConn(..)
  , ConnMap
  , RealActor(..)
  , ActorDescr(..)
  , Rule(..)
  , SG(..)
  , ConnCollection(..)
  , Bounded(..)
  , Connection(..)
    -- * Dataflow graph
  , DataflowGraph
  , ANode(..)
  , ScheduleState(..)
  ) where

import Data.Typeable
import Data.Graph.Inductive.Graph hiding (match)
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Map as Map

import DNA.AST



----------------------------------------------------------------
-- Connection variations
----------------------------------------------------------------

data Connection a = Bound Node (Conn a)
                  | Failed

-- | Collection of outbound connections
class ConnCollection a where
  type Connected a
  setActorId     :: Node -> a -> Connected a
  nullConnection :: a -> Connected a

instance ConnCollection (Conn a) where
  type Connected (Conn a) = Connection a
  setActorId n c = Bound n c
  nullConnection _ = Failed

instance (ConnCollection a, ConnCollection b) => ConnCollection (a,b) where
  type Connected (a,b) = (Connected a, Connected b)
  setActorId i (a,b) = (setActorId i a, setActorId i b)
  nullConnection _ = (nullConnection (undefined :: a), nullConnection (undefined :: b))

instance ConnCollection () where
  type Connected () = ()
  setActorId     _  = id
  nullConnection _  = ()


----------------------------------------------------------------
-- Actor representation
----------------------------------------------------------------

-- | Abstract representation of actor.
data Actor outs
  -- | Actor. We store both collection of outputs and real actor
  --   description.
  = Actor outs RealActor
  -- | Invalid actor state. These errors will be reported during
  --   compilation phase.
  | Invalid [String]

-- | Internal representation of connections of actor.
data ActorConn = ActorConn ConnType TypeRep

-- | Map of all outgoing connection of actor
type ConnMap = Map.Map ConnId ActorConn

-- | Representation of an actor. It's pair of connection map and it's
--   inner working
data RealActor = RealActor ConnMap ActorDescr

-- | Description of inner working of actor.
data ActorDescr where
  -- State machine
  StateM
    :: Expr () s
    -> [Rule s]
    -> ActorDescr
  -- Actor which produces data
  Producer
    :: Expr () s
    -> Expr () (s -> (s,Out))
    -> ActorDescr
  -- Scatter/gather actor
  ScatterGather
    :: SG
    -> ActorDescr

-- | Transition rule for the state machine
data Rule s where
  Rule :: Expr () (s -> a -> (s,Out)) -- Transition rule
       -> Rule s

-- | State of scatter-gather actor.
data SG where
  SG :: (Expr () c, Expr () (c -> c -> c), Expr () (c -> Out))
     -> Expr () (b -> c)
     -> Expr () (Int -> a -> [b])
     -> SG



----------------------------------------------------------------
-- Dataflow graph
----------------------------------------------------------------

-- | Complete description of dataflow graph.
--
type DataflowGraph = Gr ANode ConnId

-- | Node of dataflow graph
data ANode = ANode
  ScheduleState       -- How actor is scheduled
  RealActor           -- Actor data

-- | How actor is schedule to run
data ScheduleState
  = NotSched
    -- ^ Actor is not scheduled
  | Single  Int
    -- ^ Actor scheduled for execution on single node
  | MasterSlaves Int [Int]
    -- ^ Actor scheduled for execution on many nodes
  deriving (Show)
