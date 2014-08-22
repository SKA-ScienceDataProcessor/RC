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
    -- * Definition of actors
  , ActorDef
  , actor
  , simpleOut
  , rule
  , scatterGather
  , producer
  , startingState
    -- * Dataflow graph
  , DataflowGraph
  , ANode(..)
  , ScheduleState(..)
    -- * Building of dataflow graph
  , Dataflow
  , A
  , buildDataflow
  , use
  , connect
  ) where

import Control.Applicative
import Control.Monad.Trans.State.Strict

import Data.Typeable
import Data.Graph.Inductive.Graph hiding (match)
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Traversable as T
import qualified Data.Map as Map

import DNA.AST
import DNA.Compiler.Types



----------------------------------------------------------------
-- Connection variations
----------------------------------------------------------------

data Bound a = Bound Node (Conn a)
             | Failed

-- | Collection of outbound connections
class ConnCollection a where
  type Connected a
  setActorId     :: Node -> a -> Connected a
  nullConnection :: a -> Connected a

instance ConnCollection (Conn a) where
  type Connected (Conn a) = Bound a
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

-- | Node of a dataflow graph which is used during graph construction
data BuildNode where
  BuildNode :: Actor outs -> BuildNode

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





----------------------------------------------------------------
-- Defining actors
----------------------------------------------------------------

-- | Monad for defining actor
newtype ActorDef s a = ActorDef (State (ActorDefState s) a)
                     deriving (Functor,Applicative,Monad)

data ActorDefState s = ActorDefState
  { adsRules :: [Rule s]
    -- Transition rules
  , adsInit  :: [Expr () s]
    -- Initial state
  , adsProd  :: [Expr () (s -> (s,Out))]
    --
  , adsSG    :: [SG]
    --
  , adsConns :: ConnMap
    -- Outbound connections
  }


-- | Simple connection information
simpleOut :: forall s a. Typeable a => ConnType -> ActorDef s (Conn a)
simpleOut ct = ActorDef $ do
  st <- get
  let conns = adsConns st
      cid   = ConnId $ Map.size conns
  put $! st { adsConns = Map.insert cid (ActorConn ct (typeOf (undefined :: a))) conns }
  return $ Conn cid ConnOne

-- | Transition rule for an actor
rule :: Expr () (s -> a -> (s,Out)) -> ActorDef s ()
rule f = ActorDef $ do
  modify $ \st -> st { adsRules = Rule f : adsRules st }

-- | Producer actor. One which sends data indefinitely
producer :: Expr () (s -> (s,Out)) -> ActorDef s ()
producer f = ActorDef $ do
  modify $ \st -> st { adsProd = f : adsProd st }

-- | Scatter-gather actor
scatterGather :: SG -> ActorDef s ()
scatterGather sg = ActorDef $ do
  modify $ \st -> st { adsSG = sg : adsSG st }

-- | Set initial state for the actor
startingState :: Expr () s -> ActorDef s ()
startingState s = ActorDef $ do
  modify $ \st -> st { adsInit = s : adsInit st }

-- | Generate actor representation
actor :: ActorDef s outs -> Actor outs
actor (ActorDef m) =
  case s of
    ActorDefState [] []  [] [sg] c -> Actor outs (RealActor c (ScatterGather sg))
    ActorDefState _  []  _   _ _ -> oops "No initial state specified"
    ActorDefState [] _   []  _ _ -> oops "No transition rules/producers"
    ActorDefState rs [i] []  _ c -> Actor outs (RealActor c (StateM   i rs))
    ActorDefState [] [i] [f] _ c -> Actor outs (RealActor c (Producer i f))
    ActorDefState [] _   _   _ _ -> oops "Several producer steps specified"
    ActorDefState _  _   _   _ _ -> oops "Several initial states specified"
  where
    (outs,s) = runState m $ ActorDefState [] [] [] [] Map.empty
    oops = Invalid . pure



----------------------------------------------------------------
-- Language for dataflow graph definition
----------------------------------------------------------------

-- | Monad for building dataflow graph
type Dataflow = (State (Int, [(Node,BuildNode)], [(Node,Node,ConnId)]))

-- | Handle for actor.
newtype A = A Node

-- | Construct dataflow graph from its description
buildDataflow :: Dataflow () -> Compile DataflowGraph
buildDataflow m
  = applicatively
  $ (\n -> mkGraph n es) <$> T.traverse validate ns
  where
    validate (_,BuildNode (Invalid errs)) = leftA errs
    validate (i,BuildNode (Actor _ a))    = pure (i,ANode NotSched a)
    (_,ns,es) = execState m (0,[],[])


-- | Bind actor as node in the dataflow graph. It returns handle to
--   the node and collection of its outputs.
use :: forall outs. ConnCollection outs => Actor outs -> Dataflow (A, Connected outs)
use a = do
  (i,acts,conns) <- get
  put (i+1, (i,BuildNode a) : acts, conns)
  return (A i, case a of
                 Invalid _  -> nullConnection (undefined :: outs)
                 Actor o _  -> setActorId i o
         )

-- | Connect graphs
connect :: Bound a -> A -> Dataflow ()
connect Failed _ = return ()
connect (Bound from (Conn i _)) (A to) = do
  (j, acts, conns) <- get
  put ( j
      , acts
      , (from,to, i) : conns
      )
