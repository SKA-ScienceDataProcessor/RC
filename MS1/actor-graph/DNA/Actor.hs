{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module DNA.Actor where

import Control.Applicative
import Control.Monad.Trans.State.Strict

import Data.Typeable
import Data.Graph.Inductive.Graph hiding (match)
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Traversable as T
import qualified Data.IntMap as IntMap

import DNA.AST
import DNA.Compiler.Types



----------------------------------------------------------------
-- Actor building
----------------------------------------------------------------

-- Information about outgoing connection for the actor. It's pair of
-- connection index and type of the connection. Type is also stored by
-- the actor so it's needed as sanity check.
data ConnInfo = ConnInfo
  ConnId   -- ID of outgoing port
  TypeRep  -- Type of messages
  deriving (Show)

-- Set of outgoing connections for the actor
type ConnMap = IntMap.IntMap TypeRep

-- | Collection of outbound connections
class ConnCollection a where
  type Connected a
  setActorId     :: Node -> a -> Connected a
  nullConnection :: a -> Connected a

data Bound a = Bound Node a
             | Failed

instance ConnCollection (ConnFree a) where
  type Connected (ConnFree a) = Bound (ConnFree a)
  setActorId n c = Bound n c
  nullConnection _ = Failed

instance (ConnCollection a, ConnCollection b) => ConnCollection (a,b) where
  type Connected (a,b) = (Connected a, Connected b)
  setActorId i (a,b) = (setActorId i a, setActorId i b)
  nullConnection _ = (nullConnection (undefined :: a), nullConnection (undefined :: b))

instance ConnCollection () where
  type Connected () = ()
  setActorId _ = id
  nullConnection _ = ()




-- | Monad for defining actor
newtype ActorDef s a = ActorDef (State (ActorDefState s) a)
                     deriving (Functor,Applicative,Monad)

data ActorDefState s = ActorDefState
  { adsRules :: [Rule s]        -- Transition rules
  , adsInit  :: [Expr () s]     -- Initial state
  , adsProd  :: [Expr () (s -> (s,Out))]
  , adsConns :: ConnMap         -- Outbound connections
  }


-- | Simple connection information
simpleOut :: forall s a. Typeable a => ActorDef s (ConnFree a)
simpleOut = ActorDef $ do
  st <- get
  let conns = adsConns st
      n     = IntMap.size conns
  put $! st { adsConns = IntMap.insert n (typeOf (undefined :: a)) conns }
  return $ ConnFree (ConnId n)

-- | Transition rule for an actor
rule :: Expr () (s -> a -> (s,Out)) -> ActorDef s ()
rule f = ActorDef $ do
  modify $ \st -> st { adsRules = Rule f : adsRules st }

producer :: Expr () (s -> (s,Out)) -> ActorDef s ()
producer f = ActorDef $ do
  modify $ \st -> st { adsProd = f : adsProd st }

-- | Set initial state for the
startingState :: Expr () s -> ActorDef s ()
startingState s = ActorDef $ do
  modify $ \st -> st { adsInit = s : adsInit st }

-- | Generate actor representation
actor :: ConnCollection outs => ActorDef s outs -> Actor outs
actor (ActorDef m) =
  case s of
    ActorDefState _  []  _   _ -> oops "No initial state specified"
    ActorDefState [] _   []  _ -> oops "No transition rules/producers"
    ActorDefState rs [i] []  c -> Actor outs (StateM   c i rs)
    ActorDefState [] [i] [f] c -> Actor outs (Producer c i f)
    ActorDefState [] _   _   _ -> oops "Several producer steps specified"
    ActorDefState _  _   _   _ -> oops "Several initial states specified"
  where
    (outs,s) = runState m $ ActorDefState [] [] [] IntMap.empty
    oops = Invalid . pure


----------------------------------------------------------------
-- Actor representation
----------------------------------------------------------------

-- | Representation of actor
data Actor outs
  -- We allow invalid actors. Instead of checking at construction time
  -- all errors are reported during compilation phase
  = Actor outs Actor'
  | Invalid [String]

-- Real description of an actor
data Actor' where
  -- State machine
  StateM   :: ConnMap
           -> Expr () s
           -> [Rule s]
           -> Actor'
  -- Actor which produces data
  Producer :: ConnMap
           -> Expr () s
           -> Expr () (s -> (s,Out))
           -> Actor'
  -- FIXME: scatter/gather primitives does not have
  -- Scatted data
  -- Gather data


-- Transition rule for the state machine
data Rule s where
  Rule :: Expr () (s -> a -> (s,Out)) -- Transition rule
       -> Rule s



----------------------------------------------------------------
-- Dataflow graph
----------------------------------------------------------------

-- Node of a graph
data ANode where
  ANode :: Actor outs -> ANode

data ANode' where
  ANode' :: Int                 -- Node index (default 0)
         -> Actor'              -- Actor data
         -> ANode'

-- | Complete description of dataflow graph
type DataflowGraph = Gr ANode' ConnInfo




----------------------------------------------------------------
-- Language for dataflow graph definition
----------------------------------------------------------------

-- | Monad for building dataflow graph
type Dataflow = (State (Int, [(Int,ANode)], [(Int,Int,ConnInfo)]))

data A = A Node

buildDataflow :: Dataflow () -> Compile DataflowGraph
buildDataflow m
  = applicatively
  $ (\n -> mkGraph n es) <$> T.traverse validate ns
  where
    validate (_,ANode (Invalid errs)) = leftA errs
    validate (i,ANode (Actor _ a))      = pure (i,ANode' 0 a)
    (_,ns,es) = execState m (0,[],[])


-- | Bind actor as node in the dataflow graph. It returns handle to
--   the node and collection of its outputs.
use :: forall outs. ConnCollection outs => Actor outs -> Dataflow (A, Connected outs)
use a = do
  (i,acts,conns) <- get
  put (i+1, (i,ANode a) : acts, conns)
  return (A i, case a of
                 Invalid _                -> nullConnection (undefined :: outs)
                 Actor o (StateM   _ _ _) -> setActorId i o
                 Actor o (Producer _ _ _) -> setActorId i o
         )

-- | Connect graphs
connect :: forall a. Bound (ConnFree a) -> A -> Dataflow ()
connect Failed _ = return ()
connect (Bound from (ConnFree i)) (A to) = do
  (j, acts, conns) <- get
  put ( j
      , acts
      , (from,to,ConnInfo i (typeOf (undefined :: a))) : conns
      )
