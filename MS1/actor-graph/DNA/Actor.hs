{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module DNA.Actor (
    -- * Actor description
    ActorDef
  , Conn(..)
  , ConnInfo(..)
  , ConnMap
  , simpleOut
  , rule
  , producer
  , startingState
    -- ** Complete actor
  , Actor(..)
  , actor
    -- * Dataflow graph building
  , DataflowGraph
  , ANode(..)
  , ANode'(..)
  , buildDataflow
  , use
  , connect
    -- * Internal
  , Actor'(..)
  , Rule(..)
  ) where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

import Data.Typeable
import Data.Graph.Inductive.Graph hiding (match)
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Traversable as T
import qualified Data.Set as Set
import qualified Data.Map as Map
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
  Int      -- ID of outgoing port
  TypeRep  -- Type of messages

-- Set of outgoing connections for the actor
type ConnMap = IntMap.IntMap TypeRep


-- | Phantom typed connection information
data Conn f a where
  Conn :: Typeable a => f Int -> Int -> Conn f a

data No a = No
data Id a = Id a

-- | Description of connection
class Connectable c where
  setConnection :: Int -> c No a -> c Id a

instance Connectable Conn where
  setConnection i (Conn _ j) = Conn (Id i) j

  
-- | Collection of outbound connections
class ConnCollection a where
  type Connected a
  setActorId :: Int -> a -> Connected a

instance ConnCollection (Conn No a) where
  type Connected (Conn No a) = Conn Id a
  setActorId = setConnection
  
instance (ConnCollection a, ConnCollection b) => ConnCollection (a,b) where
  type Connected (a,b) = (Connected a, Connected b)
  setActorId i (a,b) = (setActorId i a, setActorId i b)






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
simpleOut :: forall s a. Typeable a => ActorDef s (Conn No a)
simpleOut = ActorDef $ do
  st <- get
  let conns = adsConns st
      n     = IntMap.size conns
  put $! st { adsConns = IntMap.insert n (typeOf (undefined :: a)) conns }
  return $ Conn No n

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
    ActorDefState _  []  _   _ -> oops "No initial sate specified"
    ActorDefState [] _   []  _ -> oops "No transition rules/producers"
    ActorDefState rs [i] []  c -> Actor (StateM outs c i rs)
    ActorDefState [] [i] [f] c -> Actor (Producer outs c i f)
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
  = Actor (Actor' outs)
  | Invalid [String]

-- Real description of an actor
data Actor' outs where
  -- State machine
  StateM   :: ConnCollection outs
           => outs   
           -> ConnMap
           -> Expr () s
           -> [Rule s]
           -> Actor' outs
  Producer :: ConnCollection outs
           => outs
           -> ConnMap
           -> Expr () s
           -> Expr () (s -> (s,Out))
           -> Actor' outs

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
  ANode' :: Int -> Actor' outs -> ANode'

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
    validate (i,ANode (Actor a))      = pure (i,ANode' 0 a)
    (_,ns,es) = execState m (0,[],[])

                  
-- | Bind actor as node in the dataflow graph. It returns handle to
--   the node and collection of its outputs.
use :: ConnCollection outs => Actor outs -> Dataflow (A, outs)
use a = do
  (i,acts,conns) <- get
  put (i+1, (i,ANode a) : acts, conns)
  return (A i, undefined)

-- | Connect graphs
connect :: forall a. Conn Id a -> A -> Dataflow ()
connect (Conn (Id from) i) (A to) = do
  (j, acts, conns) <- get  
  put ( j, acts
             , (from,to,ConnInfo i (typeOf (undefined :: a))) : conns
             )
