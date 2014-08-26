{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DNA (
    -- * Definition of actors
    ActorDef
  , actor
  , simpleOut
  , rule
  , scatterGather
  , producer
  , startingState  
    -- * Definition of dataflow graph
  , Dataflow
  , A
  , buildDataflow
  , use
  , connect  
    -- * Compilation
  , compile
  ) where

import Control.Applicative
import Control.Monad.Trans.State.Strict

import Data.Typeable
import Data.Graph.Inductive.Graph hiding (match)
import qualified Data.Traversable as T
import qualified Data.Map as Map

import DNA.AST
import DNA.Actor
import DNA.Compiler.Types
import DNA.Compiler.Basic
import DNA.Compiler.Scheduler

----------------------------------------------------------------
-- Monad for defining actors
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
-- Dataflow graph definition
----------------------------------------------------------------

-- | Monad for building dataflow graph
type Dataflow = (State (Int, [(Node,BuildNode)], [(Node,Node,ConnId)]))

-- | Node of a dataflow graph which is used during graph construction
data BuildNode where
  BuildNode :: Actor outs -> BuildNode

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



----------------------------------------------------------------
-- Compilation helpers
----------------------------------------------------------------


-- | Compile program and write generated program
compile
  :: (DataflowGraph -> Compile a) -- ^ Code generator
  -> (a -> IO ())                 -- ^ Action to write code
  -> CAD                          -- ^ Cluster description
  -> Dataflow ()                  -- ^ Graph
  -> IO ()
compile codegen write cad gr = do
  let r = runCompile $ codegen
                   =<< checkSchedule
                   =<< schedule cad
                   =<< checkGraph
                   =<< buildDataflow gr
  case r of
    Left  errs -> mapM_ putStrLn errs
    Right a    -> write a
