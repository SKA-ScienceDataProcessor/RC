-- | Basic functions which 
module DNA.Compiler.Basic (
    checkGraph
  , checkSchedule  
  ) where

import Control.Applicative

import Data.Graph.Inductive.Graph
import qualified Data.Map as Map
import qualified Data.Foldable as T

import DNA.Actor
import DNA.AST
import DNA.Compiler.Types


-- | Check that all connections in the dataflow graph are present
checkGraph :: DataflowGraph -> Compile DataflowGraph
checkGraph gr = do
  applicatively $ T.sequenceA_ [checkNodeConn (context gr n) | n <- nodes gr]
  return gr


-- | Check that all outgoing connection of an node connections are present.
checkNodeConn :: Context ANode ConnId -> CompileA ()
checkNodeConn (_,_,ANode _ (RealActor cmap _), conns)
  | T.all (==1) (Map.unionWith (+) actual expected)
    = pure ()
  | otherwise
    = compErrorA ["Bad connections"]
  where
    -- List of outgoing connection for the actor in the graph. Note
    -- that we can connect single port to several other actors.
    actual = Map.fromListWith (+) [(i,1) | (i,_) <- conns]
    -- Connection which should be present
    expected = (0 :: Int) <$ cmap


-- | Check if scheduling is correct
checkSchedule :: DataflowGraph -> Compile DataflowGraph
checkSchedule gr = do
  applicatively $ T.sequenceA_
    [ let Just (ANode sched (RealActor _ a)) = lab gr n
      in case (sched,a) of
           (Single{},       StateM{}       ) -> pure ()
           (Single{},       Producer{}     ) -> pure ()
           (MasterSlaves{}, ScatterGather{}) -> pure ()
           _ -> compErrorA ["Bad schedule"]
    | n <- nodes gr
    ]
  return gr
