-- | Schedule program for execution. At the moment it schedule for
--   exact number of threads
module DNA.Compiler.Scheduler where

import Control.Monad

import Data.Graph.Inductive.Graph
import qualified Data.IntMap as IntMap
import           Data.IntMap ((!))

import DNA.AST
import DNA.Actor
import DNA.Compiler.Types

----------------------------------------------------------------
-- Simple scheduler
----------------------------------------------------------------

-- | Number of nodes in the cluster
type CAD = Int

-- | Very simple scheduler 
schedule :: CAD -> DataflowGraph -> Compile DataflowGraph
schedule n gr = do
  when (n < noNodes gr) $
    compError ["Not enough nodes to schedule algorithm"]
  let ns = IntMap.fromList $ nodes gr `zip` [0..]
  return $ gmap (\(a1, i, ANode _ a, a2) -> (a1, i, ANode (ns ! i) a, a2)) gr
