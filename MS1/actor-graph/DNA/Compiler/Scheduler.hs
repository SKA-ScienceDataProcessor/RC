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

-- | Simple scheduler. It assumes that all optimization passes are
-- done and will not try to transform graph.
schedule :: CAD -> DataflowGraph -> Compile DataflowGraph
schedule nTotal gr = do
  let nMust = nMandatory gr
      nVar  = nVarGroups gr
      nFree = nTotal - nMust - nVar -- We need at least 1 node per group
  when (nFree < 0) $
    compError ["Not enough nodes to schedule algorithm"]
  -- Schedule for mandatory nodes. For each node we
  let mandSched = IntMap.fromList $ mandatoryNodes gr `zip` [0..]
  -- Now we should schedule remaining nodes.
  let varGroups = contiguosChunks nMust (splitChunks nVar (nTotal - nMust))
      varSched  = IntMap.fromList $ variableNodes gr `zip` varGroups
  -- Build schedule for every
  let sched i (ANode _ a@(RealActor _ act)) =
        case act of
          StateM{}        -> ANode (Single n) a
          Producer{}      -> ANode (Single n) a
          ScatterGather{} -> ANode (MasterSlaves n (varSched ! i)) a
        where
          n = mandSched ! i
  return $ gmap (\(a1, i, a, a2) -> (a1, i, sched i a, a2)) gr

-- | Total number of processes that we must spawn.
nMandatory :: DataflowGraph -> Int
nMandatory gr
  = sum [ getN $ lab' $ context gr n | n <- nodes gr]
  where
    getN (ANode _ _) = 1

-- | List of all actors for which we must allocate separate cluster node.
mandatoryNodes :: DataflowGraph -> [Node]
mandatoryNodes gr =
  [ n | n <- nodes gr]

variableNodes :: DataflowGraph -> [Node]
variableNodes gr =
  [ n | n <- nodes gr
      , isSG (lab' $ context gr n)
      ]
  where
    isSG (ANode _ (RealActor _ (ScatterGather{}))) = True
    isSG  _                                        = False

-- | Number of groups for which we can vary number of allocated nodes
nVarGroups :: DataflowGraph -> Int
nVarGroups gr
  = sum [ get $ lab' $ context gr n | n <- nodes gr]
  where
    get (ANode _ (RealActor _ (ScatterGather{}))) = 1
    get _                                         = 0


-- Split N items to k groups.
splitChunks :: Int -> Int -> [Int]
splitChunks nChunks nTot
  = map (+1) toIncr ++ rest
  where
    (chunk,n)     = nTot `divMod` nChunks
    (toIncr,rest) = splitAt n (replicate nChunks chunk)

contiguosChunks :: Int -> [Int] -> [[Int]]
contiguosChunks off = go [off ..]
  where
    go _   []    = []
    go xs (n:ns) = case splitAt n xs of
                     (a,rest) -> a : go rest ns
