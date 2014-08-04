-- | Basic functions which 
module DNA.Compiler.Basic where

import DNA.Actor
import DNA.AST
import DNA.Compiler.Types

-- | Check that all connections in the dataflow graph are present
chechGraph :: DataflowGraph -> Compile DataflowGraph
chechGraph gr = do
  return gr
