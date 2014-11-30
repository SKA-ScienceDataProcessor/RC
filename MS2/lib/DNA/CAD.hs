{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveDataTypeable #-}
module DNA.CAD where

import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import Control.Distributed.Process

import DNA.CmdOpts
import DNA.Monitor

import GHC.Generics (Generic)

----------------------------------------------------------------
-- Basic CAD operations
----------------------------------------------------------------
-- | Cluster architecture description. Nodes are arranged into rose
--   tree and it's polymorphic in 
data CAD a = CAD a [CAD a]
             deriving (Show,Typeable,Generic,Functor)

-- | Information about node 
data NodeInfo = NodeInfo ProcessId
              deriving (Show,Typeable,Generic)

instance Binary a => Binary (CAD a)
instance Binary NodeInfo

-- | Make CAD from list of nodes. At the moment w don't use any
--   information about nodes.
makeCAD :: [a] -> CAD a
makeCAD []     = error "DNA.CAD.makeCAD: empty list of nodes"
makeCAD (x:xs) = CAD x [CAD a [] | a <- xs]


-- | 
spawnHierachically :: CAD NodeId -> Process (CAD NodeInfo)
spawnHierachically = undefined


----------------------------------------------------------------
-- Controller process for node
----------------------------------------------------------------

-- Each node have controller process which monitors child node and
-- spawns actors.
nodeController :: CAD NodeId -> Process ()
nodeController (CAD _ children) = do
  undefined





{-




-}
