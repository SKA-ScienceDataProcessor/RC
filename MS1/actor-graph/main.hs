{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Main(main) where

import Control.Distributed.Process
import Control.Distributed.Process.Serializable

import Data.HListF
import Graph.Actor
import Graph.Graph

----------------------------------------------------------------

-- Simple actor which prints to stdout everything it gets
data Printer a = Printer

instance (Show a, Serializable a) => Actor (Printer a) where
  type Inputs     (Printer a) = '[a]
  type Outputs    (Printer a) = '[]
  type ActorState (Printer a) = ()
  startActor _ = return ( return ()
                        , ConsF (ActorHandler $ \_ a _ -> liftIO (print a))
                          NilF
                        )


-- Source
-- data Source a = Source [a]



main :: IO ()
main = do
  return ()
