{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main(main) where

import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)

import Data.Typeable
import Data.HListF
import Graph.Actor
import Graph.Graph

----------------------------------------------------------------

-- Simple actor which prints to stdout everything it gets
data Printer a = Printer
                 deriving (Show,Typeable)

instance (Show a, Serializable a) => Actor (Printer a) where
  type Inputs     (Printer a) = '[a]
  type Outputs    (Printer a) = '[]
  type ActorState (Printer a) = ()
  startActor _ = return ( return ()
                        , StateMachine
                        $ (MsgHandler $ \_ a _ -> say $ "*** " ++ show a)
                        :. Nil
                        )


-- Simple data source
data Src a = Src [a]
           deriving (Show,Typeable)

instance (Show a, Serializable a) => Actor (Src a) where
  type Inputs     (Src a) = '[]
  type Outputs    (Src a) = '[a]
  type ActorState (Src a) = [a]
  startActor (Src list) = return
    ( return list
    , Source $ \(port :. Nil) as -> case as of
        []   -> return Nothing
        x:xs -> sendChan port x >> return (Just xs) 
    )


----------------------------------------------------------------


  
main :: IO ()
main = do
  backend <- initializeBackend "localhost" "8001" initRemoteTable
  startMaster backend $ \_ -> do
    say "Started"
    runActorGraph $ runGraphBuilder $ do
      (p, Nil)       <- use (Printer :: Printer Int)
      (_, cS :. Nil) <- use (Src [1 .. 10::Int])
      connect cS p
