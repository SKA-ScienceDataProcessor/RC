{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Main(main) where

import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)

import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)

import Data.HListF
import Graph.Actor
import Graph.Graph

----------------------------------------------------------------
-- Primitive actors
----------------------------------------------------------------

-- | Simple actor which prints to stdout everything it gets
data Printer a = Printer
                 deriving (Show,Typeable,Generic)
instance Binary (Printer a)

instance (Show a, Serializable a, Typeable a) => Actor (Printer a) where
  type Inputs     (Printer a) = '[a]
  type Outputs    (Printer a) = '[]
  type ActorState (Printer a) = ()
  startActor _ = return ( return ()
                        , StateMachine
                        $ (MsgHandler $ \_ a _ -> say $ "*** " ++ show a)
                        :. Nil
                        )


-- | Simple data source. Could be used for testing
data Src a = Src [a]
           deriving (Show,Typeable,Generic)
instance Binary a => Binary (Src a)

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
-- Dot-product related actors
----------------------------------------------------------------

-- | Worker for dot product
data DotWorker = DotWorker
                 deriving (Show,Typeable,Generic)
instance Binary DotWorker

instance Actor DotWorker where
  type Inputs     DotWorker = '[Input (Int,Int)]
  type Outputs    DotWorker = '[Result Double]
  type ActorState DotWorker = ()
  startActor _ = return
    ( return ()
    , StateMachine
    $ (MsgHandler $ \(r :. Nil) (Input (i1,i2)) _ -> return ())
    :. Nil
    )

newtype Result a = Result a
                   deriving (Show,Typeable,Generic,Binary)

newtype Input a = Input a
                deriving (Show,Typeable,Generic,Binary)

data Terminate = Terminate
                   deriving (Show,Typeable,Generic)

instance Binary Terminate


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
