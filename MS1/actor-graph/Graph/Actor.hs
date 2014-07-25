{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
-- | Description of actor
module Graph.Actor (
    -- * Type classes
    Actor(..)
  , ActorDict(..)
  , AllocChans(..)
  , Connectable
  , buildConnections
  , graphConnections
    -- * Extra data types
  , Channels(..)
  , Remote(..)
  , MsgHandler(..)
  , ActorHandlers(..)
  , Conn(..)
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Data.Binary (Binary)
import Data.Typeable
import Data.Graph.Inductive (Node)

import Data.HListF


----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Type class for actors
class ( Connectable  (Outputs a)
      , AllocChans   (Inputs a)
      , Serializable (HListF (Outputs a) Remote)
      , Typeable a
      , Binary a
      ) => Actor a where
  -- | List of input types for the actor. We assume that they are
  --   unique since we dispatch by type. It's however not enforced.
  type Inputs     a :: [*]
  -- | List of output types.
  type Outputs    a :: [*]
  -- | State of actor which is threaded
  type ActorState a :: *
  -- | Create representation of actor. It generate pair of all
  --   handlers for an actor and action to create initial state.
  startActor :: a -> Process ( Process (ActorState a)
                             , ActorHandlers a
                             )


-- | Reification of actor dictionary
data ActorDict a where
  ActorDict :: Actor a => ActorDict a

-- | Type class which means that for every type in the list we can
--   create channel for each type.
class AllocChans xs where
  allocChans :: Process (HListF xs Channels)

instance AllocChans '[] where
  allocChans = return Nil

instance (Serializable x,AllocChans xs) => AllocChans (x ': xs) where
  allocChans = do (sp,rp) <- newChan
                  rest    <- allocChans
                  return $ Channels sp rp :. rest


-- | Type class for constructing table connection
class Connectable xs where
  doBuildeConnections :: Int -> [(Int,ProcessId)] -> Maybe (HListF xs Remote)
  doGraphConnections  :: Node -> Int -> HListF xs Conn


instance Connectable '[] where
  doBuildeConnections _ _ = Just Nil
  doGraphConnections  _ _ = Nil

instance (Serializable x, Connectable xs) => Connectable (x ': xs) where
  doBuildeConnections off pids = do
    x  <- off `lookup` pids
    xs <- doBuildeConnections (off+1) pids
    return $ Remote x :. xs
  doGraphConnections n i = Conn i n :. doGraphConnections n (i+1)

buildConnections :: Connectable xs => [(Int,ProcessId)] -> Maybe (HListF xs Remote)
buildConnections = doBuildeConnections 0

graphConnections :: Connectable xs => Node -> HListF xs Conn
graphConnections n = doGraphConnections n 0


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Wrapper data type for both ends of channels
data Channels a = Channels (SendPort a) (ReceivePort a)

-- | Wrapper for remote ID
newtype Remote a = Remote ProcessId
                   deriving (Typeable,Binary)

-- | Handler for incoming message of type /i/. /s/ is state of actor
--   and /outs/ is list of outputs
newtype MsgHandler s outs i = MsgHandler (HListF outs SendPort -> i -> s -> Process s)

-- | Handlers for actor /a/. It could be either list of handlers or
--   action or action to perform
data ActorHandlers a
  = StateMachine (HListF (Inputs a) (MsgHandler (ActorState a) (Outputs a)))
  | Source       (HListF (Outputs a) SendPort -> ActorState a -> Process (Maybe (ActorState a)))

-- | Phantom typed connection info
data Conn a = Conn Int Node
