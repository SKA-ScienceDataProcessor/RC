{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- | Building and analyzing of graph for actor network
module Graph.Graph (
    -- * Actor network description
    ActorGraph
  , ANode
  , AConn
    -- * Running actor graph
  , runActorGraph
  , runActor
    -- * Building actor graph
  , GraphBuilder
  , runGraphBuilder
  , A
  , Conn
  , use
  , connect
  , Member
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Serializable

import Data.Binary (Binary)
import qualified Data.Binary     as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Binary.Get as Binary
import Data.Typeable
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Graph.Inductive.Graph hiding (match)
import Data.Graph.Inductive.PatriciaTree
import qualified Data.IntMap as IntMap
import qualified Data.Foldable as T
import qualified Data.Set    as Set

import GHC.Generics (Generic)

import Data.HListF
import Graph.Actor



----------------------------------------------------------------
-- Graph data types
----------------------------------------------------------------

-- | Graph of actors
type ActorGraph = Gr ANode AConn

-- | In node type we store existentially wrapped actor
data ANode where
  ANode :: Actor a => a -> ANode

instance Show ANode where
  show (ANode a) = "ANode:(" ++ show (typeOf a) ++ ")"

-- | Connection of actor. We store both type signature and position in
--   output list of outgoing node
data AConn = AConn Int TypeRep
             deriving Show


----------------------------------------------------------------
-- Executing graph
--
-- First step is to convert explicit graph to implicit where all connection info is stored in
----------------------------------------------------------------

-- | Start actor graph execution
runActorGraph :: ActorGraph ->Process ()
runActorGraph gr = do
  me <- getSelfPid
  -- Start actor for every node in the graph
  actors <- fmap IntMap.fromList $ forM (nodes gr) $ \n -> do
    pid <- spawnLocal $ case lab' $ context gr n of
                          ANode a -> runActor me a
    return (n,pid)
  -- Send connection info to each of the actors
  forM_ (IntMap.toList actors) $ \(n,pid) -> do
    let remotes = [ (i, actors IntMap.! i)
                  | (_,AConn i _) <- lsuc' $ context gr n
                  ]
    case lab' $ context gr n of
      ANode (_ :: a) ->
        case buildConnections remotes :: Maybe (HListF (Outputs a) Remote) of
          Nothing -> error "Bad connections"
          Just  p -> send pid p
  -- Wait until all actors are ready
  let readyLoop pending ready
        | Set.null pending = T.forM_ ready $ \p -> send p Start
        | otherwise        = do Initialized p <- expect
                                readyLoop (Set.delete p pending) (Set.insert p ready)
  readyLoop (Set.fromList $ T.toList actors) Set.empty
  -- Stuck here forever
  forever $ liftIO $ threadDelay 1000000


-- | Start actor execution. We send list of its connections as message
--   since we can't
runActor :: forall a. Actor a
         => ProcessId           -- PID of master process
         -> a                   -- Actor state
         -> Process ()
runActor master actor = do
  me               <- getSelfPid
  (initS,handlers) <- startActor actor
  -- Allocate channels for receiving messages
  inputs  <- allocChans :: Process (HListF (Inputs a) Channels)
  -- Get list of connections and send connection requests to other
  -- actors
  remotes <- expect :: Process (HListF (Outputs a) Remote)
  iforHListF remotes $ \i r@(Remote conn) ->
    send conn (GetPort (getFingerprint r) i me)
  -- If actor have no outgoing connections it's ready and we send
  -- message immediately since we'll never get connection request
  case remotes of
    NilF -> send master (Initialized me)
    _    -> return ()
  -- Enter first loop where we establish connection
  let loop outs = receiveWait
          -- We received message to start. By that point all
          -- connections should be established and we should receive
        [ match $ \Start -> do
            case sequenceHListF outs of
              Right o -> return o
              Left  _ -> error "Internal error"
          -- We get request for connection. Note that `i' is index of
          -- sender's output and wee need just send it back
        , match $ \gr -> sendConnection gr inputs >> loop outs
          -- We get send port back. If all output ports are set up we
          -- notify master process
        , match $ \conn -> do
            let outs' = setConnection conn outs
            case sequenceHListF outs' of
              Left  _ -> return ()
              Right _ -> send master (Initialized me)
            loop outs'
        ]
  outs <- loop $ mapF (\(Remote a) -> Compose (Left a)) remotes
  --
  say $ "actor " ++ show (typeOf actor) ++ " started"
  say $ show $ monomorphize (\(Channels c _) -> show c) inputs
  say $ show $ monomorphize show outs
  -- let handlers = monomorphize2 (\(MsgHandler h) (Channels _ p) ->
                   -- ) 
  let aloop s =
        case handlers of
          StateMachine h -> do
            act <- receiveWait $ monomorphize2
                     (\(MsgHandler f) (Channels _ port) -> matchChan port (\a -> return (f outs a))
                     ) h inputs
            aloop =<< act s
          Source step -> do ms <- step outs s
                            say "Sending"
                            case ms of Nothing -> return ()
                                       Just s' -> aloop s'
  aloop =<< initS

sendConnection :: GetPort -> HListF xs Channels -> Process ()
sendConnection _ NilF = say "OOPS" >> error "sendConnection: unknown connection type"
sendConnection port@(GetPort fp i pid) (ConsF (Channels p _) xs)
  | fp == fpx = send pid $ Connection i (wrapMessage p)
  | otherwise = sendConnection port xs
  where
    fpx = getFingerprint p


-- Set connection for a process
setConnection :: Connection
              -> HListF xs (Either ProcessId `Compose` SendPort)
              -> HListF xs (Either ProcessId `Compose` SendPort)
setConnection (Connection 0 msg) (ConsF (Compose x) xs) =
  case x of
    Right _ -> error "setConnection: connection is already established"
    Left _  -> case runIdentity $ unwrapMessage msg of
                 Nothing -> error "oops!"
                 Just  p -> ConsF (Compose (Right p)) xs
setConnection (Connection n msg) (ConsF x xs)
  = ConsF x $ setConnection (Connection (n-1) msg) xs
setConnection _ _
  = error "setConnection: No such index"


getFingerprint :: forall f x. Typeable x => f x -> Fingerprint
getFingerprint _ = fingerprint (undefined :: x)



----------------------------------------------------------------
-- Message data types
----------------------------------------------------------------

-- | Message sent to master
newtype Initialized = Initialized ProcessId
                    deriving (Show,Typeable,Generic)
instance Binary Initialized

-- | Message which tells actor to actually start
data Start = Start
           deriving (Show,Typeable,Generic)
instance Binary Start

-- | Request for SendPort. It include type of messages being sent and position of
data GetPort = GetPort Fingerprint Int ProcessId
             deriving (Typeable,Generic)

instance Show GetPort where
  show (GetPort fp i p) = "(GetPort " ++ showFingerprint fp "" ++ " " ++ show i ++ " " ++ show p ++ ")"

instance Binary GetPort where
  put (GetPort f i p)
    = Binary.putByteString (encodeFingerprint f) >> Binary.put i >> Binary.put p
  get =  GetPort
     <$> (decodeFingerprint <$> Binary.getByteString sizeOfFingerprint)
     <*> Binary.get
     <*> Binary.get

-- | Return SendPort wrapped as a message.
data Connection = Connection Int Message
                deriving (Typeable,Generic,Show)
instance Binary Connection


----------------------------------------------------------------
-- Graph builder
----------------------------------------------------------------

-- | Simple monadic interface fro graph building
type GraphBuilder = State GState

data GState = GState !Int [LNode ANode] [LEdge AConn]

runGraphBuilder :: GraphBuilder () -> ActorGraph
runGraphBuilder m =
  case execState m (GState 0 [] []) of
    GState _ ns es -> mkGraph ns es


-- | Handle of actor. It's phantom-typed ID of an actor.
data A a = A Node


-- | Bind actor into graph node
use :: Actor a => a -> GraphBuilder (A a, HListF (Outputs a) Conn)
use a = do
  GState i ns es <- get
  put $ GState (i+1) ((i,ANode a):ns) es
  return ( A i
         , graphConnections i
         )

-- FIXME: ensure statically that types match
connect :: forall a actor. (Typeable a, Member a (Inputs actor))
        => Conn a -> A actor -> GraphBuilder ()
connect (Conn idx n) (A node) = do
  GState i ns es <- get
  put $ GState i ns ((n,node,AConn idx (typeOf (undefined :: a))) : es)

-- | Class which tells that /x/ is element in the type level list /xs/
class Member (x :: *) (xs :: [*])

-- Implementation uses overlapping instances
instance                Member x (x ': xs)
instance Member x xs => Member x (y ': xs)
