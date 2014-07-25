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

import Control.Applicative (Applicative(..),(<$>))
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
----------------------------------------------------------------

-- | Start execution of actor graph.
runActorGraph :: ActorGraph ->Process ()
runActorGraph gr = do
  say $ "GRAPH:\n" ++ (prettify gr)
  -- Start actor for every node in the graph. Their PIDs are stored in
  -- nodes of new graph.
  runGr <- startActors gr
  -- Send connection info to each of the actors. To need to start
  -- processes first otherwise we can't build connection network
  forM_ (nodes runGr) $ \n -> do
    let cxt         = context runGr n
        (pid,anode) = lab' cxt
        remotes     = [ (i, pid) | (_,AConn i _) <- lsuc' cxt]
    case anode of
      ANode (_::a) ->
        case buildConnections remotes :: Maybe (HListF (Outputs a) Remote) of
          Nothing -> error "Bad connections"
          Just  p -> send pid p
  -- Wait until all actors are ready. When all actors establish
  -- connections we send Start message to each of them
  let readyLoop pending ready
        | Set.null pending = T.forM_ ready $ \p -> send p Start
        | otherwise        = do Initialized p <- expect
                                readyLoop (Set.delete p pending) (Set.insert p ready)
  readyLoop (Set.fromList [ pid | (pid,_) <- map (lab' . context runGr) (nodes runGr)])
             Set.empty
  -- Stuck here forever
  --
  -- FIXME: we want to set up some monitoring of actors
  forever $ liftIO $ threadDelay 1000000


-- Start actors and assign
startActors :: ActorGraph -> Process (Gr (ProcessId,ANode) AConn)
startActors = nmapM $ \n@(ANode a) -> do
  me  <- getSelfPid
  pid <- spawnLocal $ runActor me a
  return (pid,n)


-- | Start actor execution.
runActor :: forall a. Actor a
         => ProcessId           -- PID of master process
         -> a                   -- Actor state
         -> Process ()
runActor master actor = do
  me               <- getSelfPid
  -- Obtain information for actor execution
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
    Nil -> send master (Initialized me)
    _   -> return ()
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
  -- Start main loop
  say $ "actor `" ++ show (typeOf actor) ++ "' started"
  let aloop = case handlers of
        StateMachine h ->
          let matches = monomorphize2
                          (\(MsgHandler f) (Channels _ port) -> matchChan port (\a -> return (f outs a))
                          ) h inputs
          in \s -> do act <- receiveWait matches
                      aloop =<< act s
        Source step -> \s -> do ms <- step outs s
                                case ms of Nothing -> return ()
                                           Just s' -> aloop s'
  aloop =<< initS

sendConnection :: GetPort -> HListF xs Channels -> Process ()
sendConnection _ Nil = say "OOPS" >> error "sendConnection: unknown connection type"
sendConnection port@(GetPort fp i pid) (Channels p _ :. xs)
  | fp == fpx = send pid $ Connection i (wrapMessage p)
  | otherwise = sendConnection port xs
  where
    fpx = getFingerprint p


-- Set connection for a process
setConnection :: Connection
              -> HListF xs (Either ProcessId `Compose` SendPort)
              -> HListF xs (Either ProcessId `Compose` SendPort)
setConnection (Connection 0 msg) (Compose x :. xs) =
  case x of
    Right _ -> error "setConnection: connection is already established"
    Left _  -> case runIdentity $ unwrapMessage msg of
                 Nothing -> error "oops!"
                 Just  p -> Compose (Right p) :. xs
setConnection (Connection n msg) (x :. xs)
  = x :. setConnection (Connection (n-1) msg) xs
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


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

nmapM :: Monad m => (a -> m b) -> Gr a c -> m (Gr b c)
nmapM f = ufold step (return empty)
  where
    step (p,v,a,s) mgr = do gr <- mgr
                            b  <- f a
                            return $ (p,v,b,s) & gr
