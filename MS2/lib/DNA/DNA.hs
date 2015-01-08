{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
-- | DNA monad and actor creation and communication primitives.
--
--   Actors track list of nodes they own and monitor their immediate
--   children. We also have to impose important limitation: children
--   cannot outlive their parents. It's quite reasonable to allow it
--   but at the same time it could lead to processes which hangs
--   around forever because no one will request their result. We need
--   some kind of distributed garbage collection to reap such
--   processes.
--
--   When spawned process\/group of processes returns handle for
--   obtaining result of their computation. It could be serialized and
--   send to other processes.
module DNA.DNA (
      -- * DNA monad
      DNA(..)
    , runDNA
    , rank
    , groupSize
    , getMonitor
    , logMessage
    , duration
      -- * Actors
    , Actor(..)
    , actor
    , CollectActor(..)
    , collectActor
      -- ** Shell actors
    , Shell(..)
    , CollectorShell(..)
    , ShellGroup(..)
    , GroupCollect(..)
    , eval
    , startActor
    , startCollector
    , startGroup
    , startCollectorGroup
      -- * CAD & Co
    , CAD(..)
    , makeCAD
    , Location(..)
    , availableNodes
    , select
    , selectMany
      -- * Connecting actors
    , sendParam
    , broadcastParam
    , broadcastParamSlice
    , connect
    , broadcast
    , collect
    , connectCollectorGroup
      -- ** Promises
    , Promise
    , Group
    , await
    , gather
    , delay
    , delayCollector
    , delayGroup
      -- * Starting actors
    , runActor
    , runCollectActor
    , runACP
    , runMasterACP
    , __remoteTable
    , runACP__static
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Distributed.Static (closureApply)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable (Serializable)

import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)

import DNA.Types
import DNA.Controller hiding (__remoteTable)
import DNA.Logging



----------------------------------------------------------------
-- DNA monad
----------------------------------------------------------------

-- | Monad for defining DNA programs. Actors could spawn other
--   actors. One important limitation is that actors cannot outlive
--   their parent. Otherwise we could have processes whose results
--   will be never requested and no way to reap such deadlocked
--   processes.
--
--   Every actor owns set of nodes on which it could spawn other actors.
--   Upon completion this set of nodes is returned to parent actor.
newtype DNA a = DNA (ReaderT (ACP,NodeInfo,Rank,GroupSize) Process a)
                deriving (Functor,Applicative,Monad,MonadIO,MonadProcess)

-- | Execute DNA program
runDNA :: ACP -> NodeInfo -> Rank -> GroupSize -> DNA a -> Process a
runDNA mon ninfo r grp (DNA dna)
    = flip runReaderT (mon,ninfo,r,grp) dna

-- | Get rank of process in group
rank :: DNA Int
rank = do
    (_,_,Rank n,_) <- DNA ask
    return n

-- | Get size of process group
groupSize :: DNA Int
groupSize = do
    (_,_,_,GroupSize n) <- DNA ask
    return n

-- | Get monitor process
getMonitor :: DNA ACP
getMonitor = do
    (acp,_,_,_) <- DNA ask
    return acp

-- | Send message to actor's controller
sendACP :: (Binary a, Typeable a) => a -> DNA ()
sendACP a = do
    ACP pid <- getMonitor
    liftP $ send pid a


-- | Put message into log file
logMessage :: String -> DNA ()
logMessage = eventMessage

duration :: String -> DNA a -> DNA a
duration msg dna = do
    pid <- liftP getSelfPid
    let msg' = "[" ++ show pid ++ "] " ++ msg
    timePeriod msg' dna



----------------------------------------------------------------
-- Data types for actors
----------------------------------------------------------------

-- | Actor which receive messages of type @a@ and produce result of
--   type @b@. It's phantom-typed and could only be constructed by
--   'actor' which ensures that types are indeed correct.
data Actor a b where
    Actor :: (Serializable a, Serializable b) => (a -> DNA b) -> Actor a b
    deriving (Typeable)

-- | Smart constructor for actors. Here we receive parameters and
--   output channel for an actor
actor :: (Serializable a, Serializable b)
      => (a -> DNA b)
      -> Actor a b
actor = Actor


-- | Actor which collects multiple inputs from other actors
data CollectActor a b where
    CollectActor :: (Serializable a, Serializable b)
                 => (s -> a -> DNA s)
                 -> DNA s
                 -> (s -> DNA b)
                 -> CollectActor a b
    deriving (Typeable)

-- | Smart constructor for collector actors.
collectActor
    :: (Serializable a, Serializable b, Serializable s)
    => (s -> a -> DNA s)
    -> DNA s
    -> (s -> DNA b)
    -> CollectActor a b
collectActor = CollectActor



----------------------------------------------------------------
-- CAD
----------------------------------------------------------------

-- | Make CAD from list of nodes. At the moment w don't use any
--   information about nodes.
makeCAD :: [a] -> CAD a
makeCAD []     = error "DNA.CAD.makeCAD: empty list of nodes"
makeCAD (x:xs) = CAD x [CAD a [] | a <- xs]

-- | Number of available nodes
availableNodes :: DNA Int
availableNodes = do
    sendACP ReqNumNodes
    liftP expect

-- | Allocate N nodes to single actor
select
    :: Location
       -- ^ Should actor be executed on local node or on remote one
    -> Res
       -- ^ How many nodes allocate to actor. Local node is not
       --   counted here.
    -> DNA Resources
select loc n = do
    sendACP $ ReqResources loc n
    liftP expect

-- | Allocate N nodes for group of actors. Each will have only single
--   node
selectMany
    :: Res
       -- ^ How many nodes allocate to the group
    -> ResGroup
       -- ^ How to allocate resources for individual processes in
       --   group
    -> [GrpFlag]
       -- ^ Additional flags which influence resource allocation
    -> DNA [Resources]
selectMany n g f = do
    sendACP $ ReqResourcesGrp n g f
    liftP expect


----------------------------------------------------------------
-- Connect actors
----------------------------------------------------------------

-- | Send parameter to the actor
sendParam :: Serializable a => a -> Shell a b -> DNA ()
sendParam a (Shell ch _ _) = do
    liftP $ sendChan ch a


-- | Send parameter to the group of actors. All will receive same value.
broadcastParam :: Serializable a => a -> ShellGroup a b -> DNA ()
broadcastParam a (ShellGroup _ shells) = do
    forM_ shells (sendParam a)

-- | Send parameter to the group of actors. A slicing function can
-- modify the data sent to each node.
broadcastParamSlice :: Serializable a
                    => (Int -> [a]) -> ShellGroup a b -> DNA ()
broadcastParamSlice slice (ShellGroup _ shells) = do
    zipWithM_ sendParam (slice (length shells)) shells

-- | Connect two processes together
connect :: Serializable b => Shell a b -> Shell b c -> DNA ()
connect (Shell _ chDst childA) (Shell chB _ childB) = do
    -- FIXME: Do we want to allow unsafe send here?
    liftP $ sendChan chDst $ SendRemote [chB]
    sendACP $ ReqConnectTo childA childB


-- | Connect single process to the group of processes. All processes
--   in the group will receive same data.
broadcast :: Serializable b => Shell a b -> ShellGroup b c -> DNA ()
broadcast (Shell _ chDst childA) (ShellGroup gid chansB) = do
    liftP $ sendChan chDst $ SendRemote [ch | Shell ch _ _ <- chansB]
    sendACP $ ReqConnectToGrp childA gid


-- | Connect group of processes to collector process
collect :: Serializable b => ShellGroup a b -> CollectorShell b c -> DNA ()
collect (ShellGroup gid shells) (CollectorShell chB chN _ childB) = do
    liftP $ forM_ shells $ \(Shell _ dst _) -> do
        sendChan dst $ SendRemote [chB]
    sendACP $ ReqConnectGrp gid childB [chN]


-- | Connect group of processes to the group of collector
--   processes. All collectors will receive each message from each
--   actor in first group.
connectCollectorGroup
    :: Serializable b => ShellGroup a b -> GroupCollect b c -> DNA ()
connectCollectorGroup (ShellGroup gidA shells) (GroupCollect gidB colSh) = do
    let dsts   = [ch | CollectorShell ch _ _ _ <- colSh]
        chansN = [ch | CollectorShell _ ch _ _ <- colSh]
    liftP $ forM_ shells $ \(Shell _ ch _) ->
        sendChan ch $ SendRemote dsts
    sendACP $ ReqConnectGrpToGrp gidA gidB chansN


destFromLoc :: Location -> SendPort a -> Dest a
destFromLoc Local  = SendLocally
destFromLoc Remote = SendRemote . (:[])



----------------------------------------------------------------
-- Promises
----------------------------------------------------------------

newtype Promise a = Promise (ReceivePort a)

data Group a = Group (ReceivePort a) (ReceivePort Int)


await :: Serializable a => Promise a -> DNA a
await (Promise ch) = liftP $ receiveChan ch


gather :: Serializable a => Group a -> (b -> a -> b) -> b -> DNA b
gather g f = gatherM g (\b a -> return (f b a))

gatherM :: Serializable a => Group a -> (b -> a -> DNA b) -> b -> DNA b
gatherM (Group chA chN) f x0 = do
    let loop n tot !b
            | n >= tot && tot >= 0= do
                  return b
        loop n tot !b = do
            r <- liftP $ receiveWait [ matchChan chA (return . Right)
                                     , matchChan chN (return . Left)
                                     ]
            case r of
              Right a -> loop (n + 1) tot =<< f b a
              Left  k -> loop n k b
    loop 0 (-1) x0


-- | Create promise for single actor. It allows to receive data from
--   it later.
delay :: Serializable b => Location -> Shell a b -> DNA (Promise b)
delay loc (Shell _ chDst acp) = do
    myACP           <- getMonitor
    (chSend,chRecv) <- liftP newChan
    liftP $ sendChan chDst $ destFromLoc loc chSend
    sendACP $ ReqConnectTo acp myACP
    return  $ Promise chRecv

delayCollector :: Serializable b => Location -> CollectorShell a b -> DNA (Promise b)
delayCollector loc (CollectorShell _ _ chDst acp) = do
    myACP           <- getMonitor
    (chSend,chRecv) <- liftP newChan
    liftP $ sendChan chDst $ destFromLoc loc chSend
    sendACP $ ReqConnectTo acp myACP
    return  $ Promise chRecv


-- | Create promise from group of processes which allows to collect
--   data from them later.
delayGroup :: Serializable b => ShellGroup a b -> DNA (Group b)
delayGroup (ShellGroup gid shells) = do
    myACP         <- getMonitor
    (sendB,recvB) <- liftP newChan
    (sendN,recvN) <- liftP newChan
    liftP $ forM_ shells $ \(Shell _ chDst _) ->
        sendChan chDst $ SendRemote [sendB]
    sendACP $ ReqConnectGrp gid myACP [sendN]
    return  $ Group recvB recvN




----------------------------------------------------------------
-- Running actors
--
-- We use relatively complicated algorithm for spawning actors. To
-- spawn shell (not connected anywhere) actor we send message to our
-- ACP which in turn spawns ACP for shell actor.
--
----------------------------------------------------------------

-- | Start execution of standard actor.
runActor :: Actor a b -> Process ()
runActor (Actor action) = do
    -- Obtain parameters
    ParamActor parent rnk grp <- expect
    (ACP acp,ninfo) <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendDst,  chRecvDst  ) <- newChan
    -- Send shell process back
    send parent (acp, wrapMessage $ Shell chSendParam chSendDst (ACP acp))
    -- Now we can start execution and send back data
    a   <- receiveChan chRecvParam
    !b  <- runDNA (ACP acp) ninfo rnk grp (action a)
    dst <- receiveChan chRecvDst
    sendToDest dst b


-- | Start execution of collector actor
runCollectActor :: CollectActor a b -> Process ()
runCollectActor (CollectActor step start fini) = do
    -- Obtain parameters
    ParamActor parent rnk grp <- expect
    (ACP acp,ninfo) <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendDst,  chRecvDst  ) <- newChan
    (chSendN,    chRecvN    ) <- newChan
    -- Send shell process description back
    send parent (acp, wrapMessage $
                      CollectorShell chSendParam chSendN chSendDst (ACP acp))
    -- Start execution of an actor
    !b <- runDNA (ACP acp) ninfo rnk grp $ do
        s0 <- start
        s  <- gatherM (Group chRecvParam chRecvN) step s0
        fini s
    dst <- receiveChan chRecvDst
    sendToDest dst b


sendToDest :: (Serializable a) => Dest a -> a -> Process ()
sendToDest dst a =
    case dst of
      SendLocally ch  -> unsafeSendChan ch a
      SendRemote  chs -> forM_ chs $ \c -> sendChan c a


-- | Start execution of actor controller process (ACP). Takes triple
--   of actor closure, actor's rank and PID of process to send shell
--   back.
--
--   NOTE: again because of TH limitation we have to pass all
--         parameters AND closure of this function as messages because
--         we cannot create closure of our function ourselves.
runACP :: Process ()
runACP = do
    taggedMessage "ACP" "Starting ACP"
    -- Get parameters for ACP and actor
    ParamACP self act resources actorP <- expect
    let VirtualCAD _ ninfo _ = resources
    -- Start actor process
    nid <- getSelfNode
    me  <- getSelfPid
    -- FIXME: understand how do we want to monitor state of child
    --        process? Do we want to just die unconditionally or maybe
    --        we want to do something.
    (pid,_) <- spawnSupervised nid act
    send pid actorP
    send pid ((ACP me,ninfo) :: (ACP,NodeInfo))
    -- Start listening on events
    startAcpLoop self pid resources


-- | Start execution of standard actor.
runUnit :: DNA () -> Process ()
runUnit action = do
    -- Obtain parameters
    ParamActor _ rnk grp <- expect
    (acp,ninfo) <- expect
    runDNA acp ninfo rnk grp action


-- FIXME: duplication
runMasterACP :: ParamACP () -> DNA () -> Process ()
runMasterACP (ParamACP self () resources actorP) act = do
    taggedMessage "ACP" "Starting master ACP"
    let VirtualCAD _ ninfo _ = resources
    -- Start actor process
    me  <- getSelfPid
    -- FIXME: understand how do we want to monitor state of child
    --        process? Do we want to just die unconditionally or maybe
    --        we want to do something.
    pid <- spawnLocal (link me >> runUnit act)
    _   <- monitor pid
    send pid actorP
    send pid (ACP me,ninfo)
    -- Start listening on events
    startAcpLoop self pid resources

remotable [ 'runActor
          , 'runCollectActor
          , 'runACP
          ]


----------------------------------------------------------------
-- Shell actors
----------------------------------------------------------------

-- | Evaluate actor without forking off enother thread
eval :: (Serializable a, Serializable b)
     => Actor a b
     -> a
     -> DNA b
eval (Actor act) a = do
    logMessage "executing: eval"
    act a


-- | Start single actor
startActor :: (Serializable a, Serializable b)
           => Resources -> Closure (Actor a b) -> DNA (Shell a b)
startActor res child = do
    ACP acp         <- getMonitor
    (shellS,shellR) <- liftP newChan
    let clos = $(mkStaticClosure 'runActor) `closureApply` child
    liftP $ send acp $ ReqSpawnShell clos shellS res
    msg <- unwrapMessage =<< liftP (receiveChan shellR)
    case msg of
      Nothing -> error "Bad shell message"
      Just  s -> return s


-- | Start single collector actor
startCollector :: (Serializable a, Serializable b)
               => Resources
               -> Closure (CollectActor a b)
               -> DNA (CollectorShell a b)
startCollector res child = do
    (shellS,shellR) <- liftP newChan
    let clos = $(mkStaticClosure 'runCollectActor) `closureApply` child
    sendACP $ ReqSpawnShell clos shellS res
    msg <- unwrapMessage =<< liftP (receiveChan shellR)
    case msg of
      Nothing -> error "Bad shell message"
      Just  s -> return s


-- | Start group of processes
startGroup :: (Serializable a, Serializable b)
           => [Resources]
           -> GroupType
           -> Closure (Actor a b)
           -> DNA (ShellGroup a b)
startGroup res groupTy child = do
    (shellS,shellR) <- liftP newChan
    let clos = $(mkStaticClosure 'runActor) `closureApply` child
    sendACP $ ReqSpawnGroup clos shellS res groupTy
    (gid,mbox) <- liftP (receiveChan shellR)
    msgs <- mapM unwrapMessage mbox
    case sequence msgs of
      Nothing -> error "Bad shell message"
      Just  s -> return (ShellGroup gid s)


-- | Start group of collector processes
startCollectorGroup
    :: (Serializable a, Serializable b)
    => [Resources]
    -> GroupType
    -> Closure (CollectActor a b)
    -> DNA (GroupCollect a b)
startCollectorGroup res groupTy child = do
    (shellS,shellR) <- liftP newChan
    let clos = $(mkStaticClosure 'runCollectActor) `closureApply` child
    sendACP $ ReqSpawnGroup clos shellS res groupTy
    (gid,mbox) <- liftP (receiveChan shellR)
    msgs <- mapM unwrapMessage mbox
    case sequence msgs of
      Nothing -> error "Bad shell message"
      Just  s -> return (GroupCollect gid s)
