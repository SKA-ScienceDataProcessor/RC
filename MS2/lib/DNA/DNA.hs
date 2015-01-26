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
    , Mapper(..)
    , mapper
      -- ** Shell actors
    , Shell(..)
    , Val
    , Grp
    , eval
    , evalClosure
    , startActor
    , startCollector
    , startGroup
    , startGroupN
    , startCollectorGroup
    , startCollectorGroupMR
    , startMappers
      -- * CAD & Co
    , CAD(..)
    , makeCAD
    , Location(..)
    , availableNodes
    , select
    , selectMany
      -- * Connecting actors
    , broadcast
    , sendParam
    , broadcastParamSlice
    , connect
      -- ** Promises
    , Promise
    , Group
    , await
    , gather
    , delay
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
newtype DNA a = DNA (ReaderT (ACP,Rank,GroupSize) Process a)
                deriving (Functor,Applicative,Monad,MonadIO,MonadProcess)

-- | Execute DNA program
runDNA :: ACP -> Rank -> GroupSize -> DNA a -> Process a
runDNA mon r grp (DNA dna)
    = flip runReaderT (mon,r,grp) dna

-- | Get rank of process in group
rank :: DNA Int
rank = do
    (_,Rank n,_) <- DNA ask
    return n

-- | Get size of process group
groupSize :: DNA Int
groupSize = do
    (_,_,GroupSize n) <- DNA ask
    return n

-- | Get monitor process
getMonitor :: DNA ACP
getMonitor = do
    (acp,_,_) <- DNA ask
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


-- | Mapper actor. Essentially unfoldr
data Mapper a b where
    Mapper :: (Serializable a, Serializable b, Serializable s)
           => (a -> DNA s)
           -> (s -> DNA (Maybe (s,b)))
           -> (Int -> b -> Int)
           -> Mapper a b
    deriving (Typeable)

mapper :: (Serializable a, Serializable b, Serializable s)
       => (a -> DNA s)
       -> (s -> DNA (Maybe (s, b)))
       -> (Int -> b -> Int)
       -> Mapper a b
mapper = Mapper



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
sendParam :: Serializable a => a -> Shell (Val a) b -> DNA ()
sendParam a (Shell _ recv _) = case recv of
    RecvVal       ch  -> liftP $ sendChan ch a
    RecvBroadcast grp -> case grp of
        RecvGrp p -> liftP $ forM_ p $ \ch -> sendChan ch a

-- | Broadcast same parameter to all actors in group
broadcast :: Shell (Scatter a) b -> Shell (Val a) b
broadcast (Shell a r s) = Shell a (RecvBroadcast r) s

-- | Send parameter to the group of actors. A slicing function can
-- modify the data sent to each node.
broadcastParamSlice :: Serializable a
                    => (Int -> [a]) -> Shell (Scatter a) b -> DNA ()
broadcastParamSlice slice (Shell _ (RecvGrp chans) _) = do
    zipWithM_ (\a ch -> liftP $ sendChan ch a)
      (slice (length chans))
      chans


-- | Connect output of one shell process to input of another.
connect :: Serializable b => Shell a (tag b) -> Shell (tag b) c -> DNA ()
connect (Shell childA _ sendEnd) (Shell childB recvEnd _) = do
    case (sendEnd,recvEnd) of
      -- Val
      (SendVal chDst, RecvVal chB) -> do
          -- FIXME: Do we want to allow unsafe send here?
          liftP $ sendChan chDst $ SendRemote [chB]
          sendACP $ ReqConnect childA childB []
      (SendVal chDst, RecvBroadcast (RecvGrp chans)) -> do
          liftP $ sendChan chDst $ SendRemote chans
          sendACP $ ReqConnect childA childB []
      -- Grp
      (SendGrp chDst, RecvReduce chReduce) -> do
          let chB = map snd chReduce
          liftP $ forM_ chDst $ \ch -> sendChan ch $ SendRemote chB
          sendACP $ ReqConnect childA childB [chN | (chN,_) <- chReduce ]
      -- MR
      (SendMR chDst, RecvMR chans) -> do
          let chB = map snd chans
          liftP $ forM_ chDst $ \ch -> sendChan ch chB
          sendACP $ ReqConnect childA childB [chN | (chN,_) <- chans]
      -- IMPOSSIBLE
      --
      -- GHC cannot understand that pattern match is exhaustive
      _ -> error "Impossible: pattern match is not exhaustive"



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


destFromLoc :: Location -> SendPort a -> Dest a
destFromLoc Local  = SendLocally
destFromLoc Remote = SendRemote . (:[])

-- | Create promise for single actor. It allows to receive data from
--   it later.
delay :: Serializable b => Location -> Shell a (Val b) -> DNA (Promise b)
delay loc (Shell child _ src) = do
    myACP           <- getMonitor
    (chSend,chRecv) <- liftP newChan
    let param :: Serializable b => SendEnd (Val b) -> SendPort b -> Process ()
        param (SendVal ch) p = sendChan ch $ destFromLoc loc p
    liftP   $ param src chSend
    sendACP $ ReqConnect child (SingleActor myACP) []
    return  $ Promise chRecv

-- | Create promise from group of processes which allows to collect
--   data from them later.
delayGroup :: Serializable b => Shell a (Grp b) -> DNA (Group b)
delayGroup (Shell child _ src) = do
    myACP         <- getMonitor
    (sendB,recvB) <- liftP newChan
    (sendN,recvN) <- liftP newChan
    let param :: Serializable b => SendEnd (Grp b) -> SendPort b -> Process ()
        param (SendGrp chans) chB = forM_ chans $ \ch -> sendChan ch (SendRemote [chB])
    liftP $ param src sendB
    sendACP $ ReqConnect child (SingleActor myACP) [sendN]
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
    (acp,ParamActor parent rnk grp) <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendDst,  chRecvDst  ) <- newChan
    -- Send shell process back
    let shell = Shell (SingleActor acp)
                      (RecvVal chSendParam)
                      (SendVal chSendDst  )
    send parent (acp, wrapMessage shell)
    -- Now we can start execution and send back data
    a   <- receiveChan chRecvParam
    !b  <- runDNA acp rnk grp (action a)
    sendToDestChan chRecvDst b

-- | Run actor for group of processes which allow more than 1 task per
--   actor.
runActorManyRanks :: Actor a b -> Process ()
runActorManyRanks (Actor action) = do
    -- Obtain parameters
    (acp,ParamActor parent _ grp) <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendDst,  chRecvDst  ) <- newChan
    (chSendRnk,  chRecvRnk  ) <- newChan
    -- Send shell process back
    let shell = Shell (SingleActor acp)
                      (RecvVal chSendParam)
                      (SendVal chSendDst  )
    -- Communicate back to ACP
    send parent (acp, chSendRnk, wrapMessage shell)
    a   <- receiveChan chRecvParam
    -- Now we can start execution and send back data
    dst <- receiveChan chRecvDst
    let loop = do
            send parent (acp,chSendRnk)
            mrnk <- receiveChan chRecvRnk
            case mrnk of
                Nothing  -> return ()
                Just rnk -> do !b  <- runDNA acp rnk grp (action a)
                               sendToDest dst b
                               send parent (acp,DoneTask)
                               loop
    loop

-- | Start execution of collector actor
runCollectActor :: CollectActor a b -> Process ()
runCollectActor (CollectActor step start fini) = do
    -- Obtain parameters
    (acp,ParamActor parent rnk grp) <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendDst,  chRecvDst  ) <- newChan
    (chSendN,    chRecvN    ) <- newChan
    -- Send shell process description back
    let shell = Shell (SingleActor acp)
                      (RecvReduce [(chSendN,chSendParam)])
                      (SendVal    chSendDst)
    send parent (acp, wrapMessage shell)
    -- Start execution of an actor
    !b <- runDNA acp rnk grp $ do
        s0 <- start
        s  <- gatherM (Group chRecvParam chRecvN) step s0
        fini s
    sendToDestChan chRecvDst b

-- | Start execution of collector actor
runCollectActorMR :: CollectActor a b -> Process ()
runCollectActorMR (CollectActor step start fini) = do
    -- Obtain parameters
    (acp,ParamActor parent rnk grp) <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendDst,  chRecvDst  ) <- newChan
    (chSendN,    chRecvN    ) <- newChan
    -- Send shell process description back
    let shell = Shell (SingleActor acp)
                      (RecvMR    [(chSendN,chSendParam)])
                      (SendVal    chSendDst)
    send parent (acp, wrapMessage shell)
    -- Start execution of an actor
    let loop n tot !b
          | n >= tot && tot >= 0 = return b
          | otherwise = do
              r <- liftP $ receiveWait [ matchChan chRecvParam (return . Right)
                                       , matchChan chRecvN     (return . Left)
                                       ]
              case r of
                Right (Just a) -> loop n tot =<< step b a
                Right Nothing  -> loop (n+1) tot b
                Left  k        -> loop n k b
    b <- runDNA acp rnk grp $ fini =<< loop 0 (-1) =<< start
    sendToDestChan chRecvDst b


runMapperActor :: Mapper a b -> Process ()
runMapperActor (Mapper start step shuffle) = do
    -- Obtain parameters
    (acp,ParamActor parent rnk grp) <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendDst,  chRecvDst  ) <- newChan
    -- Send shell process description back
    let shell = Shell (SingleActor acp)
                      (RecvVal chSendParam)
                      (SendMR [chSendDst])
    send parent (acp, wrapMessage shell)
    -- Get initial parameters for unfolding
    a  <- receiveChan chRecvParam
    s0 <- runDNA acp rnk grp $ start a
    -- Get destination
    dst <- receiveChan chRecvDst
    -- Unfoldr loop
    let n = length dst
    let loop s = do
            ms <- runDNA acp rnk grp $ step s
            case ms of
              Nothing     -> return ()
              Just (s',b) -> do let i = shuffle n b
                                sendChan (dst !! i) (Just b)
                                loop s'
    loop s0
    forM_ dst $ \ch -> sendChan ch Nothing

-- | Start execution of DNA program
runDnaProgram :: DNA () -> Process ()
runDnaProgram action = do
    -- Obtain parameters
    (acp,ParamActor _ rnk grp) <- expect
    runDNA acp rnk grp action

-- Send value to the destination
sendToDestChan :: (Serializable a) => ReceivePort (Dest a) -> a -> Process ()
sendToDestChan chDst a = do
    dst <- receiveChan chDst
    sendToDest dst a

-- Send value to the destination
sendToDest :: (Serializable a) => Dest a -> a -> Process ()
sendToDest dst a = case dst of
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
    -- Start actor process
    nid <- getSelfNode
    me  <- getSelfPid
    -- FIXME: understand how do we want to monitor state of child
    --        process? Do we want to just die unconditionally or maybe
    --        we want to do something.
    (pid,_) <- spawnSupervised nid act
    send pid (ACP me, actorP)
    -- Start listening on events
    startAcpLoop self pid resources

-- FIXME: duplication
runMasterACP :: ParamACP () -> DNA () -> Process ()
runMasterACP (ParamACP self () resources actorP) act = do
    taggedMessage "ACP" "Starting master ACP"
    -- Start actor process
    me  <- getSelfPid
    -- FIXME: understand how do we want to monitor state of child
    --        process? Do we want to just die unconditionally or maybe
    --        we want to do something.
    pid <- spawnLocal (link me >> runDnaProgram act)
    _   <- monitor pid
    send pid (ACP me,actorP)
    -- Start listening on events
    startAcpLoop self pid resources

remotable [ 'runActor
          , 'runActorManyRanks
          , 'runCollectActorMR
          , 'runCollectActor
          , 'runMapperActor
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

-- | Evaluate actor without forking off enother thread
evalClosure :: (Serializable a, Serializable b)
            => Closure (Actor a b)
            -> a
            -> DNA b
evalClosure clos a = do
    logMessage "executing: evalClosure"
    Actor act <- liftP $ unClosure clos
    act a


-- | Start single actor
startActor :: (Serializable a, Serializable b)
           => Resources -> Closure (Actor a b) -> DNA (Shell (Val a) (Val b))
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
               -> DNA (Shell (Grp a) (Val b))
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
           -> DNA (Shell (Scatter a) (Grp b))
startGroup res groupTy child = do
    (shellS,shellR) <- liftP newChan
    let clos = $(mkStaticClosure 'runActor) `closureApply` child
    sendACP $ ReqSpawnGroup clos shellS res groupTy
    (gid,mbox) <- liftP (receiveChan shellR)
    msgs <- mapM unwrapMessage mbox
    case sequence msgs of
      Nothing -> error "Bad shell message"
      Just  s -> return $ assembleShellGroup gid s


assembleShellGroup :: GroupID -> [Shell (Val a) (Val b)] -> Shell (Scatter a) (Grp b)
assembleShellGroup gid shells =
    Shell (ActorGroup gid)
          (RecvGrp $ map getRecv shells)
          (SendGrp $ map getSend shells)
  where
    getRecv :: Shell (Val a) b -> SendPort a
    getRecv (Shell _ (RecvVal ch) _) = ch
    getRecv _ = error "assembleShellGroup: unexpected type of shell process"
    getSend :: Shell a (Val b) -> SendPort (Dest b)
    getSend (Shell _ _ (SendVal ch)) = ch


-- | Start group of processes where we have more tasks then processes.
startGroupN
    :: (Serializable a, Serializable b)
    => [Resources] -- ^ Resources for actors
    -> GroupType   -- ^ How individual failures should be handled
    -> Int         -- ^ Number of tasks to run
    -> Closure (Actor a b)
    -> DNA (Shell (Val a) (Grp b))
startGroupN res groupTy nTasks child = do
    (shellS,shellR) <- liftP newChan
    let clos = $(mkStaticClosure 'runActorManyRanks) `closureApply` child
    sendACP $ ReqSpawnGroupN clos shellS res nTasks groupTy
    (gid,mbox) <- liftP (receiveChan shellR)
    msgs <- mapM unwrapMessage mbox
    case sequence msgs of
      Nothing -> error "Bad shell message"
      Just  s -> return $ broadcast $ assembleShellGroup gid s
    

-- | Start group of collector processes
startCollectorGroup
    :: (Serializable a, Serializable b)
    => [Resources]
    -> GroupType
    -> Closure (CollectActor a b)
    -> DNA (Shell (Grp a) (Grp b))
startCollectorGroup res groupTy child = do
    (shellS,shellR) <- liftP newChan
    let clos = $(mkStaticClosure 'runCollectActor) `closureApply` child
    sendACP $ ReqSpawnGroup clos shellS res groupTy
    (gid,mbox) <- liftP (receiveChan shellR)
    msgs <- mapM unwrapMessage mbox
    case sequence msgs of
      Nothing -> error "Bad shell message"
      Just  s -> return $ assembleShellGroupCollect gid s

assembleShellGroupCollect :: GroupID -> [Shell (Grp a) (Val b)] -> Shell (Grp a) (Grp b)
assembleShellGroupCollect gid shells =
    Shell (ActorGroup gid)
          (RecvReduce $ getRecv =<< shells)
          (SendGrp    $ map getSend shells)
  where
    getRecv :: Shell (Grp a) b -> [(SendPort Int, SendPort a)]
    getRecv (Shell _ (RecvReduce ch) _) = ch
    getSend :: Shell a (Val b) -> SendPort (Dest b)
    getSend (Shell _ _ (SendVal ch)) = ch


-- | Start group of collector processes
startCollectorGroupMR
    :: (Serializable a, Serializable b)
    => [Resources]
    -> GroupType
    -> Closure (CollectActor a b)
    -> DNA (Shell (MR a) (Grp b))
startCollectorGroupMR res groupTy child = do
    (shellS,shellR) <- liftP newChan
    let clos = $(mkStaticClosure 'runCollectActorMR) `closureApply` child
    sendACP $ ReqSpawnGroup clos shellS res groupTy
    (gid,mbox) <- liftP (receiveChan shellR)
    msgs <- mapM unwrapMessage mbox
    case sequence msgs of
      Nothing -> error "Bad shell message"
      Just  s -> return $ assembleShellGroupCollectMR gid s

assembleShellGroupCollectMR :: GroupID -> [Shell (MR a) (Val b)] -> Shell (MR a) (Grp b)
assembleShellGroupCollectMR gid shells =
    Shell (ActorGroup gid)
          (RecvMR  $ getRecv =<< shells)
          (SendGrp $ map getSend shells)
  where
    getRecv :: Shell (MR a) b -> [(SendPort Int, SendPort (Maybe a))]
    getRecv (Shell _ (RecvMR ch) _) = ch
    getSend :: Shell a (Val b) -> SendPort (Dest b)
    getSend (Shell _ _ (SendVal ch)) = ch


-- | Start group of mapper processes
startMappers
    :: (Serializable a, Serializable b)
    => [Resources]
    -> GroupType
    -> Closure (Mapper a b)
    -> DNA (Shell (Scatter a) (MR b))
startMappers res groupTy child = do
    (shellS,shellR) <- liftP newChan
    let clos = $(mkStaticClosure 'runMapperActor) `closureApply` child
    sendACP $ ReqSpawnGroup clos shellS res groupTy
    (gid,mbox) <- liftP (receiveChan shellR)
    msgs <- mapM unwrapMessage mbox
    case sequence msgs of
      Nothing -> error "Bad shell message"
      Just  s -> return $ assembleShellMapper gid s

    
assembleShellMapper :: GroupID -> [Shell (Val a) (MR b)] -> Shell (Scatter a) (MR b)
assembleShellMapper gid shells =
    Shell (ActorGroup gid)
          (RecvGrp $ map getRecv shells)
          (SendMR  $ getSend =<< shells)
  where
    getRecv :: Shell (Val a) b -> SendPort a
    getRecv (Shell _ (RecvVal ch) _) = ch
    getRecv _ = error "assembleShellGroup: unexpected type of shell process"
    getSend :: Shell a (MR b) -> [SendPort [SendPort (Maybe b)]]
    getSend (Shell _ _ (SendMR ch)) = ch
