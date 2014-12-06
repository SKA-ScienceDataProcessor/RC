{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Controller processes for node and actor
module DNA.Controller (
      -- * Starting ACP
      startAcpLoop
    , ACP(..)
      -- * Control messages for ACP
    , ReqSpawnShell(..)
    , ReqSpawnGroup(..)
    , ReqConnect(..)
    , ReqResources(..)
    , ReqResourcesGrp(..)
      -- * Node controller
    , nodeController
    , spawnHierachically
    , startLoggerProcess
    , __remoteTable
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable (Serializable)
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import qualified Data.Foldable   as T
import qualified Data.Set        as Set
import           Data.Set          (Set)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict   (Map)
import System.IO
import System.Directory   (createDirectoryIfMissing)
import GHC.Generics (Generic)

import DNA.Lens
import DNA.Types


----------------------------------------------------------------
-- ACP
----------------------------------------------------------------

-- | Start execution of actor controller process
startAcpLoop :: Closure (Process ()) -- ^ Closure to self
             -> ProcessId            -- ^ PID of process being monitored
             -> VirtualCAD           -- ^ Allocated nodes
             -> Process ()
startAcpLoop self pid (VirtualCAD _ n nodes) = do
    let loop s = do ms <- acpStep s
                    case ms of
                      Just s' -> loop s'
                      Nothing -> return ()
    loop StateACP { _stCounter        = 0
                  , _stAcpClosure     = self
                  , _stActor          = pid
                  , _stLocalNode      = n
                  , _stChildren       = Map.empty
                  , _stGroups         = Map.empty
                  , _stNodePool       = nodes
                  , _stAllocResources = Map.empty
                  , _stUsedResources  = Map.empty
                  }



----------------------------------------------------------------
-- Control messages
----------------------------------------------------------------

-- | Command to spawn shell actor. Contains pair of
data ReqSpawnShell = ReqSpawnShell
    (Closure (Process ()))      -- Actor's closure
    (SendPort (Maybe Message))  -- Port to send Shell to
    Resources                   -- Resources ID for an actor
    deriving (Show,Typeable,Generic)
instance Binary ReqSpawnShell


-- | Command to spawn group of shell actors
data ReqSpawnGroup = ReqSpawnGroup
    (Closure (Process ()))      -- Actor's closure
    (SendPort (Maybe (GroupID,[Message])))  -- Port to send Shell to
    [Resources]                 -- Resources ID for all actors
    deriving (Show,Typeable,Generic)
instance Binary ReqSpawnGroup


-- | Command to mark process as connected to another actor
data ReqConnect
    = ReqConnectTo       ACP     ACP
    | ReqConnectToGrp    ACP     GroupID
    | ReqConnectGrp      GroupID ACP     [SendPort (Maybe Int)]
    | ReqConnectGrpToGrp GroupID GroupID [SendPort (Maybe Int)]
    deriving (Show,Typeable,Generic)
instance Binary ReqConnect



-- | Request resourses for single process.
data ReqResources = ReqResources Location Int
                    deriving (Show,Typeable,Generic)
instance Binary ReqResources

-- | Request resourses for group of processes
data ReqResourcesGrp = ReqResourcesGrp Int
                    deriving (Show,Typeable,Generic)
instance Binary ReqResourcesGrp



----------------------------------------------------------------
-- Handling of incoming messages
----------------------------------------------------------------

-- Handle incoming messages from other processes
acpStep :: StateACP -> Process (Maybe StateACP)
acpStep st = receiveWait
    [ -- Requests for resources
      msg handleReqResources
    , msg handleReqResourcesGrp
      -- Requests for spawning children\
    , msg handleSpawnShell
    , msg handleSpawnShellGroup
    , msg handleChannelMsg
      -- Connect children
    , msg handleConnect
      -- Monitoring notifications
    , match $ \(ProcessMonitorNotification _ pid reason) -> case () of
        _| pid == st ^. stActor -> handleChildTermination reason st
         | otherwise            -> Just <$> flip execStateT st (case reason of
             DiedNormal    -> handleNormalTermination pid
             _             -> handleProcessCrash      pid
             )
    ]
  where
    msg :: Serializable a => (a -> StateT StateACP Process ()) -> Match (Maybe StateACP)
    msg handler = match $ \a -> Just <$> execStateT (handler a) st




handleReqResources :: ReqResources -> StateT StateACP Process ()
handleReqResources (ReqResources loc n) = do
    res <- Resources <$> uniqID
    -- FIXME: What to do when we don't have enough resources to
    --        satisfy request?
    resourse <- case loc of
        Remote -> do
            when (n <= 0) $ error "Positive number of nodes required"
            free <- use stNodePool
            when (length free < n) $
                error "Cannot allocate enough resources!"
            let (node:touse,rest) = splitAt n free
            stNodePool .= rest
            return $ VirtualCAD loc node touse
        Local -> do
            when (n < 0) $ error "Non-negative number of nodes required"
            free <- use stNodePool
            when (length free < n) $
                error "Cannot allocate enough resources!"
            let (touse,rest) = splitAt n free
            stNodePool .= rest
            nid <- use stLocalNode
            return $ VirtualCAD loc nid touse
    stAllocResources . at res .= Just resourse
    -- Send back reply
    pid <- use stActor
    lift $ send pid res


handleReqResourcesGrp :: ReqResourcesGrp -> StateT StateACP Process ()
handleReqResourcesGrp (ReqResourcesGrp n) = do
    when (n <= 0) $ error "Positive number of nodes required"
    ress <- replicateM n $ Resources <$> uniqID
    -- FIXME: What to do when we don't have enough resources to
    --        satisfy request?
    free <- use stNodePool
    when (length free < n) $
        error "Cannot allocate enough resources!"
    let (nodes,rest) = splitAt n free
    stNodePool .= rest
    forM_ (ress `zip` nodes) $ \(r,ni) -> do
        stAllocResources . at r .= Just (VirtualCAD Remote ni [])
    -- Send back reply
    pid <- use stActor
    lift $ send pid ress


handleConnect :: ReqConnect -> StateT StateACP Process ()
handleConnect (ReqConnectGrp gid acp port) = do
    Just grp <- use $ stGroups . at gid
    case grp of
      GroupShell{} -> error "Impossible"
      GroupState ty p _ -> stGroups . at gid .= Just (GroupState ty p (Just port))
handleConnect _ = do
    -- FIXME: for time being we ignore messages. this means we cannot
    --        act properly when process fails.
    return ()

handleSpawnShell :: ReqSpawnShell -> StateT StateACP Process ()
handleSpawnShell (ReqSpawnShell actor chShell resID) = do
    -- FIXME: better error reporting. If we reuse resource to spawn
    --        process we'll trigger bad pattern error
    Just res@(VirtualCAD _ n _) <- use $ stAllocResources . at resID
    -- Spawn remote ACP
    acpClos <- use stAcpClosure
    me      <- lift getSelfPid
    (acp,_) <- lift $ spawnSupervised (nodeID n) acpClos
    lift $ send acp $ ParamACP
        { acpSelf         = acpClos
        , acpActorClosure = actor
        , acpVCAD         = res
        , acpActor = ParamActor
            { actorParentACP = me
            , actorRank      = Rank 0
            , actorGroupSize = GroupSize 1
            }
        }
    -- Updare
    stChildren       . at acp   .= Just (Left (ShellProc chShell))
    stAllocResources . at resID .= Nothing
    stUsedResources  . at acp   .= Just res

handleSpawnShellGroup :: ReqSpawnGroup -> StateT StateACP Process ()
handleSpawnShellGroup (ReqSpawnGroup actor chShell res) = do
    -- FIXME: How to assemble a group?
    gid     <- GroupID <$> uniqID
    acpClos <- use stAcpClosure
    -- Spawn remote actors
    --
    -- FIXME: Here we require that none of nodes will fail during
    let k = length res
    forM_ ([0..] `zip` res) $ \(i, rid) -> do
        Just r@(VirtualCAD _ n _) <- use $ stAllocResources . at rid
        (acp,_) <- lift $ spawnSupervised (nodeID n) acpClos
        me <- lift getSelfPid
        lift $ send acp ParamACP
            { acpSelf         = acpClos
            , acpActorClosure = actor
            , acpVCAD  = r
            , acpActor = ParamActor
                { actorParentACP = me
                , actorRank      = Rank i
                , actorGroupSize = GroupSize k
                }
            }
        -- Update process status
        stChildren       . at acp .= Just (Right gid)
        stAllocResources . at rid .= Nothing
        stUsedResources  . at acp .= Just r
    -- FIXME: pass type of group
    stGroups . at gid .= Just
        (GroupShell NormalGroup chShell (length res) [])
--        (GroupState NormalGroup (GroupProcs (length res) 0) Nothing)



handleChildTermination :: DiedReason -> StateACP -> Process (Maybe StateACP)
-- FIXME: do someing about children if needed
handleChildTermination DiedNormal _ = return Nothing
handleChildTermination _ _ = error "Ooops!"


handleNormalTermination :: ProcessId -> StateT StateACP Process ()
handleNormalTermination pid = do
    -- Silently remove node from list of monitored nodes
    r <- use $ stChildren . at pid
    case r of
      -- In principle its possible that process terminates normally
      -- and we don't know about it. When one process in group crashes
      -- we kill all other processes and forget them. But it possible
      -- that one could terminate normally before receiving message
      Nothing -> return ()
      Just (Left ShellProc{}) -> error $ "Cannot happen single proc normal termination "
      Just (Left  _  ) -> stChildren . at pid .= Nothing
      -- Process belongs to group
      Just (Right gid) -> do
          Just g <- use $ stGroups . at gid
          case g of
            GroupShell{} -> error "Cannot happen: group normal termination"
            -- We are done
            GroupState _ (GroupProcs 1 nD) (Just ch) -> do
                lift $ forM_ ch $ \c -> sendChan c (Just (nD+1))
                stGroups . at gid .= Nothing
            GroupState _ (GroupProcs 1 nD) Nothing -> do
                stGroups . at gid .= Just (CompletedGroup (Just (nD+1)))
            -- Not yet
            GroupState ty (GroupProcs nR nD) mch -> do
                stGroups . at gid .= Just (GroupState ty (GroupProcs (nR-1) (nD+1)) mch)
            CompletedGroup _ -> error "Should not happen"
    -- Mark resources as free
    mfreed <- use $ stUsedResources . at pid
    T.forM_ mfreed $ \(VirtualCAD _ n ns) -> do
        stUsedResources %= Map.delete pid
        stNodePool      %= (\free -> n : ns ++ free)


handleProcessCrash :: ProcessId -> StateT StateACP Process ()
-- FIXME: I leave processes in stChildren dictionary when process
--        crashes. Whole data structure is VERY inelegant
--
-- FIXME: kill everything if we cannot continue
handleProcessCrash pid = do
    r <- use $ stChildren . at pid
    case r of
      -- Again we could remove child from list of known processes already
      Nothing -> return ()
      -- Single process
      Just (Left (ShellProc ch)) -> do
          lift $ sendChan ch Nothing
          stChildren . at pid .= Nothing
      Just (Left (Connected remotes)) -> do
          lift $ forM_ remotes $ \(ACP acp) -> send acp Terminate
          stChildren . at pid .= Nothing
      Just (Left _) -> do
          stChildren . at pid .= Just (Left Failed)
      -- Group of processes
      Just (Right gid) -> do
          Just g <- use $ stGroups . at gid
          case g of
            GroupShell _ ch _ _ -> do
                lift $ sendChan ch Nothing
                stGroups . at gid .= Nothing
            -- In normal group we terminate whole group!
            GroupState NormalGroup _ (Just ch) -> do
                lift $ forM_ ch $ \c -> sendChan c Nothing
                stGroups . at gid .= Nothing
            GroupState NormalGroup _ Nothing -> do
                stGroups . at gid .= Just (CompletedGroup Nothing)
            -- In failout we carry on
            GroupState FailoutGroup _ _ -> undefined
            CompletedGroup{} -> return ()


handleChannelMsg :: (ProcessId,Message) -> StateT StateACP Process ()
handleChannelMsg (pid,msg) = do
    me <- lift getSelfPid
    r <- use $ stChildren . at pid
    case r of
      Just (Left (ShellProc ch)) -> do
          lift $ sendChan ch (Just msg)
          stChildren . at pid .= Just (Left Running)
      Just (Right gid) -> do
          Just grp <- use (stGroups . at gid)
          case grp of
            GroupShell ty ch n (msgs) ->
                let k = length (msg : msgs)
                in if k == n
                   then do
                       lift $ sendChan ch (Just (gid,msg : msgs))
                       stGroups . at gid .= Just (GroupState ty (GroupProcs n 0) Nothing)
                   else do
                       stGroups . at gid .= Just (GroupShell ty ch n (msg : msgs))
            _ -> error "Cannot happen: group crash"
      -- Errors
      Nothing -> error $ "Cannot happen: unknown process " ++ show pid
      _       -> error $ "Cannot happen: single proc crash " ++ show pid



----------------------------------------------------------------
-- Internal state of node controller
----------------------------------------------------------------

data StateACP = StateACP
    { _stCounter   :: !Int
      -- Counter for generation of unique IDs
    , _stAcpClosure :: Closure (Process ())
      -- Closure of runACP
    , _stActor     :: ProcessId
      -- PID of actor process
    , _stLocalNode :: NodeInfo
      -- Node we are running on
    , _stChildren :: !(Map ProcessId (Either ProcState GroupID))
      -- State of monitored processes
    , _stGroups   :: !(Map GroupID GroupState)
      -- State of groups of processes
    , _stNodePool :: ![NodeInfo]
      -- Unused nodes which could be reused
    , _stAllocResources :: !(Map Resources VirtualCAD)
      -- Resources which have been allocated but we didn't start use
      -- them yet.
    , _stUsedResources :: !(Map ProcessId VirtualCAD)
      -- Resources used by some process
    }
    deriving (Show)

-- State of process.
data ProcState
    = ShellProc (SendPort (Maybe Message))
      -- We started process but didn't receive status update from it
    | Running
      -- Process is running but we don't know its sink yet
    | Connected [ACP]
      -- Process is running and we know its sink
    | Failed
      -- Process failed
    deriving (Show)

data GroupProcs = GroupProcs
    { grpRunning :: !Int
    , grpDone    :: !Int
    }
    deriving (Show)

-- State of group of processes
data GroupState
    = GroupShell GroupType (SendPort (Maybe (GroupID,[Message]))) Int [Message]
      -- We started processes but didn't assembled processes yet
    | GroupState GroupType GroupProcs (Maybe [SendPort (Maybe Int)])
      -- Running group. It contains following fields:
      --  + Type of group of processes
      --  + (N completed, N crashed)
      --  + channel to send information on completion
    | CompletedGroup (Maybe Int)
      -- Group which completed execution normally of abnormally
    deriving (Show)

data GroupType
    = NormalGroup
    | FailoutGroup
    deriving (Show)

stCounter :: Lens' StateACP Int
stCounter = lens _stCounter (\a x -> x { _stCounter = a})

stAcpClosure :: Lens' StateACP (Closure (Process ()))
stAcpClosure = lens _stAcpClosure (\a x -> x { _stAcpClosure = a})

stActor :: Lens' StateACP ProcessId
stActor = lens _stActor (\a x -> x { _stActor = a})

stLocalNode :: Lens' StateACP NodeInfo
stLocalNode = lens _stLocalNode (\a x -> x { _stLocalNode = a})

stChildren :: Lens' StateACP (Map ProcessId (Either ProcState GroupID))
stChildren = lens _stChildren (\a x -> x { _stChildren = a})

stGroups :: Lens' StateACP (Map GroupID GroupState)
stGroups = lens _stGroups (\a x -> x { _stGroups = a})

stNodePool :: Lens' StateACP [NodeInfo]
stNodePool = lens _stNodePool (\a x -> x { _stNodePool = a})

stAllocResources :: Lens' StateACP (Map Resources VirtualCAD)
stAllocResources = lens _stAllocResources (\a x -> x { _stAllocResources = a})

stUsedResources :: Lens' StateACP (Map ProcessId VirtualCAD)
stUsedResources = lens _stUsedResources (\a x -> x { _stUsedResources = a})




-- Generate unique ID
uniqID :: Monad m => StateT StateACP m Int
uniqID = do
    i <- use stCounter
    stCounter .= (i + 1)
    return i


----------------------------------------------------------------
-- Controller processes
----------------------------------------------------------------

-- | Node controller process
--
nodeController :: Process ()
-- We have to jump through quite elaborate hoops because we cannot
-- make closure of nodeController inside body of function. (Necessary
-- functions are only visible after remotable call). Instead we send
-- closure as message.
nodeController = do
    (self,parent,subcads) <- expect
    me     <- getSelfPid
    local  <- getSelfNode
    -- Get logger process. Here we assume that it logger is already
    -- spawned and registered. Otherwise we will block here forever.
    let getLogger = do
            mh <- whereis "dnaLogger"
            maybe getLogger return mh
    logger <- getLogger
    -- FIXME: assumes reliability. Process spaning may in fact fail
    cads <- forM subcads $ \(CAD nid rest) -> do
        pid <- spawn nid self
        send pid (self,me,rest)
        expect
    let ninfo = NodeInfo { nodeCP     = NCP me
                         -- FIXME: notion of parent.
                         , nodeParent = Just (NCP parent)
                         , nodeID     = local
                         , loggerProc = logger
                         }
    send parent $ CAD ninfo cads
    -- FIXME: here we just block eternally to keep process alive. We
    --        need to figure out whether we need this process
    () <- expect
    return ()

remotable [ 'nodeController ]


-- | Spawn all nodes controllers hierarchially
spawnHierachically :: CAD NodeId -> Process (CAD NodeInfo)
spawnHierachically (CAD nid children) = do
    me  <- getSelfPid
    let clos = $(mkStaticClosure 'nodeController)
    pid <- spawn nid clos
    send pid (clos,me,children)
    cad <- expect
    return cad


-- | Start logger process and register in local registry
startLoggerProcess :: FilePath -> Process ()
startLoggerProcess logdir = do
    liftIO $ createDirectoryIfMissing True logdir
    bracket open fini $ \h -> do
        me <- getSelfPid
        register "dnaLogger" me
        forever $ do
            s <- expect
            liftIO $ print s
            liftIO $ hPutStrLn h s
            liftIO $ hFlush h
  where
    open   = liftIO (openFile (logdir ++ "/log") WriteMode)
    fini h = liftIO (hClose h)




----------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------

data FMatch b where
    MatchChan :: Serializable a
              => ReceivePort a -> (a -> Process b) -> FMatch b

instance Functor FMatch where
    fmap f (MatchChan ch g) =
        MatchChan ch (fmap f . g)
