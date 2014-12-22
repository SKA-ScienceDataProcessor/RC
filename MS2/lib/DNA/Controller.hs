
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
-- | Controller processes for node and actor
module DNA.Controller (
      -- * Interaction with remote ACP
      ACP(..)
    , terminateACP
      -- * Starting ACP
    , startAcpLoop
      -- * Control messages for ACP
    , Res(..)
    , ReqSpawnShell(..)
    , ReqSpawnGroup(..)
    , ReqConnect(..)
    , ReqResources(..)
    , ReqResourcesGrp(..)
      -- * Node controller
    , nodeController
    , spawnHierachically
    , __remoteTable
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
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
import Text.Printf
import GHC.Generics (Generic)

import DNA.Lens
import DNA.Types



----------------------------------------------------------------
-- Interaction with 
----------------------------------------------------------------

-- | Send command to remote ACP to terminate abnormally
terminateACP :: MonadProcess m => ACP -> m ()
terminateACP (ACP pid) = liftP $ do
  send pid Terminate


-- | Command for ACP to terminate
data Terminate = Terminate
                deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary Terminate


----------------------------------------------------------------
-- ACP implementation
----------------------------------------------------------------

-- | Start execution of actor controller process
startAcpLoop :: Closure (Process ()) -- ^ Closure to self
             -> ProcessId            -- ^ PID of process being monitored
             -> VirtualCAD           -- ^ Allocated nodes
             -> Process ()
startAcpLoop self pid (VirtualCAD _ n nodes) = do
    let loop s = do
            ms <- acpStep s
            case ms of
              Right s' -> loop s'
              -- FIXME: check status of child processes
              -- FIXME: log status of actors?
              Left Done -> return ()
                  -- send (loggerProc n) =<< makeLogMessage "ACP" "DONE"
              Left (Fatal s) -> return ()
                  -- send (loggerProc n) =<< makeLogMessage "ACP" ("Failure: "++s)
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

-- | What part of process pool is to use
data Res
    = NNodes Int                -- ^ Fixed number of nodes
    | NFrac  Double             -- ^ Fraction of nodes
    deriving (Show,Typeable,Generic)
instance Binary Res


-- | Command to spawn shell actor. Contains pair of
data ReqSpawnShell = ReqSpawnShell
    (Closure (Process ()))      -- Actor's closure
    (SendPort Message)          -- Port to send wrapped Shell to
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
data ReqResources = ReqResources Location Res
                    deriving (Show,Typeable,Generic)
instance Binary ReqResources

-- | Request resourses for group of processes
data ReqResourcesGrp = ReqResourcesGrp Res
                    deriving (Show,Typeable,Generic)
instance Binary ReqResourcesGrp



----------------------------------------------------------------
-- Handling of incoming messages
----------------------------------------------------------------

type Controller = StateT StateACP (ExceptT Err Process)

data Err = Fatal String
         | Done

fatal :: MonadError Err m => String -> m a
fatal msg = throwError (Fatal msg)

-- Handle incoming messages from other processes
acpStep :: StateACP -> Process (Either Err StateACP)
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
         | otherwise            -> run $ case reason of
             DiedNormal    -> handleNormalTermination pid
             _             -> handleProcessCrash      pid
    , match $ \Terminate -> do
        return $ Left $ Fatal "Terminated by request!"
    ]
  where
    run :: Controller a -> Process (Either Err StateACP)
    run action = runExceptT $ execStateT action st
    msg :: Serializable a => (a -> Controller ()) -> Match (Either Err StateACP)
    msg handler = match $ run . handler



----------------------------------------------------------------
-- Handlers for individual messages
----------------------------------------------------------------

-- Get N nodes from pool
getNNodes :: Int -> Controller [NodeInfo]
getNNodes n = do
    free <- use stNodePool
    when (length free < n) $
        fatal $ printf "Cannot allocate %i nodes" n
    let (used,rest) = splitAt n free
    stNodePool .= rest
    return used

-- Get fraction of free nodes from pool
getFracNodes :: Double -> Controller [NodeInfo]
getFracNodes frac = do
    free <- use stNodePool
    let n = length free
        k = round $ fromIntegral n * frac
    let (used,rest) = splitAt k free
    liftIO $ print (k,n)
    stNodePool .= rest
    return used


makeResource :: Location -> [NodeInfo] -> Controller VirtualCAD
makeResource Remote []     = fatal "Need positive number of nodes"
makeResource Remote (n:ns) = return (VirtualCAD Remote n ns)
makeResource Local  ns     = do
    n <- use stLocalNode
    return $ VirtualCAD Local n ns

-- > ReqResources
--
-- Request for resources
handleReqResources :: ReqResources -> Controller ()
handleReqResources (ReqResources loc req) = do
    res <- Resources <$> uniqID
    -- FIXME: What to do when we don't have enough resources to
    --        satisfy request?
    resourse <- case req of
        NNodes n -> makeResource loc =<< getNNodes n
        NFrac  f -> makeResource loc =<< getFracNodes f
    stAllocResources . at res .= Just resourse
    -- Send back reply
    pid <- use stActor
    liftP $ send pid res


-- > ReqResourcesGrp
--
-- Request for resources for group of processes
handleReqResourcesGrp :: ReqResourcesGrp -> Controller ()
handleReqResourcesGrp (ReqResourcesGrp req) = do
    -- Allocate resources
    nodes <- case req of
        NNodes n -> getNNodes n
        NFrac  f -> getFracNodes f
    res <- forM nodes $ \n -> do
        let r = VirtualCAD Remote n []
        i <- Resources <$> uniqID
        stAllocResources . at i .= Just r
        return i
    -- Send back reply
    pid <- use stActor
    liftP $ send pid (res :: [Resources])


-- > ReqConnect
--
-- Write down how processes are connected
handleConnect :: ReqConnect -> Controller ()
-- FIXME: understand what to do when connecting to group
handleConnect (ReqConnectTo (ACP pid) dst) = do
    Just st <- use $ stChildren . at pid
    case st of
      Right _ -> fatal "Impossible: group instead of process"
      Left ShellProc{} -> fatal "Impossible: shell could not be connected"
      Left Unconnected -> stChildren . at pid .= Just (Left (Connected [dst]))
      Left _           -> fatal "Double connect"
handleConnect (ReqConnectToGrp (ACP pid) gid) = do
    Just st <- use $ stChildren . at pid
    Just gr <- use $ stGroups   . at gid
    case st of
      Right _          -> fatal "Impossible: group instead of process"
      Left ShellProc{} -> fatal "Impossible: shell could not be connected"
      Left Unconnected -> undefined
      Left _           -> fatal "Double connect"
handleConnect (ReqConnectGrp gid _ port) = do
    Just grp <- use $ stGroups . at gid
    case grp of
      GroupShell{}      -> fatal "Impossible: group is still shell. Cannot connect"
      GroupState ty p _ -> stGroups . at gid .= Just (GroupState ty p (Just port))
      CompletedGroup{}  -> fatal "Cannot connect group which already completed"
handleConnect (ReqConnectGrpToGrp _ _ _) = undefined



-- > ReqSpawnShell
--
-- Spawn single shell process
handleSpawnShell :: ReqSpawnShell -> Controller ()
handleSpawnShell (ReqSpawnShell actor chShell resID) = do
    -- FIXME: better error reporting. If we reuse resource to spawn
    --        process we'll trigger bad pattern error
    Just res@(VirtualCAD _ n _) <- use $ stAllocResources . at resID
    -- Spawn remote ACP
    acpClos <- use stAcpClosure
    me      <- liftP getSelfPid
    (acp,_) <- liftP $ spawnSupervised (nodeID n) acpClos
    liftP $ send acp $ ParamACP
        { acpSelf         = acpClos
        , acpActorClosure = actor
        , acpVCAD         = res
        , acpActor = ParamActor
            { actorParentACP = me
            , actorRank      = Rank 0
            , actorGroupSize = GroupSize 1
            }
        }
    -- Update
    stChildren       . at acp   .= Just (Left (ShellProc chShell))
    stAllocResources . at resID .= Nothing
    stUsedResources  . at acp   .= Just res


-- > ReqSpawnGroup
--
-- Spawn group of processes
handleSpawnShellGroup :: ReqSpawnGroup -> Controller ()
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
        (acp,_) <- liftP $ spawnSupervised (nodeID n) acpClos
        me <- liftP getSelfPid
        liftP $ send acp ParamACP
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



-- > ProcessMonitorNotification
--
-- Controlled actor died for some reason
handleChildTermination :: DiedReason -> StateACP -> Process (Either Err StateACP)
-- FIXME: do someing about children if needed
handleChildTermination DiedNormal _ = return $ Left Done
handleChildTermination reason     _ = return $ Left $ Fatal $ show reason


-- > ProcessMonitorNotification
--
-- Child process terminated normally
handleNormalTermination :: ProcessId -> Controller ()
handleNormalTermination pid = do
    handlePidEvent pid
        -- In principle its possible that process terminates normally
        -- and we don't know about it. When one process in group crashes
        -- we kill all other processes and forget them. But it possible
        -- that one could terminate normally before receiving message
        (return ())
        (\p -> case p of
           ShellProc _ -> fatal "Impossible: shell process terminated normally"
           Unconnected -> fatal "Impossible: Unconnected process terminated normally"
           Connected _ -> return Nothing
           Failed      -> fatal "Impossible: Normal termination after crash"
        )
        (\g _ -> case g of
           GroupShell{}           -> fatal "Impossible: Normal termination in shell group"
           GroupState _ _ Nothing -> fatal "Impossible: Unconnected process in group terminated normally"
           GroupState _ (1, nD) (Just ch) -> do
               lift $ forM_ ch $ \c -> sendChan c (Just (nD + 1))
               return Nothing
           GroupState ty (nR, nD) mch -> do
               return $ Just (GroupState ty (nR-1, nD+1) mch)
           CompletedGroup{} -> fatal "Impossible: Process terminated in complete group"
        )
    dropPID pid


-- > ProcessMonitorNotification
--
-- Child process died because of exception
handleProcessCrash :: ProcessId -> Controller ()
handleProcessCrash pid = do
    handlePidEvent pid
        (return ())
        (\p -> case p of
           -- FIXME: we should notify someone probably...
           ShellProc _  -> undefined
           Unconnected  -> return $ Just Failed
           Connected acps -> do
               lift $ forM_ acps terminateACP
               return Nothing
           Failed       -> fatal "Impossible: Process crashed twice"
        )
        (\g _ -> case g of
           -- FIXME: What to do when shell process in group crashes?
           GroupShell{} -> return Nothing
           -- Normal groups
           GroupState NormalGroup _ (Just ch) -> do
               lift $ forM_ ch $ \c -> sendChan c Nothing
               return Nothing
           GroupState NormalGroup _ Nothing -> do
               return $ Just $ CompletedGroup Nothing
           -- Failout groups
           GroupState FailoutGroup (1, nD) (Just ch) -> do
               lift $ forM_ ch $ \c -> sendChan c (Just nD)
               return Nothing
           GroupState FailoutGroup (1, nD) Nothing -> do
               return $ Just $ CompletedGroup (Just nD)
           GroupState FailoutGroup (nR, nD) mch -> do
               return $ Just $ GroupState FailoutGroup (nR-1, nD) mch
           -- Completed group
           CompletedGroup {} -> return (Just g)
        )
    dropPID pid


-- > (ProcessId,Message)
--
-- Child process sent shell process back.
handleChannelMsg :: (ProcessId,Message) -> Controller ()
handleChannelMsg (pid,msg) = do
    handlePidEvent pid
        (fatal "Shell: unknown process")
        (\p -> case p of
           ShellProc ch -> do
               lift $ sendChan ch msg
               return $ Just Unconnected
           Failed -> return $ Just Failed
           _      -> fatal "Shell received twice"
        )
        (\g gid -> case g of
           GroupShell ty ch 1 msgs -> do
               let msgs' = msg : msgs
               lift $ sendChan ch (Just (gid, msgs'))
               return $ Just $ GroupState ty (length msgs', 0) Nothing
           GroupShell ty ch n msgs -> do
               return $ Just $ GroupShell ty ch (n-1) (msg : msgs)
           _ -> fatal "Invalid shell for group is received"
        )


----------------------------------------------------------------
-- Operations on internal state
----------------------------------------------------------------

-- Process event where we dispatch on PID of process
handlePidEvent
    :: ProcessId
    -> (Controller ())
    -- What to do when PID not found
    -> (ProcState  -> ExceptT Err Process (Maybe ProcState))
    -- What to do with single process
    -> (GroupState -> GroupID -> ExceptT Err Process (Maybe GroupState))
    -- What to do with group of processes
    -> StateT StateACP (ExceptT Err Process) ()
handlePidEvent pid none onProc onGrp = do
    r <- use $ stChildren . at pid
    case r of
      Nothing          -> none
      Just (Left  p  ) -> do mp' <- lift $ onProc p
                             case mp' of
                               Nothing -> dropPID pid
                               Just p' -> stChildren . at pid .= Just (Left p')
      Just (Right gid) -> do Just g <- use $ stGroups . at gid
                             mg' <- lift $ onGrp g gid
                             case mg' of
                               Nothing -> dropGroup gid
                               Just g' -> stGroups . at gid .= Just g'


-- Remove PID from process registry
dropPID :: ProcessId -> Controller ()
dropPID pid = do
    stChildren . at pid .= Nothing
    mr <- use $ stUsedResources . at pid
    case mr of
      Nothing -> return ()
      Just (VirtualCAD Local  _ ns) -> stNodePool %= (ns ++)
      Just (VirtualCAD Remote n ns) -> stNodePool %= (\xs -> n : (ns ++ xs))


-- Remove GID from process registry
dropGroup :: GroupID -> Controller ()
dropGroup gid = do
    stGroups . at gid .= Nothing
    children <- use stChildren
    let pids = [p | (p,Right gid') <- Map.toList children
                  , gid' == gid
                  ]
    forM_ pids dropPID



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
    = ShellProc (SendPort Message)
      -- We started process but didn't receive status update from it
    | Unconnected
      -- Process is running but we don't know its sink yet
    | Connected [ACP]
      -- Process is running and we know its sink[s]
    | Failed
      -- Process failed
    deriving (Show)


-- State of group of processes
data GroupState
    = GroupShell GroupType (SendPort (Maybe (GroupID,[Message]))) Int [Message]
      -- We started processes but didn't assembled processes yet
    | GroupState GroupType (Int,Int) (Maybe [SendPort (Maybe Int)])
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
    -- FIXME: assumes reliability. Process spaning may in fact fail
    cads <- forM subcads $ \(CAD nid rest) -> do
        pid <- spawn nid self
        send pid (self,me,rest)
        expect
    let ninfo = NodeInfo { nodeCP     = NCP me
                         -- FIXME: notion of parent.
                         , nodeParent = Just (NCP parent)
                         , nodeID     = local
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
