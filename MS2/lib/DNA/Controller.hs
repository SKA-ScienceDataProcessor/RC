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
    , ResGroup(..)
    , GrpFlag(..)
    , GroupType(..)
    , ReqSpawnShell(..)
    , ReqSpawnGroup(..)
    , ReqConnect(..)
    , ReqNumNodes(..)
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
import Data.Monoid   ((<>))
import qualified Data.Foldable   as T
import qualified Data.Set        as Set
import           Data.Set          (Set)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict   (Map)
import Text.Printf
import GHC.Generics (Generic)

import DNA.Lens
import DNA.Types
import DNA.Logging



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
              -- FIXME: checking that all children completed
              --        execution is race condition. It's possible
              --        that we didn't receive message that
              --        children completed execution. And it's
              --        very easy to trigger
              Left Done -> taggedMessage "ACP" "Done"
              Left (Fatal m) -> do
                  taggedMessage "ACP" ("Crash: " ++ m)
                  error "Terminate"
    loop StateACP { _stCounter        = 0
                  , _stAcpClosure     = self
                  , _stActor          = pid
                  , _stLocalNode      = n
                  , _stChildren       = Map.empty
                  , _stGroups         = Map.empty
                  , _stNodePool       = Set.fromList nodes
                  , _stAllocResources = Map.empty
                  , _stUsedResources  = Map.empty
                  }



----------------------------------------------------------------
-- Control messages
----------------------------------------------------------------

-- | What part of process pool is to use
data Res
    = N    Int                -- ^ Fixed number of nodes
    | Frac Double             -- ^ Fraction of nodes
    deriving (Show,Typeable,Generic)
instance Binary Res

-- | What part of process pool is to use
data ResGroup
    = NWorkers Int   -- ^ Allocate no less than N workers
    | NNodes   Int   -- ^ Allocate no less than N nodes to each worker
    deriving (Show,Typeable,Generic)
instance Binary ResGroup

-- | How process crashes should be treated in the group
data GroupType
    = Normal
    | Failout
    deriving (Show,Typeable,Generic)
instance Binary GroupType

-- | Flags for selection resources for groups
data GrpFlag
    = UseLocal                  -- ^ Use local node as well
    deriving (Show,Typeable,Generic)
instance Binary GrpFlag

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
    (SendPort (GroupID,[Message]))  -- Port to send Shell to
    [Resources]                 -- Resources ID for all actors
    GroupType                   -- 
    deriving (Show,Typeable,Generic)
instance Binary ReqSpawnGroup


-- | Command to mark process as connected to another actor
data ReqConnect
    = ReqConnectTo       ACP     ACP
    | ReqConnectToGrp    ACP     GroupID
    | ReqConnectGrp      GroupID ACP     [SendPort Int]
    | ReqConnectGrpToGrp GroupID GroupID [SendPort Int]
    deriving (Show,Typeable,Generic)
instance Binary ReqConnect

-- | Query number of available nodes
data ReqNumNodes = ReqNumNodes
    deriving (Show,Typeable,Generic)
instance Binary ReqNumNodes

-- | Request resourses for single process.
data ReqResources = ReqResources Location Res
                    deriving (Show,Typeable,Generic)
instance Binary ReqResources

-- | Request resourses for group of processes
data ReqResourcesGrp = ReqResourcesGrp Res ResGroup [GrpFlag]
                    deriving (Show,Typeable,Generic)
instance Binary ReqResourcesGrp



----------------------------------------------------------------
-- Handling of incoming messages
----------------------------------------------------------------

type Controller = StateT StateACP (ExceptT Err Process)

data Err = Fatal String
         | Done
         deriving (Show)

fatal :: MonadError Err m => String -> m a
fatal msg = throwError (Fatal msg)

-- Handle incoming messages from other processes
acpStep :: StateACP -> Process (Either Err StateACP)
acpStep st = receiveWait
    [ -- Requests for resources
      msg handleReqResources
    , msg handleReqResourcesGrp
    , msg $ \ReqNumNodes -> do
        pool <- use stNodePool
        pid  <- use stActor
        liftP $ send pid (Set.size pool)
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

-- > ReqResources
--
-- Request for resources
handleReqResources :: ReqResources -> Controller ()
handleReqResources (ReqResources loc req) = do
    res <- Resources <$> uniqID
    -- FIXME: What to do when we don't have enough resources to
    --        satisfy request?
    resourse <- case req of
        N    n -> makeResource loc =<< getNNodes n
        Frac f -> makeResource loc =<< getFracNodes f
    stAllocResources . at res .= Just resourse
    -- Send back reply
    pid <- use stActor
    liftP $ send pid res


-- > ReqResourcesGrp
--
-- Request for resources for group of processes
handleReqResourcesGrp :: ReqResourcesGrp -> Controller ()
handleReqResourcesGrp (ReqResourcesGrp req resGrp flags) = do
    -- Get nodes to allocate
    let withLocal ns
            | null [()| UseLocal <- flags] = return ns
            | otherwise                    = do n <- use stLocalNode
                                                return (n:ns)

    nodes <- withLocal =<< case req of
        N    n -> getNNodes n
        Frac f -> getFracNodes f
    -- Divide resources
    res <- case resGrp of
      NWorkers k -> do
          chunks <- toNChunks k nodes
          forM chunks $ \ns -> case ns of
              []     -> fatal "Impossible: empty nodelist"
              n:rest -> allocResource n rest
      NNodes k -> do
          chunks <- toSizedChunks k nodes
          forM chunks $ \ns -> case ns of
              []     -> fatal "Impossible: empty nodelist"
              n:rest -> allocResource n rest
    -- Send back reply
    pid <- use stActor
    liftP $ send pid res

-- Get N nodes from pool
getNNodes :: Int -> Controller [NodeInfo]
getNNodes n = do
    free <- Set.toList <$> use stNodePool
    when (length free < n) $
        fatal $ printf "Cannot allocate %i nodes" n
    let (used,rest) = splitAt n free
    stNodePool .= Set.fromList rest
    return used

-- Get fraction of free nodes from pool
getFracNodes :: Double -> Controller [NodeInfo]
getFracNodes frac = do
    free <- Set.toList <$> use stNodePool
    let n = length free
        k = round $ fromIntegral n * frac
    let (used,rest) = splitAt k free
    liftIO $ print (k,n)
    stNodePool .= Set.fromList rest
    return used

makeResource :: Location -> [NodeInfo] -> Controller VirtualCAD
makeResource Remote []     = fatal "Need positive number of nodes"
makeResource Remote (n:ns) = return (VirtualCAD Remote n ns)
makeResource Local  ns     = do
    n <- use stLocalNode
    return $ VirtualCAD Local n ns

allocResource :: NodeInfo -> [NodeInfo] -> Controller Resources
allocResource n nodes = do
    local <- use stLocalNode
    i     <- Resources <$> uniqID
    let loc | local == n = Local
            | otherwise  = Remote
    let r = VirtualCAD loc n nodes
    stAllocResources . at i .= Just r
    return i


-- Split list to N chunks
toNChunks :: MonadError Err m => Int -> [a] -> m [[a]]
toNChunks n items
    | n <= 0    = fatal "Non-positive number of chunks"
    | n >  len  = fatal "Cannot allocate enough items"
    | otherwise = return $ go size rest items
  where
    len = length items
    (size,rest) = len `divMod` n
    go _  _ [] = []
    go sz 0 xs = case splitAt  sz    xs of (as,rm) -> as : go sz 0     rm
    go sz r xs = case splitAt (sz+1) xs of (as,rm) -> as : go sz (r-1) rm


-- Split list to chunks of size N
toSizedChunks :: MonadError Err m => Int -> [a] -> m [[a]]
toSizedChunks n items
    | n <= 0    = fatal "Non-positive size of chunk"
    | n >  len  = fatal "Chunk size is too large"
    | otherwise = toNChunks (len `div` n) items
  where
    len = length items

-- > ReqConnect
--
-- Write down how processes are connected
handleConnect :: ReqConnect -> Controller ()
handleConnect (ReqConnectTo (ACP pid) dst) = do
    Just st <- use $ stChildren . at pid
    case st of
      Right _ -> fatal "Impossible: group instead of process"
      Left ShellProc{} -> fatal "Impossible: shell could not be connected"
      Left Unconnected -> stChildren . at pid .= Just (Left (Connected [dst]))
      Left _           -> fatal "Double connect"
handleConnect (ReqConnectToGrp (ACP pid) gid) = do
    Just st <- use $ stChildren . at pid
    pids    <- getGroupPids gid
    case st of
      Right _          -> fatal "Impossible: group instead of process"
      Left ShellProc{} -> fatal "Impossible: shell could not be connected"
      Left Unconnected -> stChildren . at pid .= Just (Left (Connected (map ACP pids)))
      Left _           -> fatal "Double connect"
handleConnect (ReqConnectGrp gid acp port) = do
    Just grp <- use $ stGroups . at gid
    case grp of
      GrShell{}      -> fatal "Impossible: group is still shell. Cannot connect"
      GrUnconnected ty nR ->
          stGroups . at gid .= Just (GrConnected ty (nR,0) port [acp])
      GrConnected{}  -> fatal "Double connect"
      GrFailed       -> do terminateACP acp
                           dropGroup gid
handleConnect (ReqConnectGrpToGrp gid dstGid port) = do
    pids <- getGroupPids dstGid
    Just grp <- use $ stGroups . at gid
    case grp of
      GrShell{}      -> fatal "Impossible: group is still shell. Cannot connect"
      GrUnconnected ty nR ->
          stGroups . at gid .= Just (GrConnected ty (nR,0) port (map ACP pids))
      GrConnected{} -> fatal "Double connect"
      GrFailed      -> do forM_ pids $ \p -> terminateACP (ACP p)
                          dropGroup gid


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
handleSpawnShellGroup (ReqSpawnGroup actor chShell res groupTy) = do
    gid     <- GroupID <$> uniqID
    acpClos <- use stAcpClosure
    -- Spawn remote actors
    --
    -- FIXME: Here we require that none of nodes will fail while we
    -- creating processes
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
        (GrShell groupTy chShell (length res) [])


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
           GrShell{}       -> fatal "Impossible: Normal termination in shell group"
           GrUnconnected{} -> fatal "Impossible: Unconnected process in group terminated normally"
           GrConnected _ (1, nD) ch _ -> do
               liftP $ forM_ ch $ \c -> sendChan c (nD + 1)
               return Nothing
           GrConnected ty (nR, nD) ch acps -> do
               return $ Just $ GrConnected ty (nR-1, nD+1) ch acps
           GrFailed -> fatal "Impossible: Process terminated in complete group"
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
           -- When process from which we didn't receive channels
           -- crashes we have no other recourse but to terminate.
           ShellProc _  -> fatal "Shell crashed. No other thing to do"
           Unconnected  -> return $ Just Failed
           Connected acps -> do
               lift $ forM_ acps terminateACP
               return Nothing
           Failed       -> fatal "Impossible: Process crashed twice"
        )
        (\g gid -> case g of
           GrShell Normal _ _ _ -> do
               terminateGroup gid
               fatal "Shell group crashed. Cannot do anything"
           GrShell Failout ch n msgs ->
               return $ Just $ GrShell Failout ch (n-1) msgs
           -- Normal groups
           GrUnconnected Normal _ -> do
               terminateGroup gid
               return $ Just $ GrFailed
           GrConnected Normal _ _ acps -> do
               terminateGroup gid
               forM_ acps terminateACP
               return Nothing
           -- Failout groups
           GrUnconnected Failout n -> do
               return $ Just $ GrUnconnected Failout (n-1)
           GrConnected Failout (1, nD) ch _ -> do
               liftP $ forM_ ch $ \c -> sendChan c nD
               return Nothing
           GrConnected Failout (nR, nD) ch acps -> do
               return $ Just $ GrConnected Failout (nR-1,nD) ch acps
           GrFailed -> return $ Just GrFailed
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
               liftP $ sendChan ch msg
               return $ Just Unconnected
           _ -> fatal "Shell received twice"
        )
        (\g gid -> case g of
           GrShell ty ch nR msgs -> case msg : msgs of
               msgs' | length msgs' == nR -> do
                           liftP $ sendChan ch (gid, msgs')
                           return $ Just $ GrUnconnected ty nR
                     | otherwise -> do
                           return $ Just $ GrShell ty ch nR msgs'
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
    -> (ProcState  -> Controller (Maybe ProcState))
    -- What to do with single process
    -> (GroupState -> GroupID -> Controller (Maybe GroupState))
    -- What to do with group of processes
    -> StateT StateACP (ExceptT Err Process) ()
handlePidEvent pid none onProc onGrp = do
    r <- use $ stChildren . at pid
    case r of
      Nothing          -> none
      Just (Left  p  ) -> do mp' <- onProc p
                             case mp' of
                               Nothing -> dropPID pid
                               Just p' -> stChildren . at pid .= Just (Left p')
      Just (Right gid) -> do Just g <- use $ stGroups . at gid
                             mg' <- onGrp g gid
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
      Just (VirtualCAD Local  _ ns) -> stNodePool %= (Set.fromList ns <>)
      Just (VirtualCAD Remote n ns) -> stNodePool %= (\xs -> Set.singleton n <> Set.fromList ns <> xs)


-- Remove GID from process registry
dropGroup :: GroupID -> Controller ()
dropGroup gid = do
    stGroups . at gid .= Nothing
    children <- use stChildren
    let pids = [p | (p,Right gid') <- Map.toList children
                  , gid' == gid
                  ]
    forM_ pids dropPID

getGroupPids :: GroupID -> Controller [ProcessId]
getGroupPids gid = do
    ch <- use stChildren
    return [ pid | (pid, Right g) <- Map.toList ch
                 , g == gid
                 ]

terminateGroup :: GroupID -> Controller ()
terminateGroup gid = do
    pids <- getGroupPids gid
    forM_ pids $ \p -> terminateACP (ACP p)


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
    , _stNodePool :: !(Set NodeInfo)
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
    = GrShell GroupType (SendPort (GroupID,[Message])) Int [Message]
      -- We started processes but didn't assembled processes yet
      --  + Type of group
      --  + Port to send resulting shell
      --  + N running processes
      --  + Accumulated shells
    | GrUnconnected GroupType Int
      -- Group which is not connected yet
      --  + Type of group
      --  + N running processes
    | GrConnected GroupType (Int,Int) [SendPort Int] [ACP]
      -- Connected group
      --  + Type of group
      --  + (N running, N completed)
      --  + Destinations
    | GrFailed
      -- Group which crashed
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

stNodePool :: Lens' StateACP (Set NodeInfo)
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
