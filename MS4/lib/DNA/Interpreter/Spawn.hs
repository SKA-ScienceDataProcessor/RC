{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Code for starting remote actors
module DNA.Interpreter.Spawn (
      -- * Spawning actors
      execSpawnActor
    , execSpawnCollector
    , execSpawnGroup
    -- , execSpawnGroupN
    , execSpawnCollectorGroup
    , execSpawnCollectorTree
    , execSpawnCollectorTreeGroup
      -- * Workers
    , spawnSingleActor
    ) where

import Control.Applicative
import Control.Concurrent  (threadDelay)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Distributed.Static  (closureApply)
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Closure
import Data.Monoid
import Data.Either
import qualified Data.Foldable as T
import qualified Data.Set as Set
import Text.Printf

-- import DNA.CH
import DNA.Types
import DNA.Lens
import DNA.DSL
import DNA.Logging
import DNA.Interpreter.Types
import DNA.Interpreter.Run


----------------------------------------------------------------
-- Functions for spawning actors
----------------------------------------------------------------

-- FIXME: One problem that should be addressed is need to restart
--        processes when they fail.

-- | Spawn simple actor on remote node
execSpawnActor
    :: (Serializable a, Serializable b)
    => Res
    -> Spawn (Closure (Actor a b))
    -> DnaMonad (Shell (Val a) (Val b))
-- BLOCKING
execSpawnActor res actorCmd = do
    let (act,flags) = runSpawn
                    $ closureApply $(mkStaticClosure 'runActor) <$> actorCmd
    aid <- AID <$> uniqID
    cad <- acquireResources res flags
    spawnSingleActor aid cad
      $ SpawnSingle act (Rank 0) (GroupSize 1) SimpleActor RcvTySimple flags
    return $ Shell aid

-- | Spawn collector actor on remote node
execSpawnCollector
    :: (Serializable a, Serializable b)
    => Res
    -> Spawn (Closure (CollectActor a b))
    -> DnaMonad (Shell (Grp a) (Val b))
-- BLOCKING
execSpawnCollector res actorCmd = do
    let (act,flags) = runSpawn
                    $ closureApply $(mkStaticClosure 'runCollectActor) <$> actorCmd
    aid <- AID <$> uniqID
    cad <- acquireResources res flags
    spawnSingleActor aid cad 
      $ SpawnSingle act (Rank 0) (GroupSize 1) SimpleActor RcvTyReduce flags
    return $ Shell aid


-- | Spawn group of normal processes
execSpawnGroup
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Spawn (Closure (Actor a b))
    -> DnaMonad (Shell (Scatter a) (Grp b))
-- BLOCKING
execSpawnGroup res resG act = do
    -- Spawn actors
    aid <- spawnActorGroup res resG RcvTySimple ActorGroup
         $ closureApply $(mkStaticClosure 'runActor) <$> act
    return $ Shell aid

{-
execSpawnGroupN
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Int
    -> Spawn (Closure (Actor a b))
    -> DnaMonad (Shell (Val a) (Grp b))
execSpawnGroupN res resG _n act = do
    -- Spawn actors
    (k,gid) <- spawnActorGroup res resG
             $ closureApply $(mkStaticClosure 'runActorManyRanks) <$> act
    -- Assemble group
    -- FIXME: Fault tolerance
    msgs <- replicateM k $ handleRecieve messageHandlers [matchMsg']
    let (chN,shells) = unzip msgs
    stPooledProcs . at gid .= Just chN
    return $ broadcast $ assembleShellGroup gid shells
-}

-- |
execSpawnCollectorGroup
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Spawn (Closure (CollectActor a b))
    -> DnaMonad (Shell (Grp a) (Grp b))
execSpawnCollectorGroup res resG act = do
    -- Spawn actors
    aid <- spawnActorGroup res resG RcvTyReduce ActorGroup
         $ closureApply $(mkStaticClosure 'runCollectActor) <$> act
    return $ Shell aid


execSpawnCollectorTree
    :: (Serializable a)
    => Spawn (Closure (TreeCollector a))
    -> DnaMonad (Shell (Grp a) (Val a))
execSpawnCollectorTree actorCmd = do
    let (act,flags) = runSpawn
                    $ closureApply $(mkStaticClosure 'runTreeActor) <$> actorCmd
        res = if UseLocal `elem` flags then N 0 else N 1
    aid <- AID <$> uniqID
    cad <- acquireResources res flags
    spawnSingleActor aid cad
      $ SpawnSingle act (Rank 0) (GroupSize 1) SimpleActor RcvTyTree flags
    return $ Shell aid

execSpawnCollectorTreeGroup
    :: (Serializable a)
    => Res
    -> Spawn (Closure (TreeCollector a))
    -> DnaMonad (Shell (Grp a) (Grp a))
execSpawnCollectorTreeGroup res act = do
    aid <- spawnActorGroup res (NWorkers 1) RcvTyTree ActorTree
         $ closureApply $(mkStaticClosure 'runTreeActor) <$> act
    return $ Shell aid


----------------------------------------------------------------
-- Spawn helpers
----------------------------------------------------------------

-- Acquire resources for single actor
acquireResources :: Res -> [SpawnFlag] -> DnaMonad VirtualCAD
acquireResources res flags = do
    runController
        $ makeResource (if UseLocal `elem` flags then Local else Remote)
      =<< requestResources res

-- Spawn actor which only uses single CH process. It doesn't record
-- actor into `stActors'
spawnSingleActor
    :: AID
    -> VirtualCAD
    -> SpawnCmd
    -> DnaMonad ()
spawnSingleActor aid cad cmd@(SpawnSingle act rnk grp actDescr addrTy flags) = do
    -- Spawn process and receive recv. address back
    (pid,_) <- liftP $ spawnSupervised (nodeId $ vcadNode cad) act
    (chSend,chRecv) <- liftP newChan
    _       <- sendActorParam
                 pid aid rnk grp
                 cad chSend (concat [fs | UseDebug fs <- flags])
    maddr   <- receiveShell chRecv aid pid $ \dst -> case (dst,addrTy) of
        (RcvSimple{},RcvTySimple) -> return ()
        (RcvReduce{},RcvTyReduce) -> return ()
        (RcvGrp{}   ,RcvTyGrp   ) -> return ()
        (RcvTree{}  ,RcvTyTree  ) -> return ()
        _           -> doPanic "Invalid RecvAddr in execSpawnGroup"
    -- Add timeout for actor
    liftP $ setTimeout flags aid
    -- Record actor
    stActors . at aid .= Just actDescr
    case maddr of
      Nothing ->
          stActorState . at aid .= Just Failed
      Just addr -> do
          stPid2Aid    . at pid .= Just aid
          stActorState . at aid .= Just 
              (Running $ ProcInfo
                 { _pinfoPID      = pid
                 , _pinfoRecvAddr = addr
                 , _pinfoNodes    = cad
                 , _pinfoClosure  = if UseRespawn `elem` flags then Just cmd else Nothing
                 })
    logSpawn pid aid cad

-- Spawn group of actors
spawnActorGroup
    :: Res                          -- Resourses allocated to group
    -> ResGroup                     -- How to split resources between actors
    -> RecvAddrType                 -- 
    -> ([AID] -> ActorHier)         -- 
    -> Spawn (Closure (Process ())) -- Closure to process'
    -> DnaMonad AID
spawnActorGroup res resG addrTy actDescr spwn = do
    let (act,flags) = runSpawn spwn
    -- Acquire resources
    rs <- runController
         $ splitResources resG
       =<< addLocal flags
       =<< requestResources res
    let k = length rs
    -- Spawn actors
    aid      <- AID <$> uniqID
    children <- forM ([0..] `zip` rs) $ \(rnk,cad) -> do
        a <- AID <$> uniqID
        spawnSingleActor a cad
          $ SpawnSingle act (Rank rnk) (GroupSize k) (GroupMember aid) addrTy flags
        return a
    -- Record compound actor
    stActors . at aid .= Just (actDescr children)
    nFailed <- do
        ns <- forM children $ \a -> do
            st <- use $ stActorState . at a
            case st of
              Just Failed -> return 1
              _           -> return 0
        return $! sum ns
    case (if UseFailout `elem` flags then k else 0) - nFailed of
      n | n < 0     -> do terminateActor aid
                          forM_ children $ \a ->
                              stActorState . at a .= Just Failed
                          stActorState . at aid .= Just Failed
        | otherwise -> do stActorState . at aid .= Just (GrpRunning n)
                          liftP $ setTimeout flags aid
    --
    return aid


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- Receive either value from process or termination notification
waitForShell
    :: Serializable a
    => ReceivePort a
    -> (ProcessId -> Bool)
    -> Process (Either ProcessMonitorNotification a)
waitForShell ch p = receiveWait
    [ matchChan ch                                             (return . Right)
    , matchIf (\(ProcessMonitorNotification _ pid _) -> p pid) (return . Left)
    ]


-- Receive shell process for single process actor and either record
-- failure or success in the dataflow graph
receiveShell
    :: ReceivePort (RecvAddr Recv)
    -> AID
    -> ProcessId
    -> (RecvAddr Recv -> DnaMonad ())
    -> DnaMonad (Maybe (RecvAddr Recv))
receiveShell ch aid pid handler = do
    -- We obtain shell or notification that process is dead
    r <- liftP $ waitForShell ch (==pid)
    case r of
      Left err -> do
        errorMsg (show err)
        return Nothing
      Right dst -> do
        handler dst
        return (Just dst)


-- Create process for forcing timeout when process
setTimeout :: [SpawnFlag] -> AID -> Process ()
setTimeout flags aid = do
    me <- getSelfPid
    T.forM_ (getTimeoutInterval flags) $ \t -> spawnLocal $ do
        link me
        liftIO $ threadDelay $ round $ t * 1e6
        send me (Timeout aid)

getTimeoutInterval :: [SpawnFlag] -> Maybe Double
getTimeoutInterval = getLast . T.foldMap (Last . go)
  where go (UseTimeout t) = Just t
        go _              = Nothing


-- Send auxiliary parameters to an actor
sendActorParam
    :: ProcessId
    -> AID
    -> Rank
    -> GroupSize
    -> VirtualCAD
    -> SendPort (RecvAddr Recv)
    -> [DebugFlag]
    -> DnaMonad ActorParam
sendActorParam pid aid rnk g cad ch flags = do
    me      <- liftP getSelfPid
    interp  <- envInterpreter <$> ask
    workDir <- envWorkDir <$> ask
    logopt  <- use stLogOpt
    let logopt' =
            ( case logOptDebugPrint logopt of
                DebugPrintEnabled -> \l -> l { logOptDebugPrint = NoDebugPrint }
                _                 -> id
            ) $
            ( case [ f | EnableDebugPrint f <- flags ] of
                []      -> id
                True:_  -> \l -> l { logOptDebugPrint = DebugPrintInherited }
                False:_ -> \l -> l { logOptDebugPrint = DebugPrintEnabled   }
            )
            logopt
    let p = ActorParam
            { actorParent      = Just me
            , actorInterpreter = interp
            , actorRank        = rnk
            , actorGroupSize   = g
            , actorNodes       = cad
            , actorDebugFlags  = flags
            , actorSendBack    = ch
            , actorLogOpt      = logopt'
            , actorWorkDir     = workDir
            }
    liftP $ send pid p
    return p



----------------------------------------------------------------
-- Resource allocation
----------------------------------------------------------------

-- Allocate list of resources for actor/actors
requestResources :: Res -> Controller [NodeInfo]
requestResources r = do
    free <- Set.toList <$> use stNodePool
    taggedMessage "DNA" $ "Req:  " ++ show r
    taggedMessage "DNA" $ "Pool: " ++ show free
    case r of
     N n -> do
        when (length free < n) $
            fatal $ printf "Cannot allocate %i nodes" n
        returnNodes $ splitAt n free
     Frac frac -> do
        let n = length free
            k = round $ fromIntegral n * frac
        returnNodes $ splitAt k free
  where
    returnNodes (used,rest) = do
        taggedMessage "DNA" $ "Returned: " ++ show used
        taggedMessage "DNA" $ "Remains:  " ++ show rest
        stNodePool .= Set.fromList rest
        return used

-- Create virtual CAD for single actor
makeResource :: Location -> [NodeInfo] -> Controller VirtualCAD
makeResource Remote []     = fatal "Need positive number of nodes"
makeResource Remote (n:ns) = return (VirtualCAD n ns)
makeResource Local  ns     = do
    n <- liftP getSelfNode
    return $ VirtualCAD (NodeInfo n) ns

-- Add local node to the list of nodes if needed
addLocal :: [SpawnFlag] -> [NodeInfo] -> Controller [NodeInfo]
addLocal flags nodes
  | UseLocal `elem` flags = do
        n <- liftP getSelfNode
        return $ NodeInfo n : nodes
  | otherwise             = return nodes

-- Split resources for multiple actors
splitResources :: ResGroup -> [NodeInfo] -> Controller [VirtualCAD]
splitResources resG nodes = case resG of
    NWorkers k -> do
        chunks <- toNChunks k nodes
        forM chunks $ \ns -> case ns of
            []     -> fatal "Impossible: empty nodelist"
            n:rest -> return $ VirtualCAD n rest
    NNodes k -> do
        when (length nodes < k) $
          fatal "Not enough nodes to schedule"
        chunks <- toSizedChunks k nodes
        forM chunks $ \ns -> case ns of
            []     -> fatal "Impossible: empty nodelist"
            n:rest -> return $ VirtualCAD n rest



-- Split list to N chunks
toNChunks :: MonadError DnaError m => Int -> [a] -> m [[a]]
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
toSizedChunks :: MonadError DnaError m => Int -> [a] -> m [[a]]
toSizedChunks n items
    | n <= 0    = fatal "Non-positive size of chunk"
    | n >  len  = fatal "Chunk size is too large"
    | otherwise = toNChunks (len `div` n) items
  where
    len = length items
