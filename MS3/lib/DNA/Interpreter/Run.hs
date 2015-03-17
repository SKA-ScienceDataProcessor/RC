{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ExistentialQuantification  #-}
-- | Module for interpreting
module DNA.Interpreter.Run (
      -- * Run actors
      runActor
    , runActorManyRanks
    , runCollectActor
    , runCollectActorMR
    , runMapperActor
      -- * CH
    , runActor__static
    , runActorManyRanks__static
    , runCollectActor__static
    , runCollectActorMR__static
    , runMapperActor__static
    , __remoteTable
    ) where

import Control.Monad
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Closure
import Data.Typeable (Typeable)

import DNA.Types
import DNA.DSL
import DNA.Interpreter.Types


----------------------------------------------------------------
-- Functions for starting actors
----------------------------------------------------------------

-- | Start execution of standard actor
runActor :: Actor a b -> Process ()
runActor (Actor action) = do
    -- Obtain parameters
    p <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendDst,  chRecvDst  ) <- newChan
    -- Send shell process back
    me <- getSelfPid
    send (actorParent p) $ Shell
        (SingleActor me)
        (RecvVal chSendParam)
        (SendVal chSendDst  )
    -- Now we can start execution and send back data
    a   <- receiveChan chRecvParam
    !b  <- runDnaParam p (action a)
    sendToDestChan chRecvDst b


-- | Run actor for group of processes which allow more than 1 task per
--   actor.
runActorManyRanks :: Actor a b -> Process ()
runActorManyRanks (Actor action) = do
    -- Obtain parameters
    p <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendDst,  chRecvDst  ) <- newChan
    (chSendRnk,  chRecvRnk  ) <- newChan
    -- Send shell process back
    me <- getSelfPid
    let shell = Shell (SingleActor me)
                      (RecvVal chSendParam)
                      (SendVal chSendDst  )
    send (actorParent p) shell
    -- Start actor execution
    a   <- receiveChan chRecvParam
    dst <- receiveChan chRecvDst
    let loop = do
            send (actorParent p) (me,chSendRnk)
            mrnk <- receiveChan chRecvRnk
            case mrnk of
                Nothing  -> return ()
                Just rnk -> do !b  <- runDnaParam p (action a)
                               sendToDest dst b
                               send (actorParent p{actorRank = rnk}) (me,DoneTask)
                               loop
    loop


-- | Start execution of collector actor
runCollectActor :: CollectActor a b -> Process ()
runCollectActor (CollectActor step start fini) = do
    -- Obtain parameters
    p           <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendDst,  chRecvDst  ) <- newChan
    (chSendN,    chRecvN    ) <- newChan
    -- Send shell process description back
    me <- getSelfPid
    send (actorParent p) $ Shell
        (SingleActor me)
        (RecvReduce [(chSendN,chSendParam)])
        (SendVal    chSendDst)
    -- Start execution of an actor
    --
    -- FIXME: I constrained CollectActor to IO only (no DNA) which
    --        means it cannot do anything specific to DNA.
    --
    --        Check for proper concurrency
    s0 <- liftIO start
    s  <- doGatherM (Group chRecvParam chRecvN) step s0
    !b <- liftIO $ fini s
    sendToDestChan chRecvDst b



-- | Start execution of collector actor
runCollectActorMR :: CollectActor a b -> Process ()
runCollectActorMR (CollectActor step start fini) = do
    -- Obtain parameters
    p <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendDst,  chRecvDst  ) <- newChan
    (chSendN,    chRecvN    ) <- newChan
    -- Send shell process description back
    me <- getSelfPid
    send (actorParent p) $ Shell
      (SingleActor me)
      (RecvMR    [(chSendN,chSendParam)])
      (SendVal    chSendDst)
    let loop n tot !b
          | n >= tot && tot >= 0 = return b
          | otherwise = do
              r <- liftP $ receiveWait [ matchChan chRecvParam (return . Right)
                                       , matchChan chRecvN     (return . Left)
                                       ]
              case r of
                Right (Just a) -> loop n tot =<< liftIO (step b a)
                Right Nothing  -> loop (n+1) tot b
                Left  k        -> loop n k b
    s <- loop 0 (-1) =<< liftIO start
    b <- liftIO $ fini s
    sendToDestChan chRecvDst b


runMapperActor :: Mapper a b -> Process ()
runMapperActor (Mapper start step shuffle) = do
    -- Obtain parameters
    p <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendDst,  chRecvDst  ) <- newChan
    -- Send shell process description back
    me <- getSelfPid
    send (actorParent p) $ Shell
        (SingleActor me)
        (RecvVal chSendParam)
        (SendMR [chSendDst])
    -- Get initial parameters for unfolding
    a   <- receiveChan chRecvParam
    s0  <- liftIO $ start a
    -- Get destination
    dst <- receiveChan chRecvDst
    -- Unfoldr loop
    let n = length dst
    let loop s = do
            ms <- liftIO $ step s
            case ms of
              Nothing     -> return ()
              Just (s',b) -> do let i = shuffle n b
                                sendChan (dst !! i) (Just b)
                                loop s'
    loop s0
    forM_ dst $ \ch -> sendChan ch Nothing


runDnaParam :: ActorParam -> DNA a -> Process a
runDnaParam p action = do
  interpreter <- unClosure $ actorInterpreter p
  runDnaMonad (actorRank p)
              (actorGroupSize p)
              (actorInterpreter p)
              (actorNodes       p)
    $ dnaInterpreter interpreter action

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

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


doGatherM :: Serializable a => Group a -> (b -> a -> IO b) -> b -> Process b
doGatherM (Group chA chN) f x0 = do
    let loop n tot !b
            | n >= tot && tot >= 0= do
                  return b
        loop n tot !b = do
            r <- receiveWait [ matchChan chA (return . Right)
                             , matchChan chN (return . Left)
                             ]
            case r of
              Right a -> loop (n + 1) tot =<< liftIO (f b a)
              Left  k -> loop n k b
    loop 0 (-1) x0

----------------------------------------------------------------
-- CH's TH
----------------------------------------------------------------

remotable [ 'runActor
          , 'runActorManyRanks
          , 'runCollectActor
          , 'runCollectActorMR
          , 'runMapperActor
          ]
