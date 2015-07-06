{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
-- |
-- Functions which execute DNA routines on remote nodes. Below is
-- general algorithms:
--
--  1. Obtain auxiliary parameters ('ActorParam')
--
--  2. Send receive address back to parent ('RecvAddr')
--
--  3. Receive initial parameters and execute program
--
--  4. Send result to latest destination received
--
-- One particular problem is related to the need of respawning of
-- processes. We must ensure that actor sends its result to real actor
-- instead of just terminated actor
module DNA.Interpreter.Run (
      -- * Run actors
      runActor
    -- , runActorManyRanks
    , runCollectActor
    , runTreeActor
      -- * Helpers
    , doGatherDna
    , splitEvenly
      -- * CH
    , runActor__static
    -- , runActorManyRanks__static
    , runCollectActor__static
    , runTreeActor__static
    , __remoteTable
    ) where

import Control.Monad
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Closure
import Data.Typeable (Typeable)
import Data.List (findIndex)
import qualified Data.Foldable as T

import DNA.CH
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
    -- Send data destination back
    sendChan (actorSendBack p)
             ( RcvSimple (wrapMessage chSendParam)
             , [sendPortId chSendParam]
             )
    -- Now we can start execution and send back data
    a   <- receiveChan chRecvParam
    !b  <- runDnaParam p (action a)
    sendResult p b


{-
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
    sendChan (actorSendBack p)
             (RcvSimple (wrapMessage chSendParam))
    -- let shell = ( RecvVal chSendParam
    --             , SendVal chSendDst  )
    -- send (actorParent p) (chSendRnk,shell)
    -- -- Start actor execution
    -- a   <- receiveChan chRecvParam
    -- let loop dst = do
    --         send (actorParent p) (me,chSendRnk)
    --         mrnk <- receiveChan chRecvRnk
    --         case mrnk of
    --             Nothing  -> return ()
    --             Just rnk -> do
    --                 !b  <- runDnaParam p{actorRank = rnk} (action a)
    --                 dst' <- drainChannel0 chRecvDst dst
    --                 sendToDest dst' b
    --                 send (actorParent p) (me,DoneTask)
    --                 loop dst'
    -- loop =<< drainChannel chRecvDst
-}

-- | Start execution of collector actor
runCollectActor :: CollectActor a b -> Process ()
runCollectActor (CollectActor step start fini) = do
    -- Obtain parameters
    p <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendN,    chRecvN    ) <- newChan
    -- Send shell process description back
    sendChan (actorSendBack p)
        ( RcvReduce [(wrapMessage chSendParam, chSendN)]
        , [ sendPortId chSendParam , sendPortId chSendN ]
        )
    -- Start execution of an actor
    !b <- runDnaParam p $ do
           case [pCrash | CrashProbably pCrash <- actorDebugFlags p] of
             pCrash : _ -> crashMaybe pCrash
             _          -> return ()
           s0 <- kernel "collector init" [] (Kern start)
           s  <- gatherM (Group chRecvParam chRecvN) step s0
           kernel "collector fini" [] (Kern (fini s))
    sendResult p b

-- | Start execution of collector actor
runTreeActor :: TreeCollector a -> Process ()
runTreeActor (TreeCollector step start) = do
    -- Obtain parameters
    p <- expect
    -- Create channels for communication
    (chSendParam,chRecvParam) <- newChan
    (chSendN,    chRecvN    ) <- newChan
    -- Send shell process description back
    sendChan (actorSendBack p)
        ( RcvTree [(wrapMessage chSendParam, chSendN)]
        , [ sendPortId chSendParam , sendPortId chSendN ]
        )
    -- Start execution of an actor
    !a <- runDnaParam p $ do
           a0 <- kernel "tree collector init" [] (Kern start)
           gatherM (Group chRecvParam chRecvN) step a0
    sendResult p a



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- Send result to the destination we 
sendResult :: (Serializable a) => ActorParam -> a -> Process ()
sendResult p !a =
    sendLoop =<< drainExpect
  where
    sendLoop (dst,dstId) = do
        -- Send data to destination
        case dst of
          RcvSimple msg  -> trySend msg
          RcvReduce msgs -> forM_ msgs $ \(m,_) -> trySend m
          RcvTree   msgs -> do
              let nReducers           = length msgs
                  GroupSize nResults  = actorGroupSize p
                  Rank      rnk       = actorRank p
                  (ch,_) = msgs !! findIndexInSplittedList nResults nReducers rnk
              trySend ch
          RcvGrp    msgs -> mapM_ trySend msgs
        -- Send confirmation to parent and wait for 
        T.forM_ (actorParent p) $ \pid -> do
            me <- getSelfPid
            send pid (SentTo (actorAID p) me dstId)
            receiveWait
                [ match $ \(Terminate msg) -> error msg
                , match $ \newDest         -> sendLoop newDest
                , match $ \AckSend         -> return ()
                ]
    -- Loop over data
    trySend msg = do
        mch <- unwrapMessage msg
        case mch of
          Just ch -> unsafeSendChan ch a
          Nothing -> doPanic "Type error in channel!"

findIndexInSplittedList :: Int -> Int -> Int -> Int
findIndexInSplittedList nElems n rnk =
    case findIndex (\(a,b) -> rnk >= a && rnk < b) bins of
      Just i -> i
      _      -> error "Impossible split!"
  where
    bins = splitEvenly nElems n

splitEvenly :: Int -> Int -> [(Int,Int)]
splitEvenly nElems n
    = offs `zip` tail offs
  where
    (bin,rest) = nElems `divMod` n
    sizes = zipWith (+) (replicate n bin) (replicate rest 1 ++ repeat 0)
    offs  = scanl (+) 0 sizes

doGatherDna
    :: Serializable a
    => [MatchS]
    -> Group a
    -> (b -> a -> IO b)
    -> b
    -> DnaMonad b
doGatherDna ms (Group chA chN) f x0 = do
    let loop n tot !b
            | n >= tot && tot >= 0 = return b
        loop n tot !b = do
            r <- handleRecieve ms
                     [ Right `fmap` matchChan' chA
                     , Left  `fmap` matchChan' chN
                     ]
            case r of
              Right a -> loop (n + 1) tot =<< liftIO (f b a)
              Left  k -> loop n k b
    loop 0 (-1) x0

----------------------------------------------------------------
-- CH's TH
----------------------------------------------------------------

remotable [ 'runActor
          -- , 'runActorManyRanks
          , 'runCollectActor
          , 'runTreeActor
          ]
