{-# LANGUAGE GADTs #-}
module DNA.Interpreter.Connect where

import Control.Monad
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Data.Typeable (cast)

import DNA.Types



-- Send channels to actor so they know where data should be sent
doConnectActors
    :: (Serializable a)
    => SendEnd (tag a) -> RecvEnd (tag a) -> Process ()
doConnectActors sendEnd recvEnd =
  case (sendEnd,recvEnd) of
    -- Val
    (SendVal chDst, RecvVal chB) ->
        -- FIXME: Do we want to allow unsafe send here?
        --        Maybe we should just use unsafe send and call it a day?
        sendChan chDst $ SendLocally chB
    (SendVal chDst, RecvBroadcast (RecvGrp chans)) ->
        sendChan chDst $ SendRemote chans
    -- Grp
    (SendGrp chDst, RecvReduce chReduce) -> do
        let chB = map snd chReduce
        forM_ chDst $ \ch -> sendChan ch $ SendRemote chB
    -- MR
    (SendMR chDst, RecvMR chans) -> do
        let chB = map snd chans
        forM_ chDst $ \ch -> sendChan ch chB
    -- IMPOSSIBLE
    --
    -- GHC cannot understand that pattern match is exhaustive
    _ -> error "Impossible: pattern match is not exhaustive"

doConnectActorsExistentially :: SomeSendEnd -> SomeRecvEnd -> Process ()
doConnectActorsExistentially (SomeSendEnd s) (SomeRecvEnd rcv) =
  case cast rcv of
    Nothing -> error "Ooops! Types do not match"
    Just r  -> doConnectActors s r
