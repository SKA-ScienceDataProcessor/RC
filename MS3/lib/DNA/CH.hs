{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Extra utils for cloud haskell
module DNA.CH where

import Control.Concurrent.STM (STM)
import Control.Distributed.Process
import Control.Distributed.Process.Serializable


----------------------------------------------------------------
-- Match with Functor instance
----------------------------------------------------------------

-- | Wrapper for Match which allow to write Functor instance. For some
--   unfathomable reason Match doesn't have one!
data Match' a = forall x. Match'
                (x -> Process a)
                (forall b. (x -> Process b) -> Match b)

matchSTM' :: STM a -> Match' a
matchSTM' stm = Match' return (matchSTM stm)

matchChan' :: Serializable a => ReceivePort a -> Match' a
matchChan' ch = Match' return (matchChan ch)

matchMsg' :: Serializable a => Match' a
matchMsg' = Match' return match

instance Functor Match' where
    fmap f (Match' g mk) = Match' ((fmap . fmap) f g) mk

-- | Convert to normal match
toMatch :: Match' a -> Match a
toMatch (Match' a f) = f a
