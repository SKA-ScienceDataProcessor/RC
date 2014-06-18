-- | Simple combinators for cloud haskell programs
module RC.Combinators (
    -- * Folds and loops
    foldMessages
  , foldMessagesMatch
  , foldMatch
  , finiFoldMessages
  , finiFoldMessagesMatch
  , finiFoldMatch
  , maybeLoop
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)


----------------------------------------------------------------
-- Folds and loops
----------------------------------------------------------------

-- | Indefinitely fold received messages
foldMessages :: (Serializable a) => (s -> a -> Process s) -> s -> Process r
foldMessages step
  = loop
  where
    loop s = loop =<< step s =<< expect

-- | Same as 'foldMessages' but uses 'Match' for receiving messages
foldMessagesMatch :: [Match a] -> (s -> a -> Process s) -> s -> Process r
foldMessagesMatch m step
  = loop
  where
    loop s = loop =<< step s =<< receiveWait m

-- | Same as 'foldMessagesMatch' but update function is merged into 'Match'
foldMatch :: [s -> Match s] -> s -> Process r
foldMatch ms
  = loop
  where
    loop s = loop =<< receiveWait [m s | m <- ms]

-- | Fold received messages.
finiFoldMessages
  :: (Serializable a) => (s -> a -> Process (Either b s)) -> s -> Process b
finiFoldMessages step
  = loop
  where
    loop s = either return loop =<< step s =<< expect

-- | Same as 'finiFoldMessages' but uses 'Match' for receiving messages
finiFoldMessagesMatch
  :: [Match a] -> (s -> a -> Process (Either b s)) -> s -> Process b
finiFoldMessagesMatch m step
  = loop
  where
    loop s = either return loop =<< step s =<< receiveWait m


-- | Same as 'foldMessages' but uses 'Match' for receiving messages
finiFoldMatch
  :: [s -> Match (Either a s)] -> s -> Process a
finiFoldMatch ms
  = loop
  where
    loop s = either return loop =<< receiveWait [m s | m <- ms]


-- | Perform action on each received @Just@ value and terminate on
--   first @Nothing@
maybeLoop :: (Serializable a)
          => (a -> Process ()) -> Process ()
maybeLoop f = loop
  where
    loop = do
      ma <- expect
      case ma of Just a  -> f a >> loop
                 Nothing -> return ()
