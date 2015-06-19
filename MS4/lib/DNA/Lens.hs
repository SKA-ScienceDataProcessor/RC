-- |
-- Lens are immensely useful for working with state but I don't want
-- to pull in full Kmettoverse for small set of combinators.
--
-- Here we redefine all necessary combinators. Full compatibility with
-- lens is maintained.
{-# LANGUAGE RankNTypes #-}
module DNA.Lens where

import Control.Applicative
import Control.Monad.State
import Data.Functor.Identity
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict as Map



type Lens' s a = forall f. Functor f => (a -> f a) -> (s -> f s)

lens :: (s -> a) -> (a -> s -> s) -> Lens' s a
lens getf putf = \f s -> flip putf s <$> f (getf s)

-- Get value from object
(^.) :: s -> Lens' s a -> a
s ^. l = getConst $ l Const s

-- Put value into object
set :: Lens' s a -> a -> s -> s
set l a s = runIdentity $ l (\_ -> Identity a) s

over :: Lens' s a -> (a -> a) -> s -> s
over l f s = runIdentity $ l (Identity . f) s

(.=) :: MonadState s m => Lens' s a -> a -> m ()
l .= b = modify' $ set l b

(%=) :: MonadState s m => Lens' s a -> (a -> a) -> m ()
l %= b = modify' $ over l b

infix 4 .=, %=

use :: MonadState s m => Lens' s a -> m a
use l = do
    s <- get
    return $ s ^. l


at :: (Ord k) => k -> Lens' (Map k v) (Maybe v)
at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (Map.delete k m)) mv
    Just v' -> Map.insert k v' m
    where mv = Map.lookup k m
{-# INLINE at #-}


(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

failure :: MonadIO m => String -> m a
failure msg = do
    liftIO $ putStrLn $ "FAILED: " ++ msg
    error msg



zoom :: Monad m => Lens' s a -> StateT a m b -> StateT s m b
zoom l action = do
    s <- get
    (b,a') <- lift $ runStateT action (s ^. l)
    put $ set l a' s
    return b
