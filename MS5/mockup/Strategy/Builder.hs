{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, FlexibleInstances,
             ScopedTypeVariables #-}

module Strategy.Builder
  (
  -- * Abstract data flow
    Flow, flow
  -- * Strategy
  , Strategy
  , runStrategy
  , uniqFlow, implementing, calculate
  -- * Kernel binding
  , Kernel(..), IsReprs(..)
  , bind, bind1D
  , rebind, rebind1D
  , bindRule, bindRule1D
  -- * Support
  , HNil(..), (:.)(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Typeable

import Strategy.Internal

-- | Run "Strategy" monad to convert it into a series of steps
runStrategy :: Strategy () -> [Step]
runStrategy strat = reverse $ ssSteps $ execState strat initStratState

-- Simple type-level lists. TODO: Import from an external library?
-- There are too many to choose from...
data HNil = HNil
  deriving Show
data (:.) a b = (:.) a b
  deriving Show
infixr :.

-- | Class for rasoning about lists of flows
class IsFlows fs where
  type Pars fs
  toList :: fs -> [FlowI]
  wilds :: Int -> fs
  fromList :: [FlowI] -> fs -- unsafe!
instance IsFlows HNil where
  type Pars HNil = HNil
  toList HNil = []
  fromList [] = HNil
  fromList _  = error "Internal error: fromList expected empty list!"
  wilds _ = HNil
instance IsFlows fs => IsFlows (Flow a :. fs) where
  type Pars (Flow a :. fs) = a :. Pars fs
  toList (Flow f :. fs) = f : toList fs
  fromList (f:fs) = Flow f :. fromList fs
  fromList _      = error "Internal error: fromList expected non-empty list!"
  wilds i = wildFlow i :. wilds (i+1)

-- | Class for reasoning about lists of data representations
class IsReprs rs where
  type RPars rs
  toReprsI :: rs -> [ReprI]
instance IsReprs HNil where
  type RPars HNil = HNil
  toReprsI _ = []
instance (DataRepr r, IsReprs rs) => IsReprs (r :. rs) where
  type RPars (r :. rs) = RPar r :. RPars rs
  toReprsI (r:.rs) = ReprI r : toReprsI rs

-- | Create a new abstract kernel flow
flow :: IsFlows fs => String -> fs -> Flow r
flow name fs = mkFlow name (toList fs)

-- | Create a new unique abstract kernel flow. In contrast to "flow",
-- the result will never be seen as equal to another flow.
uniqFlow :: IsFlows fl => String -> fl -> Strategy (Flow a)
uniqFlow name fls = state $ \ss ->
  (flow (name ++ "." ++ show (ssKernelId ss)) fls,
   ss {ssKernelId = 1 + ssKernelId ss})

-- | Represents a kernel implementation
data Kernel ps a where
  Kernel :: (IsReprs rs, DataRepr r)
         => String -> rs -> r
         -> Kernel (RPars rs) (RPar r)

prepareKernel :: forall ps a. IsFlows ps
              => ps -> Flow a -> [DomainId] -> Kernel (Pars ps) a -> Strategy KernelBind
prepareKernel ps (Flow fi) ds (Kernel kname parReprs retRep) = do

  -- Make kernel
  i <- freshKernelId
  let typeCheck (ReprI inR) = maybe False (reprCompatible retRep) (cast inR)

  -- Get parameters + representation. Filter out the ones marked as
  -- "don't care".
  let parReprsI = toReprsI parReprs
      pars = zip (toList ps) parReprsI
      pars' = filter (not . isNoReprI . snd) pars

  -- Look up dependencies
  kis <- forM (zip [(1::Int)..] pars') $ \(parn, (p, prep)) -> do

    -- Parameter flows must all be in the dependency tree of the flow
    -- to calculate. Yes, this means that theoretically flows are
    -- allowed to depend on very early flows. This is most likely not
    -- a good idea though.
    let hasDepend f
          | p == f    = True
          | otherwise = any hasDepend $ flDepends f
    when (not $ hasDepend fi) $
      fail $ "Parameter " ++ show p ++ " not a dependency of " ++ show fi ++ "!"

    -- Look up latest kernel ID
    ss <- get
    let check kern
          | kernReprCheck kern prep = return $ kernId kern
          | otherwise = fail $ concat
              [ "Data representation mismatch when binding kernel "
              , kname, " to implement "
              , flName fi, ": Expected ", show prep, " for parameter "
              , show parn, ", but kernel ", kernName kern, " produced "
              , show (kernRepr kern), "!"
              ]
    case HM.lookup p (ssMap ss) of
      Just sme -> check sme
      Nothing  -> do

        -- Not defined yet? Attempt to match a rule to produce it
        m_strat <- findRule (Flow p)
        case m_strat of
          Nothing -> fail $ "When binding kernel " ++ kname ++ " to implement " ++
                            flName fi ++ ": Could not find a kernel calculating flow " ++
                            show p ++ "!"
          Just strat -> do

            -- Execute rule
            strat

            -- Lookup again. The rule should normaly guarantee that
            -- this doesn't fail any more.
            ss' <- get
            case HM.lookup p (ssMap ss') of
              Just krn -> return $ kernId krn
              Nothing  -> fail $ "When binding kernel " ++ kname ++ " to implement " ++
                            flName fi ++ ": Failed to apply rule to calculate " ++ show p ++ "! " ++
                            "This should be impossible!"

  -- Add to kernel list
  let kern = KernelBind i fi kname (ReprI retRep) kis typeCheck
  addStep $ KernelStep ds kern
  return kern

-- | Bind the given flow to a kernel. This is equivalent to first
-- setting a "rule" for the flow, then calling "calculate". More
-- efficient though.
bind :: IsFlows fs => fs -> Flow a -> Kernel (Pars fs) a -> Strategy ()
bind ps fl k = do
  entry <- prepareKernel ps fl [] k
  let fi = kernFlow entry
  modify $ \ss -> ss{ ssMap = HM.insert fi entry (ssMap ss)}

bind1D :: IsFlows fs
       => DomainHandle d -> fs -> Flow a -> Kernel (Pars fs) a
       -> Strategy ()
bind1D d ps fl k = do
  entry <- prepareKernel ps fl [dhId d] k
  let fi = kernFlow entry
  modify $ \ss -> ss{ ssMap = HM.insert fi entry (ssMap ss)}

rebind :: Flow a -> Kernel (a :. HNil) a -> Strategy ()
rebind fl = bind (fl :. HNil) fl

rebind1D :: DomainHandle d -> Flow a -> Kernel (a :. HNil) a
         -> Strategy ()
rebind1D dh fl = bind1D dh (fl :. HNil) fl

rule :: IsFlows fs => (fs -> Flow a) -> (fs -> Strategy ()) -> Strategy ()
rule flf strat = do

  -- Pass wildcard flows to function to get pattern
  let (Flow pat) = flf (wilds 0)

      -- Rule is now to match the given pattern, and if successful
      -- execute strategy and check that it actually implements the
      -- node.
      --
      -- TODO: We probably want to make a closure of the binds
      stratRule = StratRule $ \fi ->
        matchPattern fi pat >>=
        return . void . implementing (Flow fi) . strat

  modify $ \ss -> ss{ ssRules = stratRule : ssRules ss }

-- | Simple rule, binding a kernel to a pattern
bindRule :: IsFlows fs => (fs -> Flow a) -> Kernel (Pars fs) a -> Strategy()
bindRule flf kern = rule flf $ \inp -> bind inp (flf inp) kern

bindRule1D :: IsFlows fs
           => DomainHandle d -> (fs -> Flow a) -> Kernel (Pars fs) a -> Strategy()
bindRule1D dh flf kern = rule flf $ \inp -> bind1D dh inp (flf inp) kern

-- | Check whether a flow matches a pattern.
matchPattern :: forall fs. IsFlows fs => FlowI -> FlowI -> Maybe fs
matchPattern fi pat
  | fi == pat = Just (wilds 0)
  | Just i <- flWildcard pat
  = Just $ fromList $ set i fi $ toList (wilds 0 :: fs)
  | flName fi == flName pat,
    length (flDepends fi) == length (flDepends pat),
    Just matches <- zipWithM matchPattern (flDepends fi) (flDepends pat)
  = mergeMatches matches
  | otherwise
  = Nothing
 where -- Sets n-th element in list. Edward Kmett is probably hating me
       -- now.
       set :: Int -> b -> [b] -> [b]
       set 0 x (_:ys) = x:ys
       set i x (y:ys) = y:set (i-1) x ys
       set _ _ []     = error "internal error: matchPattern/set used wrong list index!"

-- | Merges data flow pattern match results
mergeMatches :: IsFlows fs => [fs] -> Maybe fs
mergeMatches []   = Just (wilds 0)
mergeMatches [fs] = Just fs
mergeMatches (fs0:fs1:rest) = do
  let merge :: FlowI -> FlowI -> Maybe FlowI
      merge f0 f1
        | Just{} <- flWildcard f0  = Just f1
        | Just{} <- flWildcard f1  = Just f0
        | f0 == f1                 = Just f0
        | otherwise                = Nothing
  fs' <- fromList <$> zipWithM merge (toList fs0) (toList fs1)
  mergeMatches (fs':rest)

-- | Calculate a flow. This can only succeed if there is a rule in scope that
-- explains how to do this.
calculate :: Flow a -> Strategy ()
calculate fl = do
  m_strat <- findRule fl
  case m_strat of
    Nothing -> fail $ "calculate: Could not find a rule matching " ++ show fl ++ "!"
    Just strat -> strat

-- | Calculate a flow. This can only succeed if there is a rule in scope that
-- explains how to do this.
findRule :: Flow a -> Strategy (Maybe (Strategy ()))
findRule (Flow fi) = do

  -- Find a matching rule
  rules <- ssRules <$> get
  let apply (StratRule r) = r fi
  return $ listToMaybe $ mapMaybe apply rules
    -- TODO: Warn about rule overlap?

implementing :: Flow a -> Strategy () -> Strategy ()
implementing (Flow fi) strat = do
  -- Execute strategy
  strat
  -- Now verify that given flow was actually implemented
  ss <- get
  case HM.lookup fi (ssMap ss) of
    Just{}  -> return ()
    Nothing -> fail $ "Flow " ++ show fi ++ " was not implemented!"
