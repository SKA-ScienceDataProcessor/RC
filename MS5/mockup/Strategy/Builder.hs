{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, FlexibleInstances,
             ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses #-}

module Strategy.Builder
  (
  -- * Abstract data flow
    Flow, flow
  -- * Strategy
  , Strategy
  , runStrategy
  , uniqFlow, implementing, calculate
  -- * Kernel binding
  , IsReprs(..), IsReprKern(..)
  , kernel, Kernel
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

-- | Support class for allowing to pass lists of flows using curried
-- parameters
class IsFlows (Flows fs) => IsCurriedFlows fs where
  type Flows fs
  type Ret fs
  type KernFun fs
  curryFlow :: (Flows fs -> Flow (Ret fs)) -> fs
  uncurryFlow :: fs -> Flows fs -> Flow (Ret fs)
  uncurryKernFun :: fs -> KernFun fs -> Flows fs -> Kernel (Ret fs)
instance IsCurriedFlows (Flow a) where
  type Flows (Flow a) = HNil
  type Ret (Flow a) = a
  type KernFun (Flow a) = Kernel a
  curryFlow f = f HNil
  uncurryFlow fl _ = fl
  uncurryKernFun _ kfl _ = kfl
instance IsCurriedFlows fs => IsCurriedFlows (Flow f -> fs) where
  type Flows (Flow f -> fs) = Flow f :. Flows fs
  type Ret (Flow f -> fs) = Ret fs
  type KernFun (Flow f -> fs) = Flow f -> KernFun fs
  curryFlow f fl = curryFlow (f . (fl :.))
  uncurryFlow f (fl :. fls) = uncurryFlow (f fl) fls
  uncurryKernFun _ f (fl :. fls) = uncurryKernFun (undefined :: fs) (f fl) fls

-- | Class for reasoning about lists of data representations
class (IsFlows (RFlows rs), Pars (RFlows rs) ~ RPars rs) => IsReprs rs where
  type RPars rs
  type RFlows rs
  toReprsI :: rs -> [ReprI]
instance IsReprs HNil where
  type RPars HNil = HNil
  type RFlows HNil = HNil
  toReprsI _ = []
instance (DataRepr r, IsReprs rs) => IsReprs (r :. rs) where
  type RPars (r :. rs) = RPar r :. RPars rs
  type RFlows (r :. rs) = Flow (RPar r) :. RFlows rs
  toReprsI (r:.rs) = ReprI r : toReprsI rs

-- | Create a new abstract kernel flow
flow :: IsCurriedFlows fs => String -> fs
flow name = curryFlow (mkFlow name . toList)

-- | Create a new unique abstract kernel flow. In contrast to "flow",
-- the result will never be seen as equal to another flow.
uniqFlow :: IsFlows fl => String -> fl -> Strategy (Flow a)
uniqFlow name fls = state $ \ss ->
  (mkFlow (name ++ "." ++ show (ssKernelId ss)) $ toList fls,
   ss {ssKernelId = 1 + ssKernelId ss})

-- | Class for reasoning about producing kernels from curried lists of flows
class IsReprs rs => IsReprKern a rs where
  type RKern a rs
  curryReprs :: rs -> (RFlows rs -> Kernel a) -> RKern a rs
instance IsReprKern a HNil where
  type RKern a HNil = Kernel a
  curryReprs _ f = f HNil
instance (DataRepr r, IsReprKern a rs) => IsReprKern a (r :. rs) where
  type RKern a (r :. rs) = Flow (RPar r) -> RKern a rs
  curryReprs _ f fl = curryReprs (undefined :: rs) (f . (fl :.))

-- | Kernel implementation, with parameters bound to flows
data Kernel a where
  Kernel :: (IsReprs rs, IsReprKern (RPar r) rs, DataRepr r)
         => String -> KernelCode
         -> rs -> r -> RFlows rs
         -> Kernel (RPar r)

-- | Creates a new kernel using the given data representations for
-- input values. Needs to be bound to input flows.
kernel :: forall r rs. (DataRepr r, IsReprs rs, IsReprKern (RPar r) rs)
       => String -> KernelCode -> rs -> r -> RKern (RPar r) rs
kernel name code parReprs retRep
  = curryReprs (undefined :: rs) (Kernel name code parReprs retRep)

prepareKernel :: Kernel r -> Flow r -> [DomainId] -> Strategy KernelBind
prepareKernel (Kernel kname kcode parReprs retRep ps) (Flow fi) ds = do

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

  -- Make kernel, add to kernel list
  i <- freshKernelId
  let typeCheck (ReprI inR) = maybe False (reprCompatible retRep) (cast inR)
      kern = KernelBind i fi kname (ReprI retRep) kis kcode typeCheck
  addStep $ KernelStep ds kern
  return kern

-- | Bind the given flow to a kernel. For this to succeed, threee
-- conditions must be met:
--
-- 1. All input flows of the kernel must be direct or indirect data
-- dependencies of the given flow. If this flow was bound before, this
-- includes the flow itself (see also "rebind").
--
-- 2. All input flows have either been bound, or can be bound
-- automatically using rules registered by "rule".
--
-- 3. The bound kernels produce data representations that are fit
-- ("reprCompatible") for getting consumed by this kernel.
bind :: Flow r -> Kernel r -> Strategy ()
bind fl kfl = do
  entry <- prepareKernel kfl fl []
  let fi = kernFlow entry
  modify $ \ss -> ss{ ssMap = HM.insert fi entry (ssMap ss)}

-- | Bind the given flow to a kernel, using a one-dimensional
-- domain. See "bind" for details.
bind1D :: DomainHandle d -> Flow r -> Kernel r -> Strategy ()
bind1D d fl kfl = do
  entry <- prepareKernel kfl fl [dhId d]
  let fi = kernFlow entry
  modify $ \ss -> ss{ ssMap = HM.insert fi entry (ssMap ss)}

-- | Rebinds the given flow. This is a special case of "bind" for
-- kernels that modify the data the flow represents - for example to
-- change the data representations. This only works if the flow in
-- question has been bound previously.
rebind :: Flow a -> (Flow a -> Kernel a) -> Strategy ()
rebind fl f = bind fl (f fl)

-- | Rebinds the given flow, using a one-dimensional domain. See
-- "rebind" for details.
rebind1D :: DomainHandle d -> Flow a -> (Flow a -> Kernel a) -> Strategy ()
rebind1D dh fl f = bind1D dh fl (f fl)

-- | Registers a new rule for automatically binding kernels given a
-- certain data flow pattern. This is used by "calculate" to figure
-- out what to do. Furthermore, "bind" and friends will use them in
-- order to materialise missing data dependencies.
rule :: IsCurriedFlows fs
     => fs                        -- ^ Abstract data flow to match
     -> (Flows fs -> Strategy ()) -- ^ Code for binding the data flow
     -> Strategy ()
rule flf strat = do

  -- Pass wildcard flows to function to get pattern
  let (Flow pat) = uncurryFlow flf (wilds 0)

      -- Rule is now to match the given pattern, and if successful
      -- execute strategy and check that it actually implements the
      -- node.
      --
      -- TODO: We probably want to make a closure of the binds
      stratRule = StratRule $ \fi ->
        matchPattern fi pat >>=
        return . void . implementing (Flow fi) . strat

  modify $ \ss -> ss{ ssRules = stratRule : ssRules ss }

-- | Registers a new "rule" that binds a kernel to all occurences of
-- the given flow pattern. The kernel input types must exactly match
-- the flow inputs for this to work.
bindRule :: forall fs. IsCurriedFlows fs
         => fs
         -> KernFun fs
         -> Strategy ()
bindRule flf kern =
  rule flf $ \inp ->
    bind (uncurryFlow flf inp) (uncurryKernFun (undefined :: fs) kern inp)

-- | Reigsters a new "rule" that binds a kernel to all occurences of
-- the given flow pattern, subject to a one-dimensional domain. See
-- "bindRule".
bindRule1D :: forall d fs. IsCurriedFlows fs
           => DomainHandle d
           -> fs
           -> KernFun fs
           -> Strategy ()
bindRule1D dh flf kern =
  rule flf $ \inp ->
    bind1D dh (uncurryFlow flf inp) (uncurryKernFun (undefined :: fs) kern inp)

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

-- | Documents that the given strategy code is meant to bind the
-- indicated flow. An error will be raised if it fails to do so.
implementing :: Flow a -> Strategy () -> Strategy ()
implementing (Flow fi) strat = do
  -- Execute strategy
  strat
  -- Now verify that given flow was actually implemented
  ss <- get
  case HM.lookup fi (ssMap ss) of
    Just{}  -> return ()
    Nothing -> fail $ "Flow " ++ show fi ++ " was not implemented!"
