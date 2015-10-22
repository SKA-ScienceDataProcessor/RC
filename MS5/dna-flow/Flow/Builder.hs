{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, FlexibleInstances,
             ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses #-}

module Flow.Builder
  (
  -- * Abstract data flow
    Flow, flow
  -- * Strategy
  , Strategy
  , runStrategy
  , uniq, implementing, calculate
  -- * Kernel binding
  , IsReprs(..), IsReprKern(..)
  , kernel, Kernel
  , bind, rebind, bindRule, bindNew
  -- * Support
  , Z(..), (:.)(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Typeable

import Flow.Internal


-- | Run "Strategy" monad to convert it into a series of steps
runStrategy :: Strategy () -> [Step]
runStrategy strat = reverse $ ssSteps $ execState strat initStratState

-- | Class for rasoning about lists of flows
class IsFlows fs where
  type Pars fs
  toList :: fs -> [FlowI]
  wilds :: Int -> fs
  fromList :: [FlowI] -> fs -- unsafe!
instance IsFlows Z where
  type Pars Z = Z
  toList Z = []
  fromList [] = Z
  fromList _  = error "Internal error: fromList expected empty list!"
  wilds _ = Z
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
  type FlowsRet fs
  type FlowsKernFun fs
  curryFlow :: (Flows fs -> Flow (FlowsRet fs)) -> fs
  uncurryFlow :: fs -> Flows fs -> Flow (FlowsRet fs)
  uncurryKernFun :: fs -> FlowsKernFun fs -> Flows fs -> Kernel (FlowsRet fs)
instance IsCurriedFlows (Flow a) where
  type Flows (Flow a) = Z
  type FlowsRet (Flow a) = a
  type FlowsKernFun (Flow a) = Kernel a
  curryFlow f = f Z
  uncurryFlow fl _ = fl
  uncurryKernFun _ kfl _ = kfl
instance IsCurriedFlows fs => IsCurriedFlows (Flow f -> fs) where
  type Flows (Flow f -> fs) = Flow f :. Flows fs
  type FlowsRet (Flow f -> fs) = FlowsRet fs
  type FlowsKernFun (Flow f -> fs) = Flow f -> FlowsKernFun fs
  curryFlow f fl = curryFlow (f . (fl :.))
  uncurryFlow f (fl :. fls) = uncurryFlow (f fl) fls
  uncurryKernFun _ f (fl :. fls) = uncurryKernFun (undefined :: fs) (f fl) fls

-- | Class for reasoning about lists of data representations
class (IsFlows (ReprFlows rs), Pars (ReprFlows rs) ~ ReprTypes rs) => IsReprs rs where
  type ReprTypes rs
  type ReprFlows rs
  toReprsI :: rs -> [ReprI]
instance IsReprs Z where
  type ReprTypes Z = Z
  type ReprFlows Z = Z
  toReprsI _ = []
instance (DataRepr r, IsReprs rs) => IsReprs (r :. rs) where
  type ReprTypes (r :. rs) = ReprType r :. ReprTypes rs
  type ReprFlows (r :. rs) = Flow (ReprType r) :. ReprFlows rs
  toReprsI (r:.rs) = ReprI r : toReprsI rs

-- | Class for reasoning about producing kernels from curried lists of flows
class IsReprs rs => IsReprKern a rs where
  type ReprKernFun a rs
  curryReprs :: rs -> (ReprFlows rs -> Kernel a) -> ReprKernFun a rs
instance IsReprKern a Z where
  type ReprKernFun a Z = Kernel a
  curryReprs _ f = f Z
instance (DataRepr r, IsReprKern a rs) => IsReprKern a (r :. rs) where
  type ReprKernFun a (r :. rs) = Flow (ReprType r) -> ReprKernFun a rs
  curryReprs _ f fl = curryReprs (undefined :: rs) (f . (fl :.))

-- | Kernel implementation, with parameters bound to flows
data Kernel a where
  Kernel :: (IsReprs rs, IsReprKern (ReprType r) rs, DataRepr r)
         => String -> KernelCode
         -> rs -> r -> ReprFlows rs
         -> Kernel (ReprType r)

-- | Create a new abstract kernel flow
flow :: IsCurriedFlows fs => String -> fs
flow name = curryFlow (mkFlow name . toList)

-- | Makes a data flow unique. This means that the flow will get a new
-- identity, and anything bound to the old flow will no longer apply
-- to the new flow. No rule will ever mach a unique flow.
uniq :: Flow a -> Strategy (Flow a)
uniq (Flow fi) = state $ \ss ->
  (mkFlow (flName fi ++ "." ++ show (ssKernelId ss)) (flDepends fi),
   ss {ssKernelId = 1 + ssKernelId ss})

-- | Creates a new kernel using the given data representations for
-- input values. Needs to be bound to input flows.
kernel :: forall r rs. (DataRepr r, IsReprs rs, IsReprKern (ReprType r) rs)
       => String -> rs -> r -> KernelCode -> ReprKernFun (ReprType r) rs
kernel name parReprs retRep code
  = curryReprs (undefined :: rs) (Kernel name code parReprs retRep)

-- | Prepares the given kernel. This means checking its parameters and
-- adding it to the kernel list. However, it will not automatically be
-- added to the current scope.
prepareKernel :: Kernel r -> Flow r -> Strategy KernelBind
prepareKernel (Kernel kname kcode parReprs retRep ps) (Flow fi) = do

  -- Get parameters + representation. Filter out the ones marked as
  -- "don't care".
  let parReprsI = toReprsI parReprs
      pars = zip (toList ps) parReprsI

  -- Look up dependencies
  kis <- mapM (uncurry (prepareDependency kname fi)) $
         zip [1..] $ filter (not . isNoReprI . snd) pars

  -- Make kernel, add to kernel list
  i <- freshKernelId
  let typeCheck (ReprI inR) = maybe False (reprCompatible retRep) (cast inR)
      kern = KernelBind { kernId   = i
                        , kernFlow = fi
                        , kernName = kname
                        , kernRepr = ReprI retRep
                        , kernDeps = kis
                        , kernCode = kcode
                        , kernReprCheck = typeCheck
                        }
  addStep $ KernelStep kern
  return kern

-- | Prepares a concrete data dependency for a kernel implementing the
-- flow. This means finding the kernel that produces the result,
-- possibly aplying a rule, and finally doing a type-check to ensure
-- that data representations match.
prepareDependency :: String -> FlowI -> Int -> (FlowI, ReprI)
                  -> Strategy KernelDep
prepareDependency kname fi parn (p, prep) = do

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
        | kernReprCheck kern prep = return $ KernelDep (kernId kern) prep
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
            Just krn -> check krn
            Nothing  -> fail $ "When binding kernel " ++ kname ++ " to implement " ++
                          flName fi ++ ": Failed to apply rule to calculate " ++ show p ++ "! " ++
                          "This should be impossible!"

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
  entry <- prepareKernel kfl fl
  let fi = kernFlow entry
  modify $ \ss -> ss{ ssMap = HM.insert fi entry (ssMap ss)}

-- | Rebinds the given flow. This is a special case of "bind" for
-- kernels that modify the data the flow represents - for example to
-- change the data representations. This only works if the flow in
-- question has been bound previously.
rebind :: Flow a -> (Flow a -> Kernel a) -> Strategy ()
rebind fl f = bind fl (f fl)

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
         -> FlowsKernFun fs
         -> Strategy ()
bindRule flf kern =
  rule flf $ \inp ->
    bind (uncurryFlow flf inp) (uncurryKernFun (undefined :: fs) kern inp)

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

-- | Calls a kernel and returns a new unique stream for the result. This is
-- useful for input streams (the roots of the data flow graph) as well
-- as output flows, where we do not care about their output values.
bindNew :: Kernel r -> Strategy (Flow r)
bindNew kern@(Kernel name _ _ _ inps) = do
  fl <- uniq (mkFlow (name ++ "-call") (toList inps))
  bind fl kern
  return fl
