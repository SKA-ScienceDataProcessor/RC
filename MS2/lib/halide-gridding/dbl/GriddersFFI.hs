{-# LANGUAGE
    MagicHash
  , UnliftedFFITypes
  , UnboxedTuples
  , GHCForeignImportPrim
  #-}

module GriddersFFI where

import GHC.Ptr (Ptr(..), FunPtr(..))
import GHC.Types(IO(..), Int(I#))
import GHC.Prim (Addr#, State#, RealWorld, Int#)

data Grid
type GridHandle = Ptr Grid

newtype Status = St Int deriving Show

type GridFinalizer = GridHandle -> IO ()

data GridData = GD {
    gdHandle    :: GridHandle
  , gdData      :: Ptr Double
  , gdFinalizer :: GridFinalizer
  }

foreign import ccall "dynamic" 
  mkGridFinalizer :: FunPtr GridFinalizer -> GridFinalizer

type CGPrim = Addr# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr#, Addr#, Addr#, Int# #)

foreign import prim "romeinComputeGridOnCudazh" romeinComputeGridOnCuda# :: CGPrim
foreign import prim "halideComputeGridOnCudazh" halideComputeGridOnCuda# :: CGPrim

romeinComputeGridOnCuda :: Ptr Double -> Ptr Double -> IO (Either Status GridData)
romeinComputeGridOnCuda (Ptr uvwp) (Ptr ampp) = IO doit
  where
    doit s0 = case romeinComputeGridOnCuda# uvwp ampp s0 of
                (# s1, hdl, data_addr, fin, status #) ->
                   let res = case status of
                               0# -> Right (GD (Ptr hdl) (Ptr data_addr) (mkGridFinalizer $ FunPtr fin))
                               _  -> Left (St (I# status))
                   in (# s1, res #)

halideComputeGridOnCuda :: Ptr Double -> Ptr Double -> IO (Either Status GridData)
halideComputeGridOnCuda (Ptr uvwp) (Ptr ampp) = IO doit
  where
    doit s0 = case halideComputeGridOnCuda# uvwp ampp s0 of
                (# s1, hdl, data_addr, fin, status #) ->
                   let res = case status of
                               0# -> Right (GD (Ptr hdl) (Ptr data_addr) (mkGridFinalizer $ FunPtr fin))
                               _  -> Left (St (I# status))
                   in (# s1, res #)
