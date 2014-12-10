{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    MagicHash
  , UnliftedFFITypes
  , UnboxedTuples
  , GHCForeignImportPrim
  , GeneralizedNewtypeDeriving
  , AutoDeriveTypeable
  #-}

module GriddersFFI where

import GHC.Ptr (
    Ptr(..)
  , FunPtr(..)
  , castFunPtrToPtr
  , castPtrToFunPtr
  )
import GHC.Types(
    IO(..)
  , Int(I#)
  )
import GHC.Prim (
    Addr#
  , State#
  , RealWorld
  , Int#
  , addr2Int#
  , int2Addr#
  )
import Data.Typeable(Typeable)
import Foreign.C.Types (CDouble(..))
import Data.Binary (Binary(..))
import Control.Monad(liftM3)

data Grid
type GridHandle = Ptr Grid

newtype GStatus =
 GSt {fromGStatus :: Int}
   deriving (Num, Show, Eq, Binary, Typeable)

type GridFinalizer = GridHandle -> IO ()

-- FIXME! addr2Int# and int2Addr# are strongly deprecated
instance Binary (Ptr a) where
  put (Ptr addr#) = put (I# (addr2Int# addr#))
  get = fmap i2a get
    where i2a (I# i#) = Ptr (int2Addr# i#)
  
instance Binary (FunPtr a) where
  put = put . castFunPtrToPtr
  get = fmap castPtrToFunPtr get

data GridData = GD {
    gdHandle       :: GridHandle
  , gdData         :: Ptr CDouble
  , gdFinalizerPtr :: FunPtr GridFinalizer
  }

instance Binary GridData where
  put (GD hdl datap finp) = put hdl >> put datap >> put finp
  get = liftM3 GD get get get

foreign import ccall "dynamic" 
  mkGridFinalizer :: FunPtr GridFinalizer -> GridFinalizer

gdFinalize :: GridData -> IO ()
gdFinalize (GD hdl _ finptr) = mkGridFinalizer finptr hdl

type CGPrim = Addr# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr#, Addr#, Addr#, Int# #)

foreign import prim "romeinComputeGridOnCudazh" romeinComputeGridOnCuda# :: CGPrim
foreign import prim "halideComputeGridOnCudazh" halideComputeGridOnCuda# :: CGPrim

type GridProcType = Ptr CDouble -> Ptr CDouble -> IO (Either GStatus GridData)

romeinComputeGridOnCuda :: GridProcType
romeinComputeGridOnCuda (Ptr uvwp) (Ptr ampp) = IO doit
  where
    doit s0 = case romeinComputeGridOnCuda# uvwp ampp s0 of
                (# s1, hdl, data_addr, fin, status #) ->
                   let res = case status of
                               0# -> Right (GD (Ptr hdl) (Ptr data_addr) (FunPtr fin))
                               _  -> Left (GSt (I# status))
                   in (# s1, res #)

halideComputeGridOnCuda :: GridProcType
halideComputeGridOnCuda (Ptr uvwp) (Ptr ampp) = IO doit
  where
    doit s0 = case halideComputeGridOnCuda# uvwp ampp s0 of
                (# s1, hdl, data_addr, fin, status #) ->
                   let res = case status of
                               0# -> Right (GD (Ptr hdl) (Ptr data_addr) (FunPtr fin))
                               _  -> Left (GSt (I# status))
                   in (# s1, res #)
