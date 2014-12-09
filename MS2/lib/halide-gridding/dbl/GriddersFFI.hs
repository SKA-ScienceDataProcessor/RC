{-# LANGUAGE
    MagicHash
  , UnliftedFFITypes
  , UnboxedTuples
  , GHCForeignImportPrim
  , GeneralizedNewtypeDeriving
  #-}

module GriddersFFI where

import GHC.Ptr (Ptr(..), FunPtr(..))
import GHC.Types(IO(..), Int(I#))
import GHC.Prim (Addr#, State#, RealWorld, Int#)
import Foreign.C.Types (CDouble)

data Grid
type GridHandle = Ptr Grid

newtype Status = St Int deriving (Num, Show)

type GridFinalizer = Addr# -> IO ()

data GridData = GD {
    gdData      :: Ptr CDouble
  , gdFinalizer :: IO ()
  }

foreign import ccall "dynamic" 
  mkGridFinalizer :: FunPtr GridFinalizer -> GridFinalizer

type CGPrim = Addr# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr#, Addr#, Addr#, Int# #)

foreign import prim "romeinComputeGridOnCudazh" romeinComputeGridOnCuda# :: CGPrim
foreign import prim "halideComputeGridOnCudazh" halideComputeGridOnCuda# :: CGPrim

romeinComputeGridOnCuda :: Ptr CDouble -> Ptr CDouble -> IO (Either Status GridData)
romeinComputeGridOnCuda (Ptr uvwp) (Ptr ampp) = IO doit
  where
    doit s0 = case romeinComputeGridOnCuda# uvwp ampp s0 of
                (# s1, hdl, data_addr, fin, status #) ->
                   let res = case status of
                               0# -> Right (GD (Ptr data_addr) (mkGridFinalizer (FunPtr fin) hdl))
                               _  -> Left (St (I# status))
                   in (# s1, res #)

halideComputeGridOnCuda :: Ptr CDouble -> Ptr CDouble -> IO (Either Status GridData)
halideComputeGridOnCuda (Ptr uvwp) (Ptr ampp) = IO doit
  where
    doit s0 = case halideComputeGridOnCuda# uvwp ampp s0 of
                (# s1, hdl, data_addr, fin, status #) ->
                   let res = case status of
                               0# -> Right (GD (Ptr data_addr) (mkGridFinalizer (FunPtr fin) hdl))
                               _  -> Left (St (I# status))
                   in (# s1, res #)
