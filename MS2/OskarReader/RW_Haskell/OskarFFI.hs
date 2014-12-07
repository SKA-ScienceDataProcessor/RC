{-# LANGUAGE
    MagicHash
  , UnliftedFFITypes
  , UnboxedTuples
  , GHCForeignImportPrim
  #-}

module OskarFFI where

import GHC.Ptr (Ptr(..), FunPtr(..))
import GHC.Types(IO(..), Int(I#))
import GHC.Prim (Addr#, State#, RealWorld, Int#)
import Foreign.C.String (withCString)

data OData
type OHandle = Ptr OData

newtype Status = St Int deriving Show

type WriteAndFin = OHandle -> Ptr () -> Ptr () -> IO Status

foreign import ccall "dynamic" 
  mkWriteAndFin :: FunPtr WriteAndFin -> WriteAndFin

data OVisData = OVD {
    ovdHandle      :: OHandle
  , ovdTSxBL       :: Int
  , ovdNumChans    :: Int
  , ovdWriteAndFin :: WriteAndFin
  }

foreign import prim "vis_allocate_and_readzh"
  vis_allocate_and_read# :: Addr# -> State# RealWorld -> (# State# RealWorld, Addr#, Int#, Int#, Addr#, Int# #)

vis_allocate_and_read :: String -> IO (Either Status OVisData)
vis_allocate_and_read = (`withCString` doit)
  where
    doit (Ptr addr) = IO $
      \s0 -> case vis_allocate_and_read# addr s0 of
               (# s1, hdl, ts_x_bl, chans, write_and_fin, status #) ->
                  let res = case status of
                              0# -> Right (OVD (Ptr hdl) (I# ts_x_bl) (I# chans) (mkWriteAndFin $ FunPtr write_and_fin))
                              _  -> Left (St (I# status))
                  in (# s1, res #)
