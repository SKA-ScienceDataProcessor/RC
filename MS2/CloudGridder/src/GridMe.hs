{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Data.Map as M
import Data.List (delete)
import System.IO (
    openBinaryFile
  , hClose
  , IOMode(..)
  , hPutBuf
  )
import System.IO.MMap (
    mmapFilePtr
  , munmapFilePtr
  , Mode(..)
  )
import Control.Exception (bracket)
import qualified Control.Distributed.Process as P
import Control.Distributed.Process.Node (
    newLocalNode
  , runProcess
  )
import Control.Distributed.Static (initRemoteTable)
import Network.Transport.TCP (
    createTransport
  , defaultTCPParameters
  )
import Foreign.Ptr (
    Ptr
  , plusPtr
  , ptrToIntPtr
  , intPtrToPtr
  )
-- For current fake workers which simply
-- return the copies of inputs
import Foreign.Marshal.Alloc (
    mallocBytes
  , free 
  )
import Foreign.Marshal.Utils (copyBytes)
--
import Data.Binary (
    Binary(..)
  , Get
  )
import Network.Socket (withSocketsDo)


-- Utilities
instance Binary (Ptr a) where
  put = put . toInteger . ptrToIntPtr
  get = (get :: Get Integer) >>= return . intPtrToPtr . fromIntegral

writeBinFile :: FilePath -> Ptr a -> Int -> IO ()
writeBinFile f p n = bracket (openBinaryFile f WriteMode) hClose
    (\h -> hPutBuf h p n)


-- Main

-- No clutter with newtypes to make code shorter and clearer.
type ArrD = (Ptr Double, Int)
type CalcD = ArrD -> IO ArrD

-- FIXME: Put genuine computations here. For now they only make plain copies for tests.
halide, romein :: CalcD
halide (p, n) = do
  pn <- mallocBytes n
  copyBytes pn p n
  return (pn, n)
romein = halide

-- FIXME: After previous FIXME is done, put corresponding finalizers here.
type Finalizer = ArrD -> IO ()
halideFinalizer, romeinFinalizer :: Finalizer
halideFinalizer = free . fst
romeinFinalizer = halideFinalizer

compute :: P.ProcessId -> ArrD -> CalcD -> P.Process ()
compute hostid arr action = do
  me <- P.getSelfPid
  res <- P.liftIO $ action arr
  P.send hostid (res, me)

writer :: M.Map P.ProcessId FilePath -> M.Map P.ProcessId Finalizer -> P.ProcessId -> (ArrD, P.ProcessId) -> P.Process ()
writer names finalizers hostid (arr@(p, n), cid) = do
  P.liftIO (writeBinFile (names M.! cid ++ ".dat") p n >> (finalizers M.! cid) arr)
  P.send hostid cid

hostProcess :: P.Process ()
hostProcess = do
  (ptr, rawsize, offset, size) <- P.liftIO $ mmapFilePtr "UVW.dat" ReadOnly Nothing
  host <- P.getSelfPid
  let comp = compute host (plusPtr ptr offset, size)
  halideId <- P.spawnLocal (comp halide)
  romeinId <- P.spawnLocal (comp romein)
  -- This is deliberately made to be scalable to any number of worker processes!
  let
    compids = [halideId, romeinId]
    names = M.fromList $ zip compids ["Halide", "Romein"]
    finalizers = M.fromList $ zip compids [halideFinalizer, romeinFinalizer]
    --
    matcher [] ws True = P.liftIO (munmapFilePtr ptr rawsize) >> matcher [] ws False
    matcher [] [] _    = return ()
    matcher comps writers ismapped = P.receiveWait [
        P.matchIf (\cr -> snd cr `elem` comps)
          (\cr -> do
                    _ <- P.spawnLocal $ writer names finalizers host cr
                    let cid = snd cr
                    matcher (cid `delete` comps) (cid : writers) ismapped
          )
      , P.matchIf (`elem` writers)
          (\cid -> matcher comps (cid `delete` writers) ismapped)
      ]
  matcher compids [] True

main :: IO ()
main = withSocketsDo $ do
  etr <- createTransport "localhost" "8000" defaultTCPParameters
  case etr of
    Left ex  -> print ex
    Right tr -> newLocalNode tr initRemoteTable >>= (`runProcess` hostProcess)
