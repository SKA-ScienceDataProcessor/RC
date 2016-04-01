{-# LANGUAGE
      CPP
    , MultiWayIf
  #-}

module Logging.ProcessLog where

import Data.Maybe
import Data.List
import Data.Word
import qualified Data.Map as M

import GHC.RTS.Events

import Text.Scanf
import Logging.Glue

-- Utils
pA :: String -> String -> Maybe String
pA at = fmap snd . ints (skws :^ lit ('[':at ++ ":")) id

mkPAv :: String -> (Int -> a) -> ReadM a
mkPAv an = ints (lit (an ++ "=") :^ int :^ lit "]")

mkPAv2 :: String -> (Int -> Int -> a) -> ReadM a
mkPAv2 an = ints (lit (an ++ "=") :^ int  :^ lit "/" :^ int :^ lit "]")

data ProfileHint
    = FloatHint { hintFloatOps :: !Int
                , hintDoubleOps :: !Int
                }
    | MemHint { hintMemoryReadBytes :: !Int
              }
    | IOHint { hintReadBytes :: !Int
             , hintWriteBytes :: !Int
             }
    | HaskellHint { hintAllocation :: !Int
                  }
    | CUDAHint { hintCopyBytesHost :: !Int
               , hintCopyBytesDevice :: !Int
               , hintCudaFloatOps :: !Int
               , hintCudaDoubleOps :: !Int
               }
    deriving (Show, Eq)

fh, mh, ioh, hh, ch :: ProfileHint
fh  = FloatHint 0 0
mh  = MemHint 0
ioh = IOHint 0 0
hh  = HaskellHint 0
ch  = CUDAHint 0 0 0 0

#define __PH(s, h, f) ( mkPAv s (\n -> Hint $ h {f = n}) )

parseHintVal, parseHint :: ReadM Attr
parseHintVal =
     __PH ("float-ops"          , fh , hintFloatOps       )
  ^: __PH ("double-ops"         , fh , hintDoubleOps      )
  ^: __PH ("mem-read-bytes"     , mh , hintMemoryReadBytes)
  ^: __PH ("read-bytes"         , ioh, hintReadBytes      )
  ^: __PH ("write-bytes"        , ioh, hintWriteBytes     )
  ^: __PH ("haskell-alloc"      , hh , hintAllocation     )
  ^: __PH ("memcpy-bytes-host"  , ch , hintCopyBytesHost  )
  ^: __PH ("memcpy-bytes-device", ch , hintCopyBytesDevice)
  ^: __PH ("gpu-float-ops"      , ch , hintCudaFloatOps   )
  ^: __PH ("gpu-double-ops"     , ch , hintCudaDoubleOps  )
parseHint s = pA "hint" s >>= parseHintVal

-- foo = parseHint "[hint:float-ops=65] wer"

data PerfCTyp =
    CpuCycles
  | CpuInstructions
  | X87Ops
  | ScalarFloatOps
  | ScalarDoubleOps
  | SseFloatOps
  | SseDoubleOps
  | AvxFloatOps
  | AvxDoubleOps
  | MemReadBytes
  deriving (Show, Eq, Enum)

parseFC, parsePerfC :: ReadM Attr
parseFC =
     pc2 "cpu-cycles"        CpuCycles
  ^: pc2 "cpu-instructions"  CpuInstructions
  ^: pc2 "x87-ops"           X87Ops
  ^: pc2 "scalar-float-ops"  ScalarFloatOps
  ^: pc2 "scalar-double-ops" ScalarDoubleOps
  ^: pc2 "sse-float-ops"     SseFloatOps
  ^: pc2 "sse-double-ops"    SseDoubleOps
  ^: pc2 "avx-float-ops"     AvxFloatOps
  ^: pc2 "avx-double-ops"    AvxDoubleOps
  ^: pc2 "mem-read-bytes"    MemReadBytes
  where pc2 an v = mkPAv2 an (Perf v)
parsePerfC s = pA "perf" s >>= parseFC

-- bar = parsePerfC "[perf:cpu-cycles=234/56985467] werwer"

data ProcCTyp =
    ReadBytes  -- C
  | WriteBytes -- C
  deriving (Show, Eq, Enum)

parseProcCT, parseProcC :: ReadM Attr
parseProcCT =
     pc1 "read-bytes"  ReadBytes
  ^: pc1 "write-bytes" WriteBytes
  where pc1 an v = mkPAv an (Proc v)
parseProcC s = pA "proc" s >>= parseProcCT

-- baz = parseProcC "[proc:read-bytes=12321313] werwer"

data CudaOps =
    GpuDoubleOps
  | GpuDoubleOpsAdd
  | GpuDoubleOpsMul
  | GpuDoubleOpsFMA
  deriving (Show, Eq, Enum)

data CudaMem =
    MemsetBytes   -- C
  | MemcpyBytesH  -- C
  | MemcpyBytesD  -- C
  deriving (Show, Eq, Enum)

data CudaTime =
    KernelTime    -- C
  | OverheadTime  -- C
  deriving (Show, Eq, Enum)

-- FIXME: need no timing for each metrix
--   because time reported is exactly "kernel-time"
parseCO, parseCM, parseCT :: ReadM Attr
parseCO =
     pc2 "gpu-double-ops"      GpuDoubleOps
  ^: pc2 "gpu-double-ops-add"  GpuDoubleOpsAdd
  ^: pc2 "gpu-double-ops-mul"  GpuDoubleOpsMul
  ^: pc2 "gpu-double-ops-fma"  GpuDoubleOpsFMA
  where pc2 an = mkPAv2 an . CO
parseCM =
     pc2 "memset-bytes"        MemsetBytes
  ^: pc2 "memcpy-bytes-host"   MemcpyBytesH
  ^: pc2 "memcpy-bytes-device" MemcpyBytesD
  where pc2 an = mkPAv2 an . CM
parseCT =
     pc1 "kernel-time"   KernelTime
  ^: pc1 "overhead-time" OverheadTime
  where pc1 an = mkPAv an . CT

parseCuda :: ReadM Attr
parseCuda s = pA "cuda" s >>= (parseCO ^: parseCM ^: parseCT)

-- gag = parseCuda "[cuda:kernel-time=234234] werwer"
-- faf = parseCuda "[cuda:gpu-double-ops=24234/6653] werwer"

data Attr =
    Hint ProfileHint
  | Perf PerfCTyp Int Int
  | Proc ProcCTyp Int    -- C
  | CO CudaOps Int Int
  | CM CudaMem Int Int   -- C
  | CT CudaTime Int      -- C
  deriving Show

isHint :: Attr -> Bool
isHint (Hint _) = True
isHint _ = False

isPerf :: Attr -> Bool
isPerf (Perf {}) = True
isPerf _ = False

isProc :: Attr -> Bool
isProc (Proc {}) = True
isProc _ = False

isCtr :: Attr -> Bool
isCtr (Proc {}) = True
isCtr (CM {}) = True
isCtr (CT {}) = True
isCtr _ = False

mkZ :: Attr -> Attr
mkZ (Proc pt _) = Proc pt 0
mkZ (CM cm _ _) = CM cm 0 0
mkZ (CT ct _) = CT ct 0
mkZ _ = error "mkZ: impossible happened: wrong attribute"

sameAttrTyp :: Attr -> Attr -> Bool
sameAttrTyp (Proc t1 _) (Proc t2 _) = t1 == t2
sameAttrTyp (CM t1 _ _) (CM t2 _ _) = t1 == t2
sameAttrTyp (CT t1 _) (CT t2 _) = t1 == t2
sameAttrTyp _ _ = False

glueAttrs :: [Attr] -> [Attr] -> ([Attr],[(Attr, Attr)])
glueAttrs sattrs eattrs = let
    (sc, sr) = partition isCtr sattrs
    (ec, er) = partition isCtr eattrs
  in (sr ++ er, merge sc ec)
  where
    -- End attrs may contain extra values but not otherwise
    -- We also expect the order of attrs is kept
    merge [] [] = []
    merge [] (y:ys) = (mkZ y, y) : merge [] ys
    merge l@(x:xs) (y:ys) =
      if sameAttrTyp x y
        then (x, y) : merge xs ys
        else (mkZ y, y) : merge l ys
    merge _ _ = error "Start attrs' counter not appear at the end call"

parseAttr :: ReadM Attr
parseAttr =
     parseHint
  ^: parsePerfC
  ^: parseProcC
  ^: parseCuda

-----------------
data MK = Start | End deriving (Eq, Show)

data UsrMsg = UsrMsg {
    umTime  :: Timestamp
  , umMK    :: MK
  , umLocal :: Int
  , umAttrs :: [Attr]
  , umMsg   :: String
} deriving Show

z :: UsrMsg
z = UsrMsg 0 Start 0 [] ""

parseIni :: ReadM UsrMsg
parseIni = ints (lit "START ") z{umMK = Start} ^: ints (lit "END ") z{umMK = End}

parseLocs :: UsrMsg -> ReadM (Int, UsrMsg)
parseLocs um = ints (ptesla :^ ploc) f
            ^: ints (psand  :^ ploc) f
  where
    ptesla = lit "[pid=pid://tesla" :^ int
    psand  = lit "[pid=pid://sand-" :^ dr 2 :^ int
    ploc   = dr 9 {- ":40000:0:" -} :^ int :^ lit "] "
    f = (\r l -> (r, um{umLocal=l}))

parseMsg :: (Int, UsrMsg) -> String -> Maybe (Int, UsrMsg)
parseMsg (r, um) = go []
  where
    go acc s = case parseAttr s of
                 Just (a, rst) -> go (a:acc) rst
                 Nothing -> Just (r, um{umAttrs=acc, umMsg = dropWhile (== ' ') s})

parseUsrMsg :: String -> Maybe (Int, UsrMsg)
parseUsrMsg s = parseIni s >>= uncurry parseLocs >>= uncurry parseMsg


-- gago = parseUsrMsg "START [pid=pid://tesla51:40000:0:12] [proc:read-bytes=0] [proc:write-bytes=33554432] gcfs"

data UsrMsgBlock = UsrMsgBlock {
    umbBTime  :: Timestamp
  , umbETime  :: Timestamp
  , umbCap    :: Int
  , umbRemote :: Int
  , umbMsgs   :: [UsrMsg]
} deriving Show

isUm :: Event -> Bool
isUm (Event _ (UserMessage _)) = True
isUm _ = False

cvtUM :: Event -> Maybe (Int, UsrMsg)
cvtUM (Event ts (UserMessage m)) = fmap (\(r, um) -> (r, um{umTime = ts})) (parseUsrMsg m)
cvtUM _ = error "cvtUM: impossible arg"

cvtBlock :: Event -> Maybe UsrMsgBlock
cvtBlock (Event bt (EventBlock et c evts)) =
  case rel_ums of
    (x:xs) -> let
                (r, u) = fromJust (cvtUM x)
                ms = catMaybes (map (fmap (umRaw2Um r) . cvtUM) xs)
              in Just (UsrMsgBlock bt et c r (u:ms))
    _      -> Nothing
  where
    rel_ums = dropWhile (isNothing . cvtUM) $ filter isUm evts
    umRaw2Um r (rr, um)
      | r == rr = um
      | otherwise = error $ "Inconsistent block data: " ++ show r ++ '/':(show rr)
cvtBlock _ = Nothing

cvtBlocks :: [Event] -> [UsrMsgBlock]
cvtBlocks = catMaybes . map cvtBlock

getKey :: UsrMsg -> (Int, String)
getKey um = (umLocal um, umMsg um)

data KernInvDescr = KernInvDescr {
    kidStart  :: Timestamp
  , kidEnd    :: Timestamp
  , kidLocal  :: Int
  , kidHints  :: [ProfileHint]
  , kidAttrs  :: [Attr]
  , kidAPairs :: [(Attr, Attr)]
  , kidName   :: String
  } deriving Show

-- We could use State monad here but I prefer to thread
-- the map explicitly
pairThem :: [UsrMsg] -> [KernInvDescr]
pairThem = go M.empty []
  where
    go started acc (x:xs) =
      let
        k = getKey x
        mbs = k `M.lookup` started
        mk = umMK x
      in if | mk == Start && isNothing mbs -> go (M.insert k x started) acc xs
            | mk == End && isJust mbs ->
               let
                 s = fromJust mbs
                 (hints, rattrs) = partition isHint (umAttrs s)
                 (attrs, apairs) = glueAttrs rattrs (umAttrs x)
                 peeledhints = map (\(Hint h) -> h) hints
               in go (M.delete k started) (KernInvDescr (umTime s) (umTime x) (fst k) peeledhints attrs apairs (snd k) : acc) xs
            | otherwise -> error ("Unbalanced: " ++ show x)
    go started acc _ = if M.null started then reverse acc else error $ "Unpaired start(s) detected: " ++ show started

data KIDBlock = KIDBlock {
    kidbBTime  :: Timestamp
  , kidbETime  :: Timestamp
  , kidbCap    :: Int
  , kidbRemote :: Int
  , kidbMsgs   :: [KernInvDescr]
} deriving Show

cvtAll :: UsrMsgBlock -> KIDBlock
cvtAll umb = KIDBlock {
    kidbBTime  = umbBTime  umb
  , kidbETime  = umbETime  umb
  , kidbCap    = umbCap    umb
  , kidbRemote = umbRemote umb
  , kidbMsgs   = pairThem (umbMsgs umb)
  }

preProcessEventLog :: EventLog -> [UsrMsgBlock]
preProcessEventLog = cvtBlocks . glueMessages0 . events . dat

processEventLog :: EventLog -> [KIDBlock]
processEventLog = map cvtAll . preProcessEventLog

check :: (Eq a, Show a) => a -> a -> b -> b
check v1 v2 r
  | v1 /= v2 = error $ "Internal error: wrong pair: " ++ show v1 ++ '/':show v1
  | otherwise = r

data Descr = Descr {
    dRtsIdentifier :: String
  , dProgramArgs   :: [String]
  , dProgramEnv    :: [String]
  , dOsProcessPid  :: !Word32
  , dOsProcessParentPid :: !Word32
  , dWallClockTimeSec   :: !Word64
  , dWallClockTimeNSec  :: !Word32
  } deriving Show

zd :: Descr
zd = Descr "" [] [] 0 0 0 0

#define _U(f) f _ v -> go d{d/**/f = v} (n-1) xs
suckDescr :: [Event] -> (Descr, [Event])
suckDescr = go zd (6::Int)
  where
    go d 0 r = (d, r)
    go d n (x:xs) = case (spec x) of
      _U(RtsIdentifier)
      _U(ProgramArgs  )
      _U(ProgramEnv   )
      _U(OsProcessPid )
      _U(OsProcessParentPid)
      WallClockTime _ ts tns -> go d{dWallClockTimeSec = ts, dWallClockTimeNSec = tns} (n-1) xs
      _ -> go d n xs
    go d _ [] = (d, [])
#undef _U
