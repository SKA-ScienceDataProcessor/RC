
{-# LANGUAGE BangPatterns #-}

module Profiling.Linux.Perf.Stat.PMU ( pmuTypeCode ) where

import Control.Monad

import Data.Bits
import Data.Word
import Data.List

import System.FilePath

-- Linux documents some (!) of the non-standard counters in
-- /sys/bus/event_source. The format is slightly non-trivial to
-- assemble - basically, we get a bunch of fields, which we need to
-- put together according to a separate format specification. This
-- might look like "config:1-2,4-20" - telling us to put the bits of
-- the field value into bits 1-2 and 4-20 of 'config' respectively.

eventSourcePath :: FilePath
eventSourcePath = "/sys/bus/event_source/devices"

-- | Split by character. This *really* ought to be in the Prelude in some form...
split :: Char -> String -> [String]
split c s = case dropWhile (==c) s of
              "" -> []
              s' -> w : split c s''
                    where (w, s'') = break (==c) s'

-- | Read performance counter description from @/sys/bus/event_source@.
pmuTypeCode :: String -> String -> IO (Word32, Word64, Word64, Word64)
pmuTypeCode deviceName counterName = do

  -- Get type id for devide
  typeId <- fmap read $ readFile $ eventSourcePath </> deviceName </> "type"

  -- Get counter description, split it up
  desc <- readFile $ eventSourcePath </> deviceName </> "events" </> counterName
  let descs = concatMap (split ',') $ lines desc

  -- Determine configuration
  !config <- foldM (pmuGetBits deviceName "config") 0 descs
  !config1 <- foldM (pmuGetBits deviceName "config1") 0 descs
  !config2 <- foldM (pmuGetBits deviceName "config2") 0 descs
  return (typeId, config, config1, config2)

-- | Add bits from event description
pmuGetBits :: String -> String -> Word64 -> String -> IO Word64
pmuGetBits deviceName config v descLine = do

  -- Get field name and value
  let (fld, val) = case split '=' descLine of
                     [f, c] -> (f, read c)
                     [f]    -> (f, 1 :: Word64)
                     _other -> ("", 0)
  if null fld then return v else do

      -- Read field description
      let formatPath = eventSourcePath </> deviceName </> "format" </> fld
      (config':fldDesc) <- fmap (split ':') $ readFile formatPath
      if config /= config' then return v else do

          -- Get bit masks, translate
          let bitDescs = split ',' $ intercalate "," fldDesc
              makeBits range = case split '-' range of
                                 [a,b]  -> [read a..read b]
                                 [a]    -> [read a]
                                 other -> error $ "invalid bit range: " ++ intercalate "-" other

          -- Combine with bits of "val"
          let bits = zip (concatMap makeBits bitDescs)
                         (map (testBit val) [0..finiteBitSize val])
              set (i, False) v' = v' `clearBit` i
              set (i, True)  v' = v' `setBit` i
          return $! foldr set v bits
