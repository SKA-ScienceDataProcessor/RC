-- | Functions common to different interpreters
module DNA.Utils where

import Data.Binary.Get
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as S

readPackedFile :: FilePath -> IO (S.Vector Double)
readPackedFile nm = do
  bs <- BS.readFile nm
  let n = fromIntegral $ BS.length bs `div` 8
  return $ runGet (S.replicateM n getFloat64le) bs
