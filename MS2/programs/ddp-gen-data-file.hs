import Control.Monad
import Data.Binary.Put
import Data.Binary.IEEE754
import System.Environment

import qualified Data.ByteString.Lazy as BS

genFile :: Int -> FilePath -> IO ()
genFile n nm = do
    BS.writeFile nm $ runPut $ replicateM_ n (putFloat64le 0.1)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [n,nm] -> genFile (read n) nm
      _      -> error "Usage: ddp-gen-data-file N file"
