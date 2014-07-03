
import DNA.SingleThreaded
import DNA.AST


program :: DNA Double
program = DotProduct (ReadFile "data-file") (Generate 1000)

main :: IO ()
main = do
  print =<< interpret program
