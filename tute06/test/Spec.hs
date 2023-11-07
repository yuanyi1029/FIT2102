import Test.DocTest (doctest)
import System.FilePath.Glob (glob)
import Control.Monad(mapM_, (>>=), (>>))

main :: IO ()
main = putStrLn "" >> glob "src/*.hs" >>= mapM_ testOne
  where
    testOne f = do
      putStrLn ""
      putStrLn $ "Testing: " ++ f
      doctest ["-isrc", f]