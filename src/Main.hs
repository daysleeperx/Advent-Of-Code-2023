module Main (main) where

import qualified Day00.Test as Test
import System.Environment (getArgs)

solvers :: [FilePath -> IO ()]
solvers = [Test.solve]

main :: IO ()
main = do
  (day : filePath : _) <- getArgs
  let solver = solvers !! read day
  solver filePath
