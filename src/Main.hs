module Main (main) where

import qualified Day00.Test as Test
import qualified Day01.Trebuchet as Trebuchet
import qualified Day02.CubeConundrum as CubeConundrum
import System.Environment (getArgs)

solvers :: [FilePath -> IO ()]
solvers =
  [ Test.solve,
    Trebuchet.solve,
    CubeConundrum.solve
  ]

main :: IO ()
main = do
  (day : filePath : _) <- getArgs
  let solver = solvers !! read day
  solver filePath