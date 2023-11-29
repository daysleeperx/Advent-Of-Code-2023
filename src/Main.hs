module Main (main) where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn "The arguments are:"
  mapM_ putStrLn args
