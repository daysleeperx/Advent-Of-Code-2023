-- Day 3: GearRatios

module Day03.GearRatios (solve) where

import Text.Megaparsec
import Text.Megaparsec.Char

deltas :: Int -> Int -> [(Int, Int)]
deltas x y = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  putStr contents
