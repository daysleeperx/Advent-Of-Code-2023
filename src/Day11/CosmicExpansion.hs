-- Day 11: CosmicExpansion

module Day11.CosmicExpansion (solve) where

import Data.List (transpose)
import ParserUtils (Parser)
import Text.Megaparsec (many, oneOf, sepBy1)
import Text.Megaparsec.Char (newline)

type Coord = (Int, Int)

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parseLine :: Parser [Char]
parseLine = many (oneOf "#.")

parseInput :: Parser [[Char]]
parseInput = parseLine `sepBy1` newline

yOffset :: [[Char]] -> Int
yOffset = length . filter (all (== '.'))

xOffset :: [[Char]] -> Int
xOffset = yOffset . transpose

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    putStr contents