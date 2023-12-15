-- Day 11: CosmicExpansion
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Day11.CosmicExpansion (solve) where

import Control.Category ((>>>))
import Data.List (tails, transpose)
import qualified Data.Map as M
import ParserUtils (Parser)
import Text.Megaparsec (many, oneOf, parse, sepBy1)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Error (errorBundlePretty)

type Coord = (Int, Int)

data InputType = InputType
    { galaxies :: [Coord]
    , emptyRows :: [Int]
    , emptyCols :: [Int]
    }

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parseLine :: Parser [Char]
parseLine = many (oneOf "#.")

parseInput :: Parser InputType
parseInput = do
    inp <- parseLine `sepBy1` newline
    let emptyRows = getEmptyRows inp
    let emptyCols = getEmptyRows (transpose inp)
    let galaxies = (M.keys . M.filter (== '#')) (toGrid inp)
    pure InputType{..}

toGrid :: [[Char]] -> M.Map Coord Char
toGrid inp =
    M.fromList
        [ ((x, y), tile)
        | (y, row) <- zip [0 ..] inp
        , (x, tile) <- zip [0 ..] row
        ]

getEmptyRows :: [[Char]] -> [Int]
getEmptyRows inp = [y | (y, row) <- zip [0 ..] inp, all (== '.') row]

offset :: Int -> [Int] -> [Int] -> Coord -> Coord
offset n emptyRows emptyCols (x, y) =
    ( x + length (filter (< x) emptyCols) * (n - 1)
    , y + length (filter (< y) emptyRows) * (n - 1)
    )

offsetGalaxies :: Int -> InputType -> [Coord]
offsetGalaxies n InputType{..} = map (offset n emptyRows emptyCols) galaxies

cartesianPairs :: [a] -> [(a, a)]
cartesianPairs list = [(x, y) | (x : ys) <- tails list, y <- ys]

sumGalaxyPaths :: Int -> InputType -> Int
sumGalaxyPaths n =
    offsetGalaxies n
        >>> cartesianPairs
        >>> map (uncurry manhattan)
        >>> sum

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseInput filePath contents of
        Left err -> putStr (errorBundlePretty err)
        Right inp ->
            putStrLn $
                unlines
                    [ "Part 1: " <> show (sumGalaxyPaths 2 inp)
                    , "Part 2: " <> show (sumGalaxyPaths 1_000_000 inp)
                    ]