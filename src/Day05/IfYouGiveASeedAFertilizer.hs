-- Day 5: IfYouGiveASeedAFertilizer
{-# LANGUAGE LambdaCase #-}

module Day05.IfYouGiveASeedAFertilizer (solve) where

import Control.Applicative (liftA2)
import Control.Category ((>>>))
import Data.List (unfoldr)
import ParserUtils (Parser, colon, integer)
import Text.Megaparsec (choice, errorBundlePretty, many, parse)
import Text.Megaparsec.Char (string)

data Range = Range
    { target :: Int
    , source :: Int
    , step :: Int
    }
    deriving (Show)

data MapType
    = SeedToSoil
    | SoilToFertilizer
    | FertilizerToWater
    | WaterToLight
    | LightToTemperature
    | TemperatureToHumidity
    | HumidityToLocation
    deriving (Show, Read, Eq)

data RangeMap = RangeMap
    { mapType :: MapType
    , ranges :: [Range]
    }
    deriving (Show)

parseSeeds :: Parser [Int]
parseSeeds =
    string "seeds"
        *> colon
        *> many integer

parseRange :: Parser Range
parseRange =
    Range
        <$> integer
        <*> integer
        <*> integer

parseRangeMap :: Parser RangeMap
parseRangeMap =
    RangeMap
        <$> choice
            [ SeedToSoil <$ string "seed-to-soil map"
            , SoilToFertilizer <$ string "soil-to-fertilizer map"
            , FertilizerToWater <$ string "fertilizer-to-water map"
            , WaterToLight <$ string "water-to-light map"
            , LightToTemperature <$ string "light-to-temperature map"
            , TemperatureToHumidity <$ string "temperature-to-humidity map"
            , HumidityToLocation <$ string "humidity-to-location map"
            ]
        <* colon
        <*> many parseRange

type InputType = ([Int], [RangeMap])

parseInput :: Parser InputType
parseInput = liftA2 (,) parseSeeds (many parseRangeMap)

convertNum :: Int -> Range -> Int
convertNum num Range{target = t, source = src, step = s}
    | src <= num, num < src + s = num - src + t
    | otherwise = num

convertNums :: Int -> [Range] -> Int
convertNums seed [] = seed
convertNums seed (r : rs)
    | converted == seed = convertNums seed rs
    | otherwise = converted
  where
    converted = convertNum seed r

findLocation :: Int -> [RangeMap] -> Int
findLocation seed = foldl convertNums seed . map ranges

findMinLocation :: [RangeMap] -> [Int] -> Int
findMinLocation maps = minimum . map (`findLocation` maps)

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile ((== n) . length) . unfoldr (Just . splitAt n)

expandSeedRanges :: [[Int]] -> [Int]
expandSeedRanges =
    concatMap
        ( \case
            [a, b] -> [a .. a + b - 1]
            _ -> []
        )

findMinLocation' :: [RangeMap] -> [Int] -> Int
findMinLocation' maps =
    chunks 2
        >>> expandSeedRanges
        >>> map (`findLocation` maps)
        >>> minimum

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseInput filePath contents of
        Left eb -> putStr (errorBundlePretty eb)
        Right (seeds, maps) ->
            putStrLn $
                unlines
                    [ "Part 1: " <> show (findMinLocation maps seeds)
                    , "Part 2: " <> show (findMinLocation' maps seeds)
                    ]
