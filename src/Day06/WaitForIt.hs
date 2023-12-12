-- Day 6: WaitForIt
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day06.WaitForIt (solve) where

import Control.Applicative (liftA2)
import Data.Tuple.Extra (both)
import ParserUtils (Parser, colon, integer)
import Text.Megaparsec (choice, many, parse)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Error (errorBundlePretty)

type Race = [(Int, Int)]

parseNums :: Parser [Int]
parseNums = many integer

parseLine :: Parser [Int]
parseLine =
    choice
        [ string "Time"
        , string "Distance"
        ]
        <* colon
        *> parseNums

parseRace :: Parser Race
parseRace = liftA2 zip (parseLine <* newline) parseLine

getWinningBounds :: (Int, Int) -> (Int, Int)
getWinningBounds (t, dist) = (floor high, ceiling low)
  where
    delta = t * t - 4 * dist
    high = 0.5 * (fromIntegral t + sqrt (fromIntegral delta))
    low = 0.5 * (fromIntegral t - sqrt (fromIntegral delta))

noOfWinning :: (Int, Int) -> Int
noOfWinning = succ . uncurry (-) . getWinningBounds

productOfWinning :: Race -> Int
productOfWinning = product . map noOfWinning

collapse :: [Int] -> Int
collapse = read . concatMap show

collapseAndGetNoOfWinning :: Race -> Int
collapseAndGetNoOfWinning = noOfWinning . both collapse . unzip

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseRace filePath contents of
        Left eb -> putStr (errorBundlePretty eb)
        Right input ->
            putStrLn $
                unlines
                    [ "Part 1: " <> show (productOfWinning input)
                    , "Part 2: " <> show (collapseAndGetNoOfWinning input)
                    ]
