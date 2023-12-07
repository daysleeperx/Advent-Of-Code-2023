-- Day 6: WaitForIt

module Day06.WaitForIt (solve) where

import Control.Applicative (liftA2)
import Data.Tuple.Extra (both)
import ParserUtils (Parser, colon, integer)
import Text.Megaparsec (choice, many, parse)
import Text.Megaparsec.Char (string)
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
parseRace = liftA2 zip parseLine parseLine

getWinningBounds :: (Double, Double) -> (Int, Int)
getWinningBounds (t, dist) = (floor r1, ceiling r2)
  where
    r1 = 0.5 * (t + sqrt (t ** 2 - 4 * dist))
    r2 = 0.5 * (t - sqrt (t ** 2 - 4 * dist))

noOfWinning :: (Int, Int) -> Int
noOfWinning = succ . uncurry (-) . getWinningBounds . both fromIntegral

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
