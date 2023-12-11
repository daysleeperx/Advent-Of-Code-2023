-- Day 9: MirageMaintenance

module Day09.MirageMaintenance (solve) where

import Control.Arrow ((&&&))
import Data.List (unfoldr)
import ParserUtils (Parser, signedInteger)
import Text.Megaparsec (errorBundlePretty, many, parse, sepBy1)
import Text.Megaparsec.Char (newline)

parseHistory :: Parser [Int]
parseHistory = many signedInteger

parseReport :: Parser [[Int]]
parseReport = parseHistory `sepBy1` newline

windows :: Int -> [a] -> [[a]]
windows n = takeWhile ((== n) . length) . unfoldr (Just . (take n &&& tail))

next :: [Int] -> [Int]
next = map (uncurry (-) . (last &&& head)) . windows 2

expandHistory :: [Int] -> [[Int]]
expandHistory = takeWhile (not . all (== 0)) . iterate next

extrapolateFW :: [Int] -> Int
extrapolateFW = sum . map last . expandHistory

totalExtrapolateFw :: [[Int]] -> Int
totalExtrapolateFw = sum . map extrapolateFW

extrapolateBw :: [Int] -> Int
extrapolateBw = foldr1 (-) . map head . expandHistory

totalExtrapolateBw :: [[Int]] -> Int
totalExtrapolateBw = sum . map extrapolateBw

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseReport filePath contents of
        Left eb -> putStr (errorBundlePretty eb)
        Right report ->
            putStrLn $
                unlines
                    [ "Part 1: " ++ show (totalExtrapolateFw report)
                    , "Part 2: " ++ show (totalExtrapolateBw report)
                    ]