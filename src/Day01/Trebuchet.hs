-- Day 1: Trebuchet

module Day01.Trebuchet (solve) where

import Data.Char (digitToInt)
import Data.Functor (($>))
import ParserUtils (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char

parseDigit :: Parser Int
parseDigit =
  choice
    [ string "one" $> 1,
      string "two" $> 2,
      string "three" $> 3,
      string "four" $> 4,
      string "five" $> 5,
      string "six" $> 6,
      string "seven" $> 7,
      string "eight" $> 8,
      string "nine" $> 9,
      digitToInt <$> digitChar
    ]

parseDigitReversed :: Parser Int
parseDigitReversed =
  choice
    [ string (reverse "one") $> 1,
      string (reverse "two") $> 2,
      string (reverse "three") $> 3,
      string (reverse "four") $> 4,
      string (reverse "five") $> 5,
      string (reverse "six") $> 6,
      string (reverse "seven") $> 7,
      string (reverse "eight") $> 8,
      string (reverse "nine") $> 9,
      digitToInt <$> digitChar
    ]

parseFirstDigit :: Parser Int -> Parser Int
parseFirstDigit = skipManyTill letterChar

parseLine :: String -> (Int, Int)
parseLine s = (firstDigit, lastDigit)
  where
    firstDigit = case parse (parseFirstDigit parseDigit) "" s of
      Left _ -> 0
      Right x -> x
    lastDigit = case parse (parseFirstDigit parseDigitReversed) "" (reverse s) of
      Left _ -> 0
      Right x -> x

toTwoDigit :: (Int, Int) -> Int
toTwoDigit (a, b) = a * 10 + b

calibrationValue :: [(Int, Int)] -> Int
calibrationValue = sum . fmap toTwoDigit

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  print $ (calibrationValue . fmap parseLine . lines) contents
