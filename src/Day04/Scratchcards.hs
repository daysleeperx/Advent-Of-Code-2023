-- Day 4: Scratchcards

module Day04.Scratchcards (solve) where

import Control.Monad (void)
import Data.List (intersect)
import ParserUtils (Parser, colon, integer, lexeme, pipe)
import Text.Megaparsec
import Text.Megaparsec.Char

type Card = ([Int], [Int])

parseNums :: Parser [Int]
parseNums = many integer

parseCardId :: Parser Int
parseCardId = lexeme (string "Card") *> integer <* colon

parseCard :: Parser Card
parseCard = do
  void parseCardId
  winning <- parseNums
  void pipe
  nums <- parseNums
  pure (winning, nums)

parseCards :: Parser [Card]
parseCards = many parseCard

countWins :: Card -> Int
countWins = length . uncurry intersect

totalPoints :: Card -> Int
totalPoints c
  | n == 0 = 0
  | otherwise = 2 ^ (n - 1)
  where
    n = countWins c

totalCards :: [Card] -> Int
totalCards = sum . foldr ((\w xs -> 1 + sum (take w xs) : xs) . countWins) []

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseCards filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right cards ->
      putStrLn $
        unlines
          [ "Part 1: " <> show (sum $ map totalPoints cards),
            "Part 2: " <> show (totalCards cards)
          ]
