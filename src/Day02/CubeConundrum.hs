-- Day 2: CubeConundrum
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Day02.CubeConundrum (solve) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser = Parsec Void String

newtype Game = Game (Int, Int, Int) deriving (Show, Eq)

instance Ord Game where
  (Game (r, g, b)) <= (Game (r', g', b')) = r <= r' && g <= g' && b <= b'

instance Semigroup Game where
  Game (r, g, b) <> Game (r', g', b') = Game (r + r', g + g', b + b')

instance Monoid Game where
  mempty = Game (0, 0, 0)

parseGame :: Parser Game
parseGame = try red <|> try green <|> blue
  where
    red = Game . (,0,0) <$> decimal <* string " red"
    green = Game . (0,,0) <$> decimal <* string " green"
    blue = Game . (0,0,) <$> decimal <* string " blue"

parseGame' :: Parser Game
parseGame' = mconcat <$> sepBy1 parseGame (string ", ")

newtype GameRecord = GameRecord (Int, [Game]) deriving (Show)

getGameId :: GameRecord -> Int
getGameId (GameRecord (idx, _)) = idx

getGames :: GameRecord -> [Game]
getGames (GameRecord (_, games)) = games

parseGameId :: Parser Int
parseGameId = string "Game " *> decimal <* string ": "

parseGameRecord :: Parser GameRecord
parseGameRecord = GameRecord <$> ((,) <$> parseGameId <*> sepBy1 parseGame' (string "; "))

parseGameRecords :: Parser [GameRecord]
parseGameRecords = parseGameRecord `sepBy1` newline

filterPossibleGames :: Game -> [GameRecord] -> [GameRecord]
filterPossibleGames g = filter (all (<= g) . getGames)

sumPossibleGamesIds :: Game -> [GameRecord] -> Int
sumPossibleGamesIds g = sum . fmap getGameId . filterPossibleGames g

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseGameRecords filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right records -> print $ sumPossibleGamesIds (Game (12, 13, 14)) records
