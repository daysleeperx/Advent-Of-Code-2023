-- Day 2: CubeConundrum
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Day02.CubeConundrum (solve) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser = Parsec Void String

newtype Game = Game (Int, Int, Int) deriving (Show)

instance Semigroup Game where
  Game (r, g, b) <> Game (r', g', b') = Game (r + r', g + g', b + b')

instance Monoid Game where
  mempty = Game (0, 0, 0)

parseGame :: Parser Game
parseGame =
  choice
    [ Game . (,0,0) <$> try $ decimal <* string "red",
      Game . (0,,0) <$> try $ decimal <* string "green",
      Game . (0,0,) <$> try $ decimal <* string "blue"
    ]

parseGame' :: Parser Game
parseGame' = foldl1 (<>) <$> parseGame `sepBy1` string ", "

newtype GameRecord = GameRecord (Int, Game) deriving (Show)

parseGameId :: Parser Int
parseGameId = string "Game " *> decimal <* string ": "

parseGameRecord :: Parser GameRecord
parseGameRecord = do
  gameId <- parseGameId
  games <- parseGame' `sepBy1` string "; "
  pure $ GameRecord (gameId, foldl1 (<>) games)

parseGameRecords :: Parser [GameRecord]
parseGameRecords = parseGameRecord `sepBy1` newline

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseGameRecords filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right records -> print records
