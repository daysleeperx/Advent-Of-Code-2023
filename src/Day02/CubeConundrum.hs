-- Day 2: CubeConundrum
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Day02.CubeConundrum (solve) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

symbol :: String -> Parser String
symbol = L.symbol sc

comma :: Parser String
comma = symbol ","

semicolon :: Parser String
semicolon = symbol ";"

colon :: Parser String
colon = symbol ":"

newtype Game = Game (Int, Int, Int) deriving (Show, Eq)

instance Ord Game where
  (Game (r, g, b)) <= (Game (r', g', b')) = r <= r' && g <= g' && b <= b'

instance Semigroup Game where
  Game (r, g, b) <> Game (r', g', b') = Game (max r r', max g g', max b b')

instance Monoid Game where
  mempty = Game (0, 0, 0)

parseGame :: Parser Game
parseGame = try red <|> try green <|> blue
  where
    red = Game . (,0,0) <$> integer <* string "red"
    green = Game . (0,,0) <$> integer <* string "green"
    blue = Game . (0,0,) <$> integer <* string "blue"

parseGame' :: Parser Game
parseGame' = mconcat <$> sepBy1 parseGame comma

newtype GameRecord = GameRecord (Int, [Game]) deriving (Show)

getGameId :: GameRecord -> Int
getGameId (GameRecord (idx, _)) = idx

getGames :: GameRecord -> [Game]
getGames (GameRecord (_, games)) = games

parseGameId :: Parser Int
parseGameId = lexeme (string "Game") *> integer <* colon

parseGameRecord :: Parser GameRecord
parseGameRecord = GameRecord <$> ((,) <$> parseGameId <*> sepBy1 parseGame' semicolon)

parseGameRecords :: Parser [GameRecord]
parseGameRecords = parseGameRecord `sepBy1` newline

filterPossibleGames :: Game -> [GameRecord] -> [GameRecord]
filterPossibleGames g = filter (all (<= g) . getGames)

sumPossibleGamesIds :: Game -> [GameRecord] -> Int
sumPossibleGamesIds g = sum . fmap getGameId . filterPossibleGames g

powerOfCubes :: Game -> Int
powerOfCubes (Game (r, g, b)) = r * g * b

sumPowerOfLeastCubes :: [GameRecord] -> Int
sumPowerOfLeastCubes = sum . fmap (powerOfCubes . mconcat . getGames)

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseGameRecords filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right records -> do
      print $ sumPossibleGamesIds (Game (12, 13, 14)) records
      print $ sumPowerOfLeastCubes records
