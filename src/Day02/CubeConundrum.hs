-- Day 2: CubeConundrum
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day02.CubeConundrum (solve) where

import ParserUtils (Parser, colon, comma, integer, lexeme, semicolon)
import Text.Megaparsec (
    MonadParsec (try),
    errorBundlePretty,
    parse,
    sepBy1,
    (<|>),
 )
import Text.Megaparsec.Char (newline, string)

newtype Game = Game (Int, Int, Int) deriving (Show, Eq)

instance Semigroup Game where
    (<>) :: Game -> Game -> Game
    Game (r, g, b) <> Game (r', g', b') = Game (max r r', max g g', max b b')

instance Monoid Game where
    mempty :: Game
    mempty = Game (0, 0, 0)

parseGame :: Parser Game
parseGame = try red <|> try green <|> blue
  where
    red = Game . (,0,0) <$> integer <* string "red"
    green = Game . (0,,0) <$> integer <* string "green"
    blue = Game . (0,0,) <$> integer <* string "blue"

parseGame' :: Parser Game
parseGame' = mconcat <$> parseGame `sepBy1` comma

data GameRecord = Gamerecord
    { getGameId :: Int
    , getGames :: [Game]
    }
    deriving (Show)

parseGameId :: Parser Int
parseGameId =
    lexeme (string "Game")
        *> integer
        <* colon

parseGameRecord :: Parser GameRecord
parseGameRecord = do
    getGameId <- parseGameId
    getGames <- parseGame' `sepBy1` semicolon
    pure Gamerecord{..}

parseGameRecords :: Parser [GameRecord]
parseGameRecords = parseGameRecord `sepBy1` newline

isPossibleGame :: Game -> Game -> Bool
isPossibleGame g g' = g <> g' == g

filterPossibleGames :: Game -> [GameRecord] -> [GameRecord]
filterPossibleGames g = filter (all (isPossibleGame g) . getGames)

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
        Right records ->
            putStrLn $
                unlines
                    [ "Part 1: " <> show (sumPossibleGamesIds (Game (12, 13, 14)) records)
                    , "Part 2: " <> show (sumPowerOfLeastCubes records)
                    ]
