-- Day 3: GearRatios
{-# LANGUAGE LambdaCase #-}

module Day03.GearRatios (solve) where

import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (nub, (\\))
import ParserUtils (Parser, dot, integer)
import Text.Megaparsec
import Text.Megaparsec.Char

type Position = (Int, Int)

data NumberWithRange = NumberWithRange Int Position Position deriving (Show)

data Cell = NumberCell NumberWithRange | SymbolCell Char Position deriving (Show)

getNum :: NumberWithRange -> Int
getNum (NumberWithRange n _ _) = n

parseNumberWithRange :: Parser Cell
parseNumberWithRange = do
  startPos <- getSourcePos
  num <- integer
  endPos <- getSourcePos
  pure $
    NumberCell
      ( NumberWithRange
          num
          (unPos $ sourceLine startPos, unPos $ sourceColumn startPos)
          (unPos $ sourceLine endPos, unPos (sourceColumn endPos) - 1)
      )

parseSymbol :: Parser Cell
parseSymbol = do
  pos <- getSourcePos
  symbol <- satisfy (\c -> not (isDigit c || c == '.'))
  pure $ SymbolCell symbol (unPos $ sourceLine pos, unPos $ sourceColumn pos)

parseEngineElement :: Parser Cell
parseEngineElement = parseNumberWithRange <|> parseSymbol

parseInputRow :: Parser [Cell]
parseInputRow = skipMany dot *> parseEngineElement `sepEndBy` many dot

parseInput :: Parser [Cell]
parseInput = concat <$> parseInputRow `sepBy` newline

deltas :: Int -> Int -> [(Int, Int)]
deltas x y = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

hasAdjacentSymbol :: NumberWithRange -> [Cell] -> Bool
hasAdjacentSymbol (NumberWithRange _ (startY, startX) (endY, endX)) cells = any isAdjacentSymbol allAdjacentPositions
  where
    positions = [(y, x) | y <- [startY .. endY], x <- [startX .. endX]]
    allAdjacentPositions = (\\ positions) . nub . concatMap (uncurry deltas) $ positions
    isAdjacentSymbol pos =
      any
        ( \case
            SymbolCell _ p -> p == pos
            _ -> False
        )
        cells

partNumbers :: [Cell] -> [NumberWithRange]
partNumbers cells = do
  NumberCell n <- cells
  guard (hasAdjacentSymbol n cells)
  pure n

sumPartNumbers :: [Cell] -> Int
sumPartNumbers = sum . map getNum . partNumbers

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr $ errorBundlePretty eb
    Right input -> print $ sumPartNumbers input
