-- Day 3: GearRatios
{-# LANGUAGE LambdaCase #-}

module Day03.GearRatios (solve) where

import Control.Monad (guard)
import Data.Function ((&))
import Data.List (nub, (\\))
import ParserUtils (Parser, dot, integer)
import Text.Megaparsec (
    SourcePos (sourceColumn, sourceLine),
    anySingle,
    errorBundlePretty,
    getSourcePos,
    many,
    parse,
    sepBy,
    sepEndBy,
    skipMany,
    unPos,
    (<|>),
 )
import Text.Megaparsec.Char (newline)

type Position = (Int, Int)

data NumberWithRange = NumberWithRange Int Position Position deriving (Show)

data Cell = NumberCell NumberWithRange | SymbolCell Char Position deriving (Show)

getNum :: NumberWithRange -> Int
getNum (NumberWithRange n _ _) = n

parseNumberWithRange :: Parser Cell
parseNumberWithRange = do
    startPos <- getSourcePos
    num <- integer
    let start@(startY, startX) = (unPos $ sourceLine startPos, unPos $ sourceColumn startPos)
    pure $
        NumberCell
            ( NumberWithRange
                num
                start
                (startY, startX + length (show num) - 1)
            )

parseSymbol :: Parser Cell
parseSymbol = do
    pos <- getSourcePos
    symbol <- anySingle
    pure $
        SymbolCell
            symbol
            (unPos $ sourceLine pos, unPos $ sourceColumn pos)

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
partNumbers cells = [n | NumberCell n <- cells, hasAdjacentSymbol n cells]

sumPartNumbers :: [Cell] -> Int
sumPartNumbers = sum . map getNum . partNumbers

getAdjacentNumbers :: Position -> [Cell] -> [NumberWithRange]
getAdjacentNumbers (y, x) cells = do
    let positions = deltas y x
    NumberCell nwr@(NumberWithRange _ (startY, startX) (endY, endX)) <- cells
    guard $ any (\pos -> pos `elem` [(y', x') | y' <- [startY .. endY], x' <- [startX .. endX]]) positions
    pure nwr

sumGearRatios :: [Cell] -> Int
sumGearRatios cells =
    positions
        & map (`getAdjacentNumbers` cells)
        & filter ((== 2) . length)
        & map (product . map getNum)
        & sum
  where
    positions = [(y, x) | SymbolCell _ (y, x) <- cells]

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseInput filePath contents of
        Left eb -> putStr $ errorBundlePretty eb
        Right input ->
            putStrLn $
                unlines
                    [ "Part 1: " <> show (sumPartNumbers input)
                    , "Part 2: " <> show (sumGearRatios input)
                    ]
