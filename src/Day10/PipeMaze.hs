-- Day 10: PipeMaze
{-# LANGUAGE RecordWildCards #-}

module Day10.PipeMaze (solve) where

import Control.Applicative (liftA2)
import qualified Data.Map as M
import ParserUtils (Parser)
import Text.Megaparsec (many, oneOf, parse, sepBy1)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Error (errorBundlePretty)

data Dir = N | S | E | W deriving (Show)

type Coord = (Int, Int)
type Grid = M.Map Coord Char

data InputType = InputType
    { start :: Coord
    , grid :: Grid
    }
    deriving (Show)

parseRow :: Parser [Char]
parseRow = many (oneOf "|-LJ7FS.")

parseMaze :: Parser Grid
parseMaze = mazeToGrid <$> parseRow `sepBy1` newline

parseInput :: Parser InputType
parseInput = do
    grid <- parseMaze
    let start = (head . M.keys . M.filter (== 'S')) grid
    pure InputType{..}

mazeToGrid :: [[Char]] -> Grid
mazeToGrid maze =
    M.fromList
        [ ((x, y), tile)
        | (y, row) <- zip [0 ..] maze
        , (x, tile) <- zip [0 ..] row
        ]

connect :: Grid -> (Char -> Bool) -> Coord -> Maybe Coord
connect grid c coord = M.lookup coord grid >>= \t -> if c t then Just coord else Nothing

connectNorth :: Char -> Bool
connectNorth = (`elem` "|F7S")
connectSouth :: Char -> Bool
connectSouth = (`elem` "|LJS")
connectEast :: Char -> Bool
connectEast = (`elem` "-7JS")
connectWest :: Char -> Bool
connectWest = (`elem` "-FLS")

traverseL :: Grid -> Coord -> Dir -> Maybe [Coord]
traverseL grid (x, y) dir = case dir of
    E ->
        connect grid connectEast (x + 1, y) >>= \c -> case grid M.! c of
            '-' -> doTraverseL grid c E
            '7' -> doTraverseL grid c S
            'J' -> doTraverseL grid c N
            _ -> Just [c]
    W ->
        connect grid connectWest (x - 1, y) >>= \c -> case grid M.! c of
            '-' -> doTraverseL grid c W
            'F' -> doTraverseL grid c S
            'L' -> doTraverseL grid c N
            _ -> Just [c]
    N ->
        connect grid connectNorth (x, y - 1) >>= \c -> case grid M.! c of
            '|' -> doTraverseL grid c N
            'F' -> doTraverseL grid c E
            '7' -> doTraverseL grid c W
            _ -> Just [c]
    S ->
        connect grid connectSouth (x, y + 1) >>= \c -> case grid M.! c of
            '|' -> doTraverseL grid c S
            'J' -> doTraverseL grid c W
            'L' -> doTraverseL grid c E
            _ -> Just [c]
  where
    doTraverseL g c d = liftA2 (:) (Just c) (traverseL g c d)

getFarthestDistance :: InputType -> Int
getFarthestDistance InputType{..} = maybe 0 length (traverseL grid start S) `div` 2

{--https://en.wikipedia.org/wiki/Shoelace_formula-}

shoelace :: [Coord] -> Int
shoelace coords = abs . sum . zipWith (\(x, y) (x', y') -> (x - x') * (y + y')) coords $ tail coords

getEnclosedInLoop :: InputType -> Int
getEnclosedInLoop InputType{..} =
    case traverseL grid start S of
        Nothing -> 0
        Just path -> (shoelace path - length path + 3) `div` 2

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseInput filePath contents of
        Left eb -> putStr (errorBundlePretty eb)
        Right input ->
            putStrLn $
                unlines
                    [ "Part 1: " ++ show (getFarthestDistance input)
                    , "Part 2: " ++ show (getEnclosedInLoop input)
                    ]