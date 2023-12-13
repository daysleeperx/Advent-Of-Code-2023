-- Day 10: PipeMaze
{-# LANGUAGE RecordWildCards #-}

module Day10.PipeMaze (solve) where

import qualified Data.Map as M
import Data.Maybe (catMaybes)
import ParserUtils (Parser)
import Text.Megaparsec (many, oneOf, parse, sepBy1)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Error (errorBundlePretty)

type Coord = (Int, Int)

type Maze = [[Char]]

data InputType = InputType
    { start :: Coord
    , graph :: M.Map Coord [Coord]
    }
    deriving (Show)

parseRow :: Parser [Char]
parseRow = many (oneOf "|-LJ7FS.")

parseMaze :: Parser Maze
parseMaze = parseRow `sepBy1` newline

parseInput :: Parser InputType
parseInput = do
    maze <- parseMaze
    let grid = mazeToGrid maze
    let graph = mazeToGraph maze
    let start = (head . M.keys . M.filter (== 'S')) grid
    pure InputType{..}

mazeToGrid :: Maze -> M.Map Coord Char
mazeToGrid maze =
    M.fromList
        [ ((x, y), tile)
        | (y, row) <- zip [0 ..] maze
        , (x, tile) <- zip [0 ..] row
        ]

connect :: M.Map Coord Char -> (Char -> Bool) -> Coord -> Maybe Coord
connect grid c coord = M.lookup coord grid >>= \t -> if c t then Just coord else Nothing

connectNorth :: Char -> Bool
connectNorth t = t `elem` "|F7"
connectSouth :: Char -> Bool
connectSouth t = t `elem` "|LJ"
connectEast :: Char -> Bool
connectEast t = t `elem` "-7J"
connectWest :: Char -> Bool
connectWest t = t `elem` "-FL"

neighbours :: Coord -> M.Map Coord Char -> [Coord]
neighbours coord@(x, y) grid =
    case grid M.! coord of
        '|' -> applyConns [connectNorth, connectSouth] [(x, y - 1), (x, y + 1)]
        '-' -> applyConns [connectEast, connectWest] [(x + 1, y), (x - 1, y)]
        'L' -> applyConns [connectNorth, connectEast] [(x, y - 1), (x + 1, y)]
        'J' -> applyConns [connectNorth, connectWest] [(x, y - 1), (x - 1, y)]
        '7' -> applyConns [connectSouth, connectWest] [(x, y + 1), (x - 1, y)]
        'F' -> applyConns [connectSouth, connectEast] [(x, y + 1), (x + 1, y)]
        'S' ->
            applyConns
                [connectNorth, connectSouth, connectEast, connectWest]
                [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]
        _ -> []
  where
    applyConns :: [Char -> Bool] -> [Coord] -> [Coord]
    applyConns conns coords = catMaybes $ zipWith (connect grid) conns coords

mazeToGraph :: Maze -> M.Map Coord [Coord]
mazeToGraph maze =
    M.fromList
        [ (coord, neighbours coord grid)
        | coord <- M.keys grid
        , grid M.! coord /= '.'
        ]
  where
    grid = mazeToGrid maze

findLoop :: InputType -> [Coord]
findLoop InputType{..} = start : dfs [start] []
  where
    dfs :: [Coord] -> [Coord] -> [Coord]
    dfs [] _ = []
    dfs (coord : coords) visited
        | coord `elem` visited = coord : dfs coords visited
        | otherwise = dfs (coords <> graph M.! coord) (coord : visited)

getFarthestDistance :: InputType -> Int
getFarthestDistance = (`div` 2) . length . findLoop

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseInput filePath contents of
        Left eb -> putStr (errorBundlePretty eb)
        Right input ->
            putStrLn $
                unlines
                    [ "Part 1: " ++ show (getFarthestDistance input)
                    ]