-- Day 10: PipeMaze

module Day10.PipeMaze (solve) where

import Control.Monad (forM_)
import qualified Data.Map as M
import ParserUtils (Parser)
import Text.Megaparsec (choice, many, parse, sepBy1)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Error (errorBundlePretty)

type Coord = (Int, Int)

type Maze = [[Char]]

parseRow :: Parser [Char]
parseRow =
    (many . choice)
        [ char '|'
        , char '-'
        , char 'L'
        , char 'J'
        , char '7'
        , char 'F'
        , char 'S'
        , char '.'
        ]

parseMaze :: Parser Maze
parseMaze = parseRow `sepBy1` newline

mazeToGrid :: Maze -> M.Map Coord Char
mazeToGrid maze =
    M.fromList
        [ ((x, y), tile)
        | (y, row) <- zip [0 ..] maze
        , (x, tile) <- zip [0 ..] row
        ]

inBounds :: Maze -> Coord -> Bool
inBounds maze (x, y) = 0 <= x && x < length (head maze) && 0 <= y && y < length maze

neighbours :: Coord -> M.Map Coord Char -> Maze -> [Coord]
neighbours coord@(x, y) grid maze =
    case grid M.! coord of
        '|' ->
            ([(x, y - 1) | inBounds maze (x, y - 1), grid M.! (x, y - 1) `elem` "|F7"])
                <> ([(x, y + 1) | inBounds maze (x, y + 1), grid M.! (x, y + 1) `elem` "|LJ"])
        '-' ->
            [(x - 1, y) | inBounds maze (x - 1, y), grid M.! (x - 1, y) `elem` "-FL"]
                <> [(x + 1, y) | inBounds maze (x + 1, y), grid M.! (x + 1, y) `elem` "-7J"]
        'L' ->
            [(x, y - 1) | inBounds maze (x, y - 1), grid M.! (x, y - 1) `elem` "|F7"]
                <> [(x + 1, y) | inBounds maze (x + 1, y), grid M.! (x + 1, y) `elem` "-7J"]
        'J' ->
            [(x, y - 1) | inBounds maze (x, y - 1), grid M.! (x, y - 1) `elem` "|F7"]
                <> [(x - 1, y) | inBounds maze (x - 1, y), grid M.! (x - 1, y) `elem` "-FL"]
        '7' ->
            [(x, y + 1) | inBounds maze (x, y + 1), grid M.! (x, y + 1) `elem` "|LJ"]
                <> [(x - 1, y) | inBounds maze (x - 1, y), grid M.! (x - 1, y) `elem` "-FL"]
        'F' ->
            [(x, y + 1) | inBounds maze (x, y + 1), grid M.! (x, y + 1) `elem` "|LJ"]
                <> [(x + 1, y) | inBounds maze (x + 1, y), grid M.! (x + 1, y) `elem` "-7J"]
        'S' ->
            [(x, y - 1) | inBounds maze (x, y - 1), grid M.! (x, y - 1) `elem` "|F7"]
                <> [(x, y + 1) | inBounds maze (x, y + 1), grid M.! (x, y + 1) `elem` "|LJ"]
                <> [(x - 1, y) | inBounds maze (x - 1, y), grid M.! (x - 1, y) `elem` "-FL"]
                <> [(x + 1, y) | inBounds maze (x + 1, y), grid M.! (x + 1, y) `elem` "-7J"]
        _ -> []

mazeToGraph :: Maze -> M.Map Coord [Coord]
mazeToGraph maze = M.fromList [(coord, neighbours coord grid maze) | coord <- M.keys grid]
  where
    grid = mazeToGrid maze

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseMaze filePath contents of
        Left eb -> putStr (errorBundlePretty eb)
        Right maze -> forM_ (M.toList $ mazeToGraph maze) print