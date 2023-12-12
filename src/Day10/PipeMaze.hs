-- Day 10: PipeMaze
{-# LANGUAGE RecordWildCards #-}

module Day10.PipeMaze (solve) where

import Control.Monad (forM_)
import ParserUtils (Parser)
import Text.Megaparsec (choice, many, parse, sepBy1)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Error (errorBundlePretty)

type Coord = (Int, Int)

data Tile = Empty | Start | NS | EW | NE | NW | SW | SE deriving (Show, Eq)

type Maze = [[Tile]]

parseTile :: Parser Tile
parseTile =
    choice
        [ Empty <$ char '.'
        , Start <$ char 'S'
        , NS <$ char '|'
        , EW <$ char '-'
        , NE <$ char 'L'
        , NW <$ char 'J'
        , SW <$ char '7'
        , SE <$ char 'F'
        ]

parseRow :: Parser [Tile]
parseRow = many parseTile

parseMaze :: Parser Maze
parseMaze = parseRow `sepBy1` newline

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseMaze filePath contents of
        Left eb -> putStr (errorBundlePretty eb)
        Right maze -> forM_
            [ (x, y)
            | x <- [0 .. length (head maze)]
            , y <- [0 .. length maze]
            ]
            $ \coord -> do
                print coord