-- Day 7: CamelCards

module Day07.CamelCards (solve) where

import ParserUtils (integer)
import Text.Megaparsec
import Text.Megaparsec.Char

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    putStr contents