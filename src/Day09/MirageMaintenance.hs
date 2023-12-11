-- Day 9: MirageMaintenance

module Day09.MirageMaintenance (solve) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import ParserUtils (Parser, signedInteger)
import Text.Megaparsec (eof, errorBundlePretty, many, parse, sepBy1)
import Text.Megaparsec.Char (eol, newline)

parseHistory :: Parser [Int]
parseHistory = many signedInteger <* (void eol <|> eof)

parseReport :: Parser [[Int]]
parseReport = parseHistory `sepBy1` newline

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseReport filePath contents of
        Left eb -> putStr (errorBundlePretty eb)
        Right report -> print report