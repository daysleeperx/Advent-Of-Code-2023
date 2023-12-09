-- Day 8: HauntedWasteland

module Day08.HauntedWasteland (solve) where

import Control.Applicative (liftA2)
import Control.Category ((>>>))
import Control.Monad (void)
import qualified Data.Map as Map
import ParserUtils (Parser, comma, parens)
import Text.Megaparsec (choice, count, many, parse)
import Text.Megaparsec.Char (char, letterChar, newline, string)
import Text.Megaparsec.Error (errorBundlePretty)

data Instruction = GoLeft | GoRight deriving (Show, Eq)

type LabeledMap = (String, String)

parseInstruction :: Parser Instruction
parseInstruction =
    choice
        [ GoLeft <$ char 'L'
        , GoRight <$ char 'R'
        ]

parseInstructions :: Parser [Instruction]
parseInstructions = many parseInstruction

parseLabel :: Parser String
parseLabel = count 3 letterChar

parseLabeledMap :: Parser LabeledMap
parseLabeledMap = parens (liftA2 (,) parseLabel (comma >> parseLabel))

parseLine :: Parser (String, LabeledMap)
parseLine = liftA2 (,) parseLabel (string " = " >> parseLabeledMap)

parseInput :: Parser ([Instruction], Map.Map String LabeledMap)
parseInput = do
    instructions <- parseInstructions
    void (newline >> newline)
    labeledMaps <- Map.fromList <$> many parseLine
    pure (instructions, labeledMaps)

go :: Map.Map String LabeledMap -> String -> Instruction -> String
go labeledMap current instruction =
    let
        (left, right) = labeledMap Map.! current
     in
        case instruction of
            GoLeft -> left
            GoRight -> right

countStepsToTarget :: Map.Map String LabeledMap -> [Instruction] -> Int
countStepsToTarget labeledMap =
    cycle
        >>> scanl (go labeledMap) "AAA"
        >>> takeWhile (/= "ZZZ")
        >>> length

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseInput filePath contents of
        Left eb -> putStr (errorBundlePretty eb)
        Right (instructions, labeledMaps) ->
            print $ countStepsToTarget labeledMaps instructions