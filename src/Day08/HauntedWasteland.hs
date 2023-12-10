-- Day 8: HauntedWasteland

module Day08.HauntedWasteland (solve) where

import Control.Applicative (liftA2)
import Control.Category ((>>>))
import Control.Monad (void)
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import ParserUtils (Parser, comma, parens)
import Text.Megaparsec (choice, count, many, parse)
import Text.Megaparsec.Char (alphaNumChar, char, newline, string)
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
parseLabel = count 3 alphaNumChar

parseLabeledMap :: Parser LabeledMap
parseLabeledMap = parens (liftA2 (,) parseLabel (comma *> parseLabel))

parseLine :: Parser (String, LabeledMap)
parseLine = liftA2 (,) parseLabel (string " = " *> parseLabeledMap)

parseInput :: Parser ([Instruction], Map.Map String LabeledMap)
parseInput = do
    instructions <- parseInstructions
    void (count 2 newline)
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

countStepsToTarget :: String -> Map.Map String LabeledMap -> [Instruction] -> Int
countStepsToTarget start labeledMap =
    cycle
        >>> scanl (go labeledMap) start
        >>> takeWhile (not . isSuffixOf "Z")
        >>> length

countStepsToTargetSimul :: Map.Map String LabeledMap -> [Instruction] -> Int
countStepsToTargetSimul labeledMap instructions =
    foldl1
        lcm
        [ countStepsToTarget s labeledMap instructions
        | s <- Map.keys labeledMap
        , "A" `isSuffixOf` s
        ]

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseInput filePath contents of
        Left eb -> putStr (errorBundlePretty eb)
        Right (instructions, labeledMaps) ->
            putStrLn $
                unlines
                    [ "Part 1: " <> show (countStepsToTarget "AAA" labeledMaps instructions)
                    , "Part 2: " <> show (countStepsToTargetSimul labeledMaps instructions)
                    ]