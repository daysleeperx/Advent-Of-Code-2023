-- Day 7: CamelCards
{-# LANGUAGE InstanceSigs #-}

module Day07.CamelCards (solve) where

import Control.Applicative (liftA2)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing)
import ParserUtils (Parser, integer)
import Text.Megaparsec (choice, errorBundlePretty, many, parse)
import Text.Megaparsec.Char (char, space)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Show)

data HandRank = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
    deriving (Eq, Ord, Show)

newtype Hand = Hand ([Card], HandRank) deriving (Eq, Show)

parseCard :: Parser Card
parseCard =
    choice
        [ Two <$ char '2'
        , Three <$ char '3'
        , Four <$ char '4'
        , Five <$ char '5'
        , Six <$ char '6'
        , Seven <$ char '7'
        , Eight <$ char '8'
        , Nine <$ char '9'
        , Ten <$ char 'T'
        , Jack <$ char 'J'
        , Queen <$ char 'Q'
        , King <$ char 'K'
        , Ace <$ char 'A'
        ]

parseHand :: Parser Hand
parseHand = Hand . assignHandRank <$> many parseCard
  where
    assignHandRank :: [Card] -> ([Card], HandRank)
    assignHandRank cards =
        case (length . group . sort) cards of
            1 -> (cards, FiveOfAKind)
            2 -> if any ((== 4) . length) (group $ sort cards) then (cards, FourOfAKind) else (cards, FullHouse)
            3 -> if any ((== 3) . length) (group $ sort cards) then (cards, ThreeOfAKind) else (cards, TwoPairs)
            4 -> (cards, OnePair)
            5 -> (cards, HighCard)
            _ -> error "Invalid hand"

parseHandLine :: Parser (Hand, Int)
parseHandLine = liftA2 (,) parseHand (space >> integer)

parseHands :: Parser [(Hand, Int)]
parseHands = many parseHandLine

instance Ord Hand where
    compare :: Hand -> Hand -> Ordering
    (Hand (cards, rank)) `compare` (Hand (cards', rank')) =
        case rank `compare` rank' of
            EQ -> compare cards cards'
            LT -> LT
            GT -> GT

totalWinnings :: [(Hand, Int)] -> Int
totalWinnings = sum . zipWith (*) [1 ..] . map snd . sortBy (comparing fst)

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseHands filePath contents of
        Left eb -> putStr (errorBundlePretty eb)
        Right hands -> print $ totalWinnings hands