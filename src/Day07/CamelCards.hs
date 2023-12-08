-- Day 7: CamelCards
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Day07.CamelCards (solve) where

import Control.Applicative (liftA2)
import Control.Category ((>>>))
import Data.List (group, nub, sort, sortBy)
import Data.List.Extra (replace)
import Data.Ord (comparing)
import ParserUtils (Parser, integer)
import Text.Megaparsec (choice, errorBundlePretty, many, parse)
import Text.Megaparsec.Char (char, space)

data Card = Jack | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Queen | King | Ace
    deriving (Eq, Ord, Show)

data HandRank = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
    deriving (Eq, Ord, Show)

newtype Hand = Hand ([Card], HandRank) deriving (Eq, Show)

instance Ord Hand where
    compare :: Hand -> Hand -> Ordering
    (Hand (cards, rank)) `compare` (Hand (cards', rank')) =
        case rank `compare` rank' of
            EQ -> compare cards cards'
            LT -> LT
            GT -> GT

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
parseHand = wildcardJ . Hand . assignHandRank <$> many parseCard

parseHandLine :: Parser (Hand, Int)
parseHandLine = liftA2 (,) parseHand (space >> integer)

parseHands :: Parser [(Hand, Int)]
parseHands = many parseHandLine

assignHandRank :: [Card] -> ([Card], HandRank)
assignHandRank cards =
    ( \case
        [5] -> (cards, FiveOfAKind)
        [1, 4] -> (cards, FourOfAKind)
        [2, 3] -> (cards, FullHouse)
        [1, 1, 3] -> (cards, ThreeOfAKind)
        [1, 2, 2] -> (cards, TwoPairs)
        [1, 1, 1, 2] -> (cards, OnePair)
        _ -> (cards, HighCard)
    )
        $ (sort . map length . group . sort) cards

wildcardJ :: Hand -> Hand
wildcardJ h@(Hand (cards, _))
    | Jack `elem` cards, not (all (== Jack) cards) = Hand (cards, rank')
    | otherwise = h
  where
    cards' = [replace [Jack] [c] cards | c <- nub cards, c /= Jack]
    Hand (_, rank') = maximum $ map (Hand . assignHandRank) cards'

totalWinnings :: [(Hand, Int)] -> Int
totalWinnings =
    sortBy (comparing fst)
        >>> map snd
        >>> zipWith (*) [1 ..]
        >>> sum

solve :: FilePath -> IO ()
solve filePath = do
    contents <- readFile filePath
    case parse parseHands filePath contents of
        Left eb -> putStr (errorBundlePretty eb)
        Right hands -> print $ totalWinnings hands