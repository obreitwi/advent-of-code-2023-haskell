{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( part0,
    part1,
    part2,
  )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor
import Data.List (group, intersect, sort, sortBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Internal.Fusion.Common (findBy)
import Debug.Trace
import Prelude hiding (takeWhile)

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 6440): "
  either error part1inner $ parseOnly parseInput1 s
  putStr "Part 2 debug (expecting 71503): "
  either error part2inner $ parseOnly parseInput2 s

-- putStr "Part 2 debug (expecting 46): "
-- either error part2inner $ parseOnly parseInput2 s

part1 :: IO ()
part1 = do
  s <- TIO.readFile "input.txt"
  putStr "Part 1: "
  either error part1inner $ parseOnly parseInput1 s

part2 :: IO ()
part2 = do
  s <- TIO.readFile "input.txt"
  putStr "Part 2: "
  either error part2inner $ parseOnly parseInput2 s

part1inner :: Input1 -> IO ()
part1inner input = do
  -- mapM_ print input
  let sorted = sortOn (\(h, _) -> (getType h, h)) input
  -- print sorted
  print . getPoints $ sorted

-- print . product . map getPossibilities $ input

part2inner :: Input2 -> IO ()
part2inner input = do
  print input

-- print . getPossibilities $ input

type Input1 = [(Hand, Bid)]

type Input2 = ()

parseInput1 :: Parser Input1
parseInput1 = parseGame

parseInput2 :: Parser Input2
parseInput2 = return ()

type Hand = [Card]

data Card
  = CardA
  | CardK
  | CardQ
  | CardJ
  | CardT
  | Card9
  | Card8
  | Card7
  | Card6
  | Card5
  | Card4
  | Card3
  | Card2
  deriving (Show, Eq, Ord)

data HandType
  = FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
  deriving (Show, Eq, Ord)

type Bid = Int

parseCard :: Parser Card
parseCard =
  char 'A' $> CardA
    <|> char 'K' $> CardK
    <|> char 'Q' $> CardQ
    <|> char 'J' $> CardJ
    <|> char 'T' $> CardT
    <|> char '9' $> Card9
    <|> char '8' $> Card8
    <|> char '7' $> Card7
    <|> char '6' $> Card6
    <|> char '5' $> Card5
    <|> char '4' $> Card4
    <|> char '3' $> Card3
    <|> char '2' $> Card2

parseHand :: Parser Hand
parseHand = count 5 parseCard

parseHandBid :: Parser (Hand, Bid)
parseHandBid = do
  hand <- parseHand
  skipSpace
  bid <- decimal
  return (hand, bid)

parseGame :: Parser [(Hand, Bid)]
parseGame = sepBy1' parseHandBid endOfLine

getType :: Hand -> HandType
getType [] = error "empty hand"
getType cards = case sortBy (comparing Down) . M.elems . countCards $ cards of
  [5] -> FiveOfAKind
  4 : [1] -> FourOfAKind
  3 : [2] -> FullHouse
  3 : 1 : [1] -> ThreeOfAKind
  2 : 2 : [1] -> TwoPair
  2 : 1 : 1 : [1] -> OnePair
  1 : 1 : 1 : 1 : [1] -> HighCard
  _ -> error "invalid hand"

countCards :: Hand -> M.Map Card Int
countCards = go M.empty
  where
    go :: M.Map Card Int -> [Card] -> M.Map Card Int
    go mp [] = mp
    go mp (c : cc) = go (M.insertWith (+) c 1 mp) cc

getPoints :: [(Hand, Bid)] -> Int
getPoints [] = 0
getPoints ((_, bid) : remainder) = bid * (1 + length remainder) + getPoints remainder
