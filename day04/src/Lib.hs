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
import Data.List (intersect)
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 13): "
  either error part1inner $ parseOnly parseCards s
  putStr "Part 2 debug (expecting 30): "
  either error part2inner $ parseOnly parseCards s

part1 :: IO ()
part1 = do
  s <- TIO.readFile "input.txt"
  putStr "Part 1: "
  either error part1inner $ parseOnly parseCards s

part2 :: IO ()
part2 = do
  s <- TIO.readFile "input.txt"
  putStr "Part 2: "
  either error part2inner $ parseOnly parseCards s

part1inner :: [Card] -> IO ()
part1inner cards = do
  print . sum . map points $ cards

part2inner :: [Card] -> IO ()
part2inner cards = do
  print . sum . countWinningCards $ cards


data Card = Card
  { cardId :: Int,
    winning :: [Int],
    numbers :: [Int]
  }
  deriving (Show)

points :: Card -> Int
points = go . numMatches
  where
   go 0 = 0
   go n = shiftL 1 (n-1)

numMatches :: Card -> Int
numMatches Card {..} = length $ winning `intersect` numbers

countWinningCards :: [Card] -> [Int]
countWinningCards = go [] (repeat 1)
  where
    go :: [Int] -> [Int] -> [Card] -> [Int]
    go total _ [] = total
    go total (co:copies) (ca:cards) = go (co:total) (incFirstNumBy (numMatches ca) co copies) cards
    go _ [] _ = undefined

incFirstNumBy :: Int -> Int -> [Int] -> [Int]
incFirstNumBy 0 _ remaining = remaining
incFirstNumBy n by (r:remaining) = (r+by):incFirstNumBy (n-1) by remaining
incFirstNumBy _ _ [] = undefined

ensureParsed :: Result a -> Result a
ensureParsed (Partial p) = ensureParsed $ feed (Partial p) ""
ensureParsed r = r

parseCards :: Parser [Card]
parseCards = sepBy1' parseCard endOfLine <* endOfLine

parseCard :: Parser Card
parseCard = do
  _ <- "Card"
  skipSpace
  cardId <- decimal
  _ <- ":"
  skipSpace
  winning <- sepBy decimal (many1' (char ' '))
  skipSpace
  _ <- "|"
  skipSpace
  numbers <- sepBy decimal (many1' (char ' '))
  return $ Card {cardId, winning, numbers}
