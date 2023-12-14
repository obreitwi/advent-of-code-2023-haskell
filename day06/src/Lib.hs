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
import Data.List (intersect, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Internal.Fusion.Common (findBy)
import Debug.Trace
import Prelude hiding (takeWhile)

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 288): "
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

type Time = Int

type Distance = Int

type Input1 = [(Time, Distance)]
type Input2 = (Time, Distance)

part1inner :: Input1 -> IO ()
part1inner input = do
  -- print input
  print . product . map getPossibilities $ input

part2inner :: Input2 -> IO ()
part2inner input = do
  -- print input
  print . getPossibilities $ input

parseInput1 :: Parser Input1
parseInput1 = do
  _ <- "Time:"
  skipSpace
  times <- sepBy1' decimal skipSpace
  endOfLine
  _ <- "Distance:"
  skipSpace
  distances <- sepBy1' decimal skipSpace
  return $ zip times distances

parseInput2 :: Parser Input2
parseInput2 = do
  _ <- "Time:"
  skipSpace
  times <- sepBy1' decimal skipSpace
  endOfLine
  _ <- "Distance:"
  skipSpace
  distances <- sepBy1' decimal skipSpace
  return (combine times, combine distances)

combine :: [Int] -> Int
combine = foldl1 (\cum add -> cum * product (replicate (numDigits add) 10) + add)

getPossibilities :: (Time, Distance) -> Int
getPossibilities (total, threshold) = length . filter (> threshold) . map (getDistance total) $ [1 .. (total - 1)]

getDistance :: Time -> Time -> Distance
getDistance total charging = charging * (total - charging)

numDigits :: Int -> Int
numDigits = length . show
