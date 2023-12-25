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
import Data.List (intersect, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Internal.Fusion.Common (findBy)
import Debug.Trace
import Prelude hiding (takeWhile)

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 374): "
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
  -- putStrLn "input:"
  -- printUniverse input
  -- putStrLn "\ntransposed:"
  -- printUniverse . transpose $ input
  -- putStrLn ""
  let expanded = transpose . expand . transpose . expand $ input
  -- putStrLn "expanded:"
  -- printUniverse expanded
  -- putStrLn "---"
  let galaxies = findCoords expanded
  -- print galaxies
  print . computeDistances $ galaxies

-- print . product . map getPossibilities $ input

part2inner :: Input2 -> IO ()
part2inner input = do
  print input

-- print . getPossibilities $ input

type Input1 = [[Cell]]

type Input2 = ()

parseInput1 :: Parser Input1
parseInput1 = parseUniverse

parseInput2 :: Parser Input2
parseInput2 = return ()

data Cell = EmptySpace | Galaxy deriving (Show, Eq)

parseUniverse :: Parser [[Cell]]
parseUniverse = sepBy1' (many1' parseCell) endOfLine

parseCell :: Parser Cell
parseCell = char '.' $> EmptySpace <|> char '#' $> Galaxy

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

transpose :: [[a]] -> [[a]]
transpose x | all isEmpty x = []
  | otherwise = map head x : transpose (map tail x)

expand :: [[Cell]] -> [[Cell]]
expand (x : xx)
  | all (== EmptySpace) x = x : x : expand xx
  | otherwise = x : expand xx
expand [] = []

findCoords :: [[Cell]] -> [(Int, Int)]
findCoords cells = [(i, j) | (j, row) <- enumerate cells, (i, c) <- enumerate row, c == Galaxy]

enumerate :: [a] -> [(Int, a)]
enumerate = go 0
  where
    go :: Int -> [a] -> [(Int, a)]
    go _ [] = []
    go i (x : xs) = (i, x) : go (i + 1) xs

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

computeDistances :: [(Int, Int)] -> Int
computeDistances cc = (`quot` 2) . sum . map (uncurry dist) $ [(c1, c2) | c1 <- cc, c2 <- cc]

printCell :: Cell -> IO ()
printCell EmptySpace = putChar '.'
printCell Galaxy = putChar '#'

printUniverse :: [[Cell]] -> IO ()
printUniverse = mapM_ (\l -> mapM_ printCell l >> putStrLn "")
