{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( part0,
    part1,
    part2,
  )
where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Bifunctor as B
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor
import Data.List (intersect, sortBy)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Internal.Fusion.Common (findBy)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Debug.Trace
import Prelude hiding (takeWhile)

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 405): "
  either error part1inner $ parseOnly parseInput1 s
  putStr "Part 2 debug (expecting 71503): "
  either error part2inner $ parseOnly parseInput2 s

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
  print . calculateScore $ map determineMirror input

filterSame :: (Int, [(Vote, Int)]) -> Bool
filterSame = cmp . map snd . snd
  where
    cmp :: [Int] -> Bool
    cmp [] = False
    cmp [_] = False
    cmp (f : s : _) = f == s

part2inner :: Input2 -> IO ()
part2inner input = do
  print input

type Input1 = [Pattern]

type Input2 = ()

parseInput1 :: Parser Input1
parseInput1 = sepBy1' parsePattern (many1' endOfLine)

parseInput2 :: Parser Input2
parseInput2 = return ()

printPattern :: Pattern -> IO ()
printPattern = mapM_ printVector

printVector :: (U.Unbox a) => (Show a) => U.Vector a -> IO ()
printVector = putStrLn . filter (/= '"') . show

type Pattern = V.Vector (U.Vector Char)

data Vote = Col Int | Row Int deriving (Show, Eq, Ord)

ash :: Char
ash = '.'

rocks :: Char
rocks = '#'

parseCell :: Parser Char
parseCell = char ash <|> char rocks

parsePattern :: Parser Pattern
parsePattern = do
  rows <- sepBy1' (many1' parseCell) endOfLine
  return . V.fromList . map U.fromList $ rows

calculateScore :: [Vote] -> Int
calculateScore = sum . map calc
  where
    calc :: Vote -> Int
    calc (Col c) = c
    calc (Row r) = 100 * r

determineMirror :: Pattern -> Vote
determineMirror p = head . filter (verifyVote p) . map fst . sortBy (flip compareVotes) . M.toList . hughVotes $ p



compareVotes :: (Vote, Int) -> (Vote, Int) -> Ordering
compareVotes (_, l) (_, r) = compare l r

hughVotes :: Pattern -> M.Map Vote Int
hughVotes p = foldl1 (M.unionWith (+)) $ map (hughVotesCoords p) coords
  where
    coords = [(i, j) | i <- [0 .. (numCols - 1)], j <- [0 .. (numRows - 1)]]

    numCols = U.length (p V.! 0)
    numRows = V.length p

hughVotesCoords :: Pattern -> (Int, Int) -> M.Map Vote Int
hughVotesCoords p c = M.unionWith (+) (hughVotesCol p c) (hughVotesRow p c)

hughVotesRow :: Pattern -> (Int, Int) -> M.Map Vote Int
hughVotesRow p (i, j) = M.fromList . map (\ii -> (Col (ii + 1), 1)) $ mirrorRows -- NOTE: +1 because AoC counts from 1
  where
    num = U.length (p V.! j)

    coords = map (,j) . filter (/= i) $ [0 .. (num - 1)]

    currentCell = (p V.! j) U.! i

    matching = map (fst . fst) . filter (\(_, c) -> c == currentCell) . map (\c@(ii, jj) -> (c, (p V.! jj) U.! ii)) $ coords

    -- mirrors can only be between rows
    mirrorRows = map (\c -> c `quot` 2 + i) . filter (\x -> x `rem` 2 == 1) . map (\x -> x - i) $ matching

hughVotesCol :: Pattern -> (Int, Int) -> M.Map Vote Int
hughVotesCol p (i, j) = M.fromList . map (\jj -> (Row (jj + 1), 1)) $ mirrorCols -- NOTE: +1 because AoC counts from 1
  where
    num = V.length p

    coords = map (i,) . filter (/= j) $ [0 .. (num - 1)]

    currentCell = (p V.! j) U.! i

    matching = map (snd . fst) . filter (\(_, c) -> c == currentCell) . map (\c@(ii, jj) -> (c, (p V.! jj) U.! ii)) $ coords

    -- mirrors can only be between rows
    mirrorCols = map (\c -> c `quot` 2 + j) . filter (\y -> y `rem` 2 == 1) . map (\y -> y - j) $ matching

verifyVote :: Pattern -> Vote -> Bool
verifyVote p (Col i) = verifyCol p i
verifyVote p (Row j) = verifyRow p j

verifyCol :: Pattern -> Int -> Bool
verifyCol p i = all vrfy toTest
  where
    toTest :: [(Int, Int)]
    toTest = [(ii, jj) | ii <- [from .. to], jj <- [0 .. (numRows - 1)]]

    numCols = U.length (V.head p)
    numRows = V.length p

    range = min (numCols - i) i

    from = i - range
    to = i - 1

    get :: (Int, Int) -> Char
    get (ii, jj) = (p V.! jj) U.! ii

    vrfy :: (Int, Int) -> Bool
    vrfy (ii, jj) = get (ii, jj) == get (i + (i - ii) - 1, jj)

verifyRow :: Pattern -> Int -> Bool
verifyRow p j = all vrfy toTest
  where
    toTest :: [(Int, Int)]
    toTest = [(ii, jj) | ii <- [0 .. (numCols - 1)], jj <- [from .. to]]

    numCols = U.length (V.head p)
    numRows = V.length p

    range = min (numRows - j) j

    from = j - range
    to = j - 1

    get :: (Int, Int) -> Char
    get (ii, jj) = (p V.! jj) U.! ii

    vrfy :: (Int, Int) -> Bool
    vrfy (ii, jj) = get (ii, jj) == get (ii, j + (j - jj) - 1)
