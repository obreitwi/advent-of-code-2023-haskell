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

import Data.Attoparsec.Text hiding (takeWhile, take)
import qualified Data.Text.IO as TIO
import Rebase.Prelude
import Prelude ()

-- import Control.Applicative
-- import Data.Bits
-- import Data.Char
-- import Data.Foldable
-- import Data.Functor
-- import Data.List (intersect, sortBy)
-- import Data.Text.Internal.Fusion.Common (findBy)
-- import Debug.Trace
-- import Prelude hiding (takeWhile)
-- import qualified Data.Bifunctor as B
-- import qualified Data.List as L
-- import qualified Data.Map.Strict as M
-- import qualified Data.Text as T
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as U

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting XXX): "
  either error part1inner $ parseOnly parseInput1 s
  putStr "Part 2 debug (expecting XXX): "
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
  -- print input
  -- print $ dropColumn [Empty, Cube, Empty, Round]
  -- putStrLn ""
  -- mapM_ print . rotateCounterClock $ input
  -- putStrLn ""
  -- mapM_ (print . dropColumn) . rotateCounterClock $ input
  -- putStrLn ""
  print . calculateLoadWest . map dropColumn . rotateCounterClock $ input

part2inner :: Input2 -> IO ()
part2inner input = do
  print input

type Input1 = [[Cell]]

type Input2 = ()

parseInput1 :: Parser Input1
parseInput1 = sepBy1' (many1' parseCell) endOfLine

parseInput2 :: Parser Input2
parseInput2 = return ()

data Cell = Empty | Round | Cube deriving (Show, Eq)

parseCell :: Parser Cell
parseCell = char '.' $> Empty <|> char 'O' $> Round <|> char '#' $> Cube

rotateCounterClock :: [[Cell]] -> [[Cell]]
rotateCounterClock = reverse . go
  where
    go :: [[Cell]] -> [[Cell]]
    go [] = []
    go cc
      | all null cc = []
      | otherwise = map head cc : go (map tail cc)

calculateLoadWest :: [[Cell]] -> Int
calculateLoadWest = sum . map calculateLoadColumn

calculateLoadColumn :: [Cell] -> Int
calculateLoadColumn = sum . map fst . filter ((== Round) . snd) . zip [1..] . reverse

dropColumn :: [Cell] -> [Cell]
dropColumn [] = []
dropColumn (Cube:cc) = Cube : dropColumn cc
dropColumn cc = (uncurry (++) . partition (== Round) . takeWhile (/= Cube) $ cc) ++ dropColumn (dropWhile (/= Cube) cc)
