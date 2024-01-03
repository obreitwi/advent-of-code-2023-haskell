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

import Data.Attoparsec.Text hiding (take, takeWhile)
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import qualified Data.Text.IO as TIO
import Rebase.Prelude
import Prelude ()

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
  print . calculateLoadWest . map dropColumn . rotateCounterClock $ input

part2inner :: Input2 -> IO ()
part2inner input = do
  when (input /= (rotateCounterClock . rotateCounterClock . rotateCounterClock . rotateCounterClock) input) $
    error "rotation not invariant"

  print . calculateLoadWest . rotateCounterClock $ rotateAndDropN 1000000000 input

type Input1 = [[Cell]]

type Input2 = Input1

parseInput1 :: Parser Input1
parseInput1 = sepBy1' (many1' parseCell) endOfLine

parseInput2 :: Parser Input2
parseInput2 = parseInput1

data Cell = Empty | Round | Cube deriving (Show, Eq, Enum, Ord)

parseCell :: Parser Cell
parseCell = char '.' $> Empty <|> char 'O' $> Round <|> char '#' $> Cube

rotateAndDropN :: Int -> [[Cell]] -> [[Cell]]
rotateAndDropN = go' M.empty
  where
    mo :: [[Cell]] -> [[Cell]]
    mo = Memo.list (Memo.list Memo.enum) fullRotationDrop

    go :: Int -> [[Cell]] -> [[Cell]]
    go 0 cc = cc
    go n prev =
      let next = mo prev
       in if prev == next
            then next
            else trace (printf "[%d] %d" n (calculateLoadWest . rotateCounterClock $ next)) $ go (n - 1) next

    go' :: M.Map [[Cell]] Int -> Int -> [[Cell]] -> [[Cell]]
    go' _ 0 prev = prev
    go' seen n prev =
      let next = fullRotationDrop prev
       in case seen M.!? next of
            Nothing -> go' (M.insert prev n seen) (n - 1) next
            Just seenAt -> let n' = (n `rem` (seenAt - n + 1)) in go' M.empty (n'-1) next

fullRotationDrop :: [[Cell]] -> [[Cell]]
fullRotationDrop =
  rotateCounterClock
    . rotateCounterClock
    . map dropColumn
    . rotateCounterClock
    . rotateCounterClock
    . rotateCounterClock -- east
    . map dropColumn
    . rotateClock
    . map dropColumn -- south
    . rotateClock -- west
    . map dropColumn
    . rotateCounterClock -- north

rotDrop :: [[Cell]] -> [[Cell]]
rotDrop = map dropColumn . rotateCounterClock

rotateClock :: [[Cell]] -> [[Cell]]
rotateClock = rotateCounterClock . rotateCounterClock . rotateCounterClock

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
calculateLoadColumn = sum . map fst . filter ((== Round) . snd) . zip [1 ..] . reverse

dropColumn :: [Cell] -> [Cell]
dropColumn [] = []
dropColumn (Cube : cc) = Cube : dropColumn cc
dropColumn cc = (uncurry (++) . partition (== Round) . takeWhile (/= Cube) $ cc) ++ dropColumn (dropWhile (/= Cube) cc)

printCells :: [[Cell]] -> IO ()
printCells cc = mapM_ printCellLine cc >> putChar '\n'

printCellLine :: [Cell] -> IO ()
printCellLine l = mapM_ printCell l >> putChar '\n'
  where
    printCell :: Cell -> IO ()
    printCell Cube = putChar '#'
    printCell Empty = putChar '.'
    printCell Round = putChar 'O'
