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
import Data.Int
import Data.List (intersect, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Internal.Fusion.Common (findBy)
import Debug.Trace
import Prelude hiding (takeWhile)

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 114): "
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

-- either error part2inner $ parseOnly parseInput2 s

part1inner :: Input1 -> IO ()
part1inner input = do
  print . length $ input
  -- mapM_ (\n -> putStrLn $ show n ++ " → " ++ show (predictNext n)) input
  mapM_ ((\d -> mapM_ print d >> putStrLn "") . derivatives) input
  print . map predictNext $ input
  print . sum . map predictNext $ input

-- print . product . map getPossibilities $ input

part2inner :: Input2 -> IO ()
part2inner input = do
  print input

-- print . getPossibilities $ input

type Input1 = [History]

type Input2 = ()

parseInput1 :: Parser Input1
parseInput1 = sepBy1' (sepBy1' (signed decimal) $ char ' ') endOfLine

parseInput2 :: Parser Input2
parseInput2 = return ()

type History = [Int64]

derivatives :: History -> [History]
derivatives [] = []
derivatives cur =
  if all (== 0) next
    then cur : [next]
    else cur : derivatives next
  where
    next :: History
    next = zipWith (-) (tail cur) (init cur)

predictNext :: History -> Int64
predictNext = predictNext' . derivatives

predictNext' :: [History] -> Int64
predictNext' = foldr ((+) . last) 0
