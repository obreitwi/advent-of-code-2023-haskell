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
  putStr "Part 1 debug (expecting 21): "
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
  -- print input
  -- print $ map countValid input
  print . sum $ map countValid input

part2inner :: Input2 -> IO ()
part2inner input = do
  print input

type Input1 = [SpringConfig]

type Input2 = ()

parseInput1 :: Parser Input1
parseInput1 = sepBy1' parseSpringConfig endOfLine

parseInput2 :: Parser Input2
parseInput2 = return ()

data SpringConfig = SpringConfig [SpringState] [Int] deriving (Show)
data SpringState = Operational | Damaged | Unknown deriving (Show, Eq)

parseSpringConfig :: Parser SpringConfig
parseSpringConfig = do
  springs <- many1' parseSpringState
  skipSpace
  damaged <- sepBy1' decimal $ char ','
  return $ SpringConfig springs damaged

parseSpringState :: Parser SpringState
parseSpringState = char '.' $> Operational <|> char '#' $> Damaged <|> char '?' $> Unknown

countValid :: SpringConfig -> Int
countValid (SpringConfig configs damaged) = go Operational configs damaged
  where
    go :: SpringState -> [SpringState] -> [Int] -> Int
    go Operational (Operational:cfgs) dmgd = go Operational cfgs dmgd
    go _ (Damaged:cfgs) (d:dmgd) | d > 0 = go Damaged cfgs (d-1:dmgd)
      -- | otherwise = 0 -- invalid config
    go Damaged (Operational:cfgs) (d:dmgd) | d == 0 = go Operational cfgs dmgd
     -- | otherwise = 0 -- invalid config
    go _ [] [0] = 1
    go _ [] [] = 1
    go current (Unknown:cfgs) dmgd = go current (Operational:cfgs) dmgd + go current (Damaged:cfgs) dmgd
    go Unknown _ _ = error "Unknown never current state"
    go _ _ _ = 0
