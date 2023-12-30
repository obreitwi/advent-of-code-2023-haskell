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
import Data.List (intercalate, intersect, sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.MemoCombinators as Memo
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Internal.Fusion.Common (findBy)
import Debug.Trace
import Text.Printf
import Prelude hiding (takeWhile)

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 21): "
  either error part1inner $ parseOnly parseInput1 s
  putStr "Part 2 debug (expecting 525152): "
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
  -- print $ map countValid input
  print . sum $ map countValid input

part2inner :: Input2 -> IO ()
part2inner input = do
  -- mapM_ (print . countPossibilitiesAllAtOnce) input
  print . sum . map countPossibilitiesAllAtOnce $ input

type Input1 = [SpringConfig]

type Input2 = Input1

parseInput1 :: Parser Input1
parseInput1 = sepBy1' parseSpringConfig endOfLine

parseInput2 :: Parser Input2
parseInput2 = do map unfold <$> parseInput1

data SpringConfig = SC [SpringState] [Int] deriving (Show, Eq)

data SpringState = Operational | Damaged | Unknown deriving (Show, Eq, Ord, Enum)

parseSpringConfig :: Parser SpringConfig
parseSpringConfig = do
  springs <- many1' parseSpringState
  skipSpace
  damaged <- sepBy1' decimal $ char ','
  return $ SC springs damaged

parseSpringState :: Parser SpringState
parseSpringState = char '.' $> Operational <|> char '#' $> Damaged <|> char '?' $> Unknown

unfold :: SpringConfig -> SpringConfig
unfold (SC cfgs dmgd) = SC (intercalate [Unknown] (replicate 5 cfgs)) (concat $ replicate 5 dmgd)

countPossibilitiesAllAtOnce :: SpringConfig -> Int
countPossibilitiesAllAtOnce (SC sss ttt) = mo 0 sss ttt
  where
    mo :: Int -> [SpringState] -> [Int] -> Int
    mo = Memo.memo3 Memo.integral (Memo.list Memo.enum) (Memo.list Memo.integral) go'

    go' :: Int -> [SpringState] -> [Int] -> Int
    -- go' d ss tt = trace (printf "[Depth] %d [SpringStates] %s [Targets] %s" d (show ss) (show tt)) $ go d ss tt
    go' = go

    -- current damaged, input, target groups
    go :: Int -> [SpringState] -> [Int] -> Int
    go 0 [] [] = 1
    go d [] [t] | t == d = 1
    go _ [] _ = 0
    go _ (Damaged : _) [] = 0
    go d (Damaged : ss) allTargets@(t : _)
      | d >= t = 0 -- more damaged springs than the current contiguous region allows
      | otherwise = mo (d + 1) ss allTargets
    go d (Unknown : ss) [] = mo d ss []
    go 0 (Unknown : ss) targets =
      let choseDamaged = mo 1 ss targets
          choseOperational = mo 0 ss targets
       in choseDamaged + choseOperational
    go d (Unknown : ss) allTargets@(t : targets)
      | d == t = mo 0 ss targets
      | d < t = mo (d + 1) ss allTargets
      | otherwise = 0
    go 0 (Operational : ss) targets = go 0 ss targets
    go d (Operational : ss) (t : targets)
      | d == t = go 0 ss targets
      | otherwise = 0
    go _ (Operational : _) [] = 0

countValid :: SpringConfig -> Int
countValid (SC configs damaged) = go Operational configs damaged
  where
    go :: SpringState -> [SpringState] -> [Int] -> Int
    go Operational (Operational : cfgs) dmgd = go Operational cfgs dmgd
    go _ (Damaged : cfgs) (d : dmgd) | d > 0 = go Damaged cfgs (d - 1 : dmgd)
    -- \| otherwise = 0 -- invalid config
    go Damaged (Operational : cfgs) (d : dmgd) | d == 0 = go Operational cfgs dmgd
    -- \| otherwise = 0 -- invalid config
    go _ [] [0] = 1
    go _ [] [] = 1
    go current (Unknown : cfgs) dmgd = go current (Operational : cfgs) dmgd + go current (Damaged : cfgs) dmgd
    go Unknown _ _ = error "Unknown never current state"
    go _ _ _ = 0
