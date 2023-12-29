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
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (intercalate, intersect, sortBy)
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
  putStr "Part 2 debug (expecting 525152): "
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
  let damaged = map (splitDamaged . getStates) input
  let damagedConcat = S.fromList . concat $ damaged
  -- let damagedConcat = S.fromList . head $ damaged

  mapM_ print damagedConcat
  mapM_ (print . possibleCombinations) damagedConcat

type Input1 = [SpringConfig]

type Input2 = Input1

parseInput1 :: Parser Input1
parseInput1 = sepBy1' parseSpringConfig endOfLine

parseInput2 :: Parser Input2
parseInput2 = do map unfold <$> parseInput1

data SpringConfig = SC [SpringState] [Int] deriving (Show, Eq)

getStates :: SpringConfig -> [SpringState]
getStates (SC states _) = states

data SpringState = Operational | Damaged | Unknown deriving (Show, Eq, Ord)

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

-- split springs into sections seperated by at least one working spring
splitDamaged :: [SpringState] -> [[SpringState]]
splitDamaged = go [] []
  where
    --    accumulated        current split    input
    go :: [[SpringState]] -> [SpringState] -> [SpringState] -> [[SpringState]]
    go accum [] [] = accum
    go accum current [] = go (accum ++ [current]) [] []
    go accum [] (Operational : next) = go accum [] next
    go accum current (Operational : next) = go (accum ++ [current]) [] next
    go accum current (top : next) = go accum (current ++ [top]) next


-- NOTE: expects spring state to not contain Operational
possibleCombinations :: [SpringState] -> M.Map [Int] Int
possibleCombinations = foldl' (flip $ M.alter p1) M.empty . go 0
  where
    -- go :: M.Map [Int] Int -> [Int] -> [SpringState] -> M.Map [Int] Int
    -- go accum (current:other) (Damaged:ss) = go accum (current+1:other) ss
    -- go accum (current:other) (Unknown:ss) = M.alter p1
      -- accum (current+1:other) ss

    go :: Int -> [SpringState] -> [[Int]]
    go 0 [] = []
    go d [] = [[d]]
    go d (Damaged:ss) = go (d+1) ss
    go 0 (Unknown:ss) = go 1 ss
    go d (Unknown:ss) = go (d+1) ss ++ map ([d]++) (go 0 ss)
    go _ (Operational:_) = error "Operational not expected"

    p1 :: Maybe Int -> Maybe Int
    p1 Nothing = Just 1
    p1 (Just x) = Just $ x+1

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
