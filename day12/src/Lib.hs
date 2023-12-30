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
  -- let damaged = map (splitDamaged . getStates) input
  -- let damagedConcat = S.fromList . concat $ damaged
  -- let target = map getDamaged input

  -- mapM_ print damagedConcat
  -- mapM_ (print . \x -> (x, possibleCombinations x)) damagedConcat
  -- mapM_ print $ head damaged
  -- print $ head target
  -- putStrLn ""
  -- mapM_ (mapM_ print . M.toList . flip filterPossible (head target)) $ head damaged
  -- mapM_ print $ zipWith countValidDamaged target damaged
  -- print . sum $ zipWith countValidDamaged target damaged
  mapM_ (print . countPossibilitiesAllAtOnce) input
  print . sum . map countPossibilitiesAllAtOnce $ input

type Input1 = [SpringConfig]

type Input2 = Input1

parseInput1 :: Parser Input1
parseInput1 = sepBy1' parseSpringConfig endOfLine

parseInput2 :: Parser Input2
parseInput2 = do map unfold <$> parseInput1

data SpringConfig = SC [SpringState] [Int] deriving (Show, Eq)

getStates :: SpringConfig -> [SpringState]
getStates (SC states _) = states

getDamaged :: SpringConfig -> [Int]
getDamaged (SC _ dmgd) = dmgd

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
    go 0 [] = [[]]
    go d [] = [[d]]
    go d (Damaged : ss) = traceShow (length ss) $ go (d + 1) ss
    go 0 (Unknown : ss) = traceShow (length ss) $ go 0 ss ++ go 1 ss
    go d (Unknown : ss) = traceShow (length ss) $ go (d + 1) ss ++ map ([d] ++) (go 0 ss)
    go _ (Operational : _) = error "Operational not expected"

    p1 :: Maybe Int -> Maybe Int
    p1 Nothing = Just 1
    p1 (Just x) = Just $ x + 1

countValidDamaged :: [Int] -> [[SpringState]] -> Int
countValidDamaged ttt = fromMaybe 0 . flip (M.!?) [] . foldl folder initial
  where
    initial :: M.Map [Int] Int
    initial = M.singleton ttt 1

    folder :: M.Map [Int] Int -> [SpringState] -> M.Map [Int] Int
    folder x y = traceShow (x, y) $ filterDamagedGroup y x

-- folder x y = filterDamagedGroup y x

-- filter all out all possible combinations that cannot be reached for a "damanged group" (i.e. a group without Operational states)
filterDamagedGroup :: [SpringState] -> M.Map [Int] Int -> M.Map [Int] Int
-- filterDamagedGroup s = traceShowId . foldl' (M.unionWith (+)) M.empty . map (\(k, v) -> M.map (v +) (countPossibilities s k)) . M.toList
filterDamagedGroup s = foldl' (M.unionWith (+)) M.empty . map (\(k, v) -> M.map (v *) (countPossibilities s k)) . M.toList

-- NOTE: expects spring state to not contain Operational
countPossibilities :: [SpringState] -> [Int] -> M.Map [Int] Int
countPossibilities sss ttt = foldl' (flip $ M.alter p1) M.empty $ mo 0 sss ttt
  where
    mo :: Int -> [SpringState] -> [Int] -> [[Int]]
    mo = Memo.memo3 Memo.integral (Memo.list Memo.enum) (Memo.list Memo.integral) go'

    -- go :: M.Map [Int] Int -> [Int] -> [SpringState] -> M.Map [Int] Int
    -- go accum (current:other) (Damaged:ss) = go accum (current+1:other) ss
    -- go accum (current:other) (Unknown:ss) = M.alter p1
    -- accum (current+1:other) ss

    go' :: Int -> [SpringState] -> [Int] -> [[Int]]
    go' d ss tt = trace (printf "[Depth] %d [SpringStates] %s [Targets] %s" d (show ss) (show tt)) $ go d ss tt

    -- current damaged, input, target groups
    go :: Int -> [SpringState] -> [Int] -> [[Int]]
    go 0 [] [] = [[]]
    go _ [] [] = []
    -- go 0 [] targets = [targets]
    go 0 [] targets = []
    go d [] (t : tt)
      | d == t = [tt]
      | otherwise = []
    go _ (Damaged : ss) [] = []
    go d (Damaged : ss) allTargets@(t : _)
      | d >= t = [] -- more damaged springs than the current contiguous region allows
      | otherwise = mo (d + 1) ss allTargets
    go d (Unknown : ss) [] = mo d ss []
    go 0 (Unknown : ss) targets =
      let choseDamaged = mo 1 ss targets
          choseOperational = mo 0 ss targets
          remainingFixedDamaged = length . filter (== Damaged) $ ss
          totalDamagedExpected = sum targets
       in -- in choseOperational `seq` choseDamaged `seq` choseDamaged ++ choseOperational
          if remainingFixedDamaged < totalDamagedExpected -- only choose both if we can expect to succeed
            then choseDamaged ++ choseOperational
            else choseOperational
    go d (Unknown : ss) allTargets@(t : targets)
      | d == t = mo 0 ss targets
      | d < t = mo (d + 1) ss allTargets
      | otherwise = []
    go _ (Operational : _) _ = error "encountered fixed Operational spring"

    p1 :: Maybe Int -> Maybe Int
    p1 Nothing = Just 1
    p1 (Just x) = Just $ x + 1

countPossibilitiesAllAtOnce :: SpringConfig -> Int
countPossibilitiesAllAtOnce (SC sss ttt) = mo 0 sss ttt
  where
    mo :: Int -> [SpringState] -> [Int] -> Int
    mo = Memo.memo3 Memo.integral (Memo.list Memo.enum) (Memo.list Memo.integral) go'

    -- go :: M.Map [Int] Int -> [Int] -> [SpringState] -> M.Map [Int] Int
    -- go accum (current:other) (Damaged:ss) = go accum (current+1:other) ss
    -- go accum (current:other) (Unknown:ss) = M.alter p1
    -- accum (current+1:other) ss

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
          -- remainingFixedDamaged = length . filter (== Damaged) $ ss
          -- totalDamagedExpected = sum targets
       -- in choseOperational `seq` choseDamaged `seq` choseDamaged + choseOperational
       in choseDamaged + choseOperational
          -- if remainingFixedDamaged < totalDamagedExpected -- only choose both if we can expect to succeed
            -- then choseDamaged + choseOperational
            -- else choseOperational
    go d (Unknown : ss) allTargets@(t : targets)
      | d == t = mo 0 ss targets
      | d < t = mo (d + 1) ss allTargets
      | otherwise = 0
    go 0 (Operational : ss) targets = go 0 ss targets
    go d (Operational : ss) (t : targets)
      | d == t = go 0 ss targets
      | otherwise = 0
    go d (Operational : ss) [] = 0

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
