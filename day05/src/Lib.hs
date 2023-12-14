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
import Prelude hiding (takeWhile)
import Debug.Trace

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 35): "
  either error part1inner $ parseOnly parseInput1 s
  putStr "Part 2 debug (expecting 46): "
  either error part2inner $ parseOnly parseInput2 s

part1 :: IO ()
part1 = do
  s <- TIO.readFile "input.txt"
  putStr "Part 1: "
  either error part1inner $ parseOnly parseInput1 s

part1inner :: Input1 -> IO ()
part1inner input = do
  -- print input
  -- TIO.putStrLn ""
  -- print $ singleTransform (snd input) "seed" (fst input)
  -- TIO.putStrLn ""
  print . minimum $ fullTransform (snd input) (fst input)

part2 :: IO ()
part2 = do
  s <- TIO.readFile "input.txt"
  putStr "Part 2: "

part2inner :: Input2 -> IO ()
part2inner input = do
  print input
  -- TIO.putStrLn ""
  print . minimum . map idxFrom $ rangeTransform (snd input) (traceShowId (fst input))

-- print $ singleTransform (snd input) "seed" (fst input)
-- TIO.putStrLn ""
-- print . minimum $ fullTransform (snd input) (fst input)

-- either error part2inner $ parseOnly parseCards s

data Range = Range
  { startSource :: Int,
    startTarget :: Int,
    rangeLength :: Int
  }
  deriving (Show)

data Mapping = Mapping
  { mappingFrom :: T.Text,
    mappingTo :: T.Text,
    ranges :: [Range]
  }
  deriving (Show)

data IdxRange = IdxRange
  { idxFrom :: Int,
    idxLength :: Int
  }
  deriving (Show)

parseIdxRange :: Parser IdxRange
parseIdxRange = do
  idxFrom <- decimal
  skipSpace
  idxLength <- decimal
  return IdxRange {..}

nextMapping :: T.Text -> [Mapping] -> Maybe Mapping
nextMapping entry = find ((entry ==) . mappingFrom)

type Seeds = [Int]

fullTransform :: [Mapping] -> Seeds -> [Int]
fullTransform mappings = go "seed"
  where
    go :: T.Text -> [Int] -> [Int]
    go currentCategory idx = case singleTransform mappings currentCategory idx of
      (Nothing, nextIdx) -> nextIdx
      (Just next, nextIdx) -> go next nextIdx

singleTransform :: [Mapping] -> T.Text -> Seeds -> (Maybe T.Text, [Int])
singleTransform mappings currentCategory idx = case nextMapping currentCategory mappings of
  Nothing -> (Nothing, idx)
  Just mapping -> (Just . mappingTo $ mapping, map (transformSingleStep mapping) idx)

transformSingleStep :: Mapping -> Int -> Int
transformSingleStep Mapping {..} i = perform . find (`maps` i) $ ranges
  where
    perform :: Maybe Range -> Int
    perform (Just Range {..}) = (i - startSource) + startTarget
    perform Nothing = i

rangeTransform :: [Mapping] -> [IdxRange] -> [IdxRange]
rangeTransform mappings = go "seed"
  where
    go :: T.Text -> [IdxRange] -> [IdxRange]
    go currentCategory idx = case traceShowId (rangeTransform' mappings currentCategory idx) of
      (Nothing, nextIdx) -> nextIdx
      (Just next, nextIdx) -> go next nextIdx

-- get largest index to be mapped
idxMax :: [IdxRange] -> Int
idxMax = maximum . map idxTo

idxTo :: IdxRange -> Int
idxTo idx = idxFrom idx + idxLength idx

transformRangeStep :: Mapping -> IdxRange -> [IdxRange]
transformRangeStep Mapping {..} idxRange = perform . tracePrefix "after maps range:" . filter (`mapsRange` idxRange) $ traceShowId . completeBeginning . completeEnd $ completeRanges (traceShowId ranges)
  where
    perform :: [Range] -> [IdxRange]
    perform = map $ transform idxRange

    -- ensure there is no range missng at the end
    -- ensure there is no range missng at the beginngin
    completeEnd :: [Range] -> [Range]
    completeEnd [] = []
    completeEnd [i] = let endSource = startSource i + rangeLength i in
      if endSource < idxTo idxRange then
        i : [Range { startSource = endSource, startTarget = endSource, rangeLength = idxTo idxRange - endSource }]
      else
        [i]
    completeEnd (i:ii) = i : completeEnd ii
    
    -- ensure there is no range missng at the beginngin
    completeBeginning :: [Range] -> [Range]
    completeBeginning [] = []
    completeBeginning (i:ii) = case startSource i of
        0 -> i:ii
        start -> Range{ startSource = 0, startTarget = 0, rangeLength = start } : i : ii

    -- ensure that there are no gaps in ranges
    completeRanges :: [Range] -> [Range]
    completeRanges [] = []
    completeRanges (r:rr) = completeRanges' r rr

    completeRanges' :: Range -> [Range] -> [Range]
    completeRanges' current [] = [current]
    completeRanges' prev (next : remaining)
      | startSource prev + rangeLength prev == startSource next = prev : completeRanges' next remaining
      | otherwise = prev : inBetween : completeRanges' next remaining
      where
        startInBetween = startSource prev + rangeLength prev
        inBetween =
          Range
            { startSource = startInBetween,
              startTarget = startInBetween,
              rangeLength = startSource next - startInBetween
            }

    transform :: IdxRange -> Range -> IdxRange
    transform IdxRange {..} Range {..} = IdxRange {
      idxFrom = startTarget + offset,
      idxLength = min (rangeLength - offset) idxLength
    }
      where
        offset = idxFrom - startSource

rangeTransform' :: [Mapping] -> T.Text -> [IdxRange] -> (Maybe T.Text, [IdxRange])
rangeTransform' mappings currentCategory idxRanges = case nextMapping currentCategory mappings of
  Nothing -> (Nothing, idxRanges)
  Just mapping -> (Just . mappingTo $ mapping, concatMap (transformRangeStep mapping) idxRanges)

mapsRange :: Range -> IdxRange -> Bool
mapsRange Range {..} IdxRange {..} = not $ idxFrom + idxLength < startSource || startSource + rangeLength < idxFrom

maps :: Range -> Int -> Bool
maps Range {..} i = startSource <= i && i <= startSource + rangeLength

parseSeeds :: Parser Seeds
parseSeeds = "seeds: " *> sepBy1' decimal (char ' ')

type Input1 = (Seeds, [Mapping])

parseInput1 :: Parser Input1
parseInput1 = do
  seeds <- parseSeeds
  _ <- many1' endOfLine
  mappings <- parseMappings
  return (seeds, mappings)

type Input2 = ([IdxRange], [Mapping])

parseInput2 :: Parser Input2
parseInput2 = do
  _ <- "seeds: "
  seeds <- sepBy1' parseIdxRange (char ' ')
  _ <- many1' endOfLine
  mappings <- parseMappings
  return (seeds, mappings)

parseMappings :: Parser [Mapping]
parseMappings = sepBy1' parseMapping (many1' endOfLine)

parseMapping :: Parser Mapping
parseMapping = do
  mappingFrom <- takeWhile ('-' /=)
  _ <- "-to-"
  mappingTo <- takeWhile (' ' /=)
  " map:" *> endOfLine
  ranges <- sepBy1' parseRange endOfLine
  return $
    Mapping
      { mappingFrom,
        mappingTo,
        ranges = sortBy (\a b -> compare (startSource a) (startSource b)) ranges -- not yet sure if sorting needed
      }

parseRange :: Parser Range
parseRange = do
  startTarget <- decimal
  _ <- " "
  startSource <- decimal
  _ <- " "
  rangeLength <- decimal
  return $ Range {..}

tracePrefix :: Show a => String -> a -> a
tracePrefix prefix a = trace (prefix ++ " " ++ show a) a
