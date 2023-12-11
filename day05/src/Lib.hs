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
import Data.List (intersect)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Internal.Fusion.Common (findBy)
import Prelude hiding (takeWhile)

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 13): "
  either error part1inner $ parseOnly parseInput s

-- putStr "Part 2 debug (expecting 30): "

-- either error part2inner $ parseOnly parseCards s

part1 :: IO ()
part1 = do
  s <- TIO.readFile "input.txt"
  putStr "Part 1: "
  either error part1inner $ parseOnly parseInput s

part1inner :: Input -> IO ()
part1inner input = do
  print input
  TIO.putStrLn ""
  print $ singleTransform (snd input) "seed" (fst input)
  TIO.putStrLn ""
  print . minimum $ fullTransform (snd input) (fst input)

part2 :: IO ()
part2 = do
  s <- TIO.readFile "input.txt"
  putStr "Part 2: "

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
  Just mapping -> (Just . mappingTo $ mapping, map (transformToNext mapping) idx)

transformToNext :: Mapping -> Int -> Int
transformToNext Mapping {..} i = perform . find (`maps` i) $ ranges
  where
    perform :: Maybe Range -> Int
    perform (Just Range {..}) = (i - startSource) + startTarget
    perform Nothing = i

maps :: Range -> Int -> Bool
maps Range {..} i = startSource <= i && i <= startSource + rangeLength

parseSeeds :: Parser Seeds
parseSeeds = "seeds: " *> sepBy1' decimal (char ' ')

type Input = (Seeds, [Mapping])

parseInput :: Parser Input
parseInput = do
  seeds <- parseSeeds
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
        ranges
      }

parseRange :: Parser Range
parseRange = do
  startTarget <- decimal
  _ <- " "
  startSource <- decimal
  _ <- " "
  rangeLength <- decimal
  return $ Range {..}
