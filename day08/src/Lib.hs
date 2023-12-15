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
import Data.Attoparsec.Text as P
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor
import Data.List (intersect, sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Internal.Fusion.Common (findBy)
import Debug.Trace
import Prelude hiding (takeWhile)

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1a debug (expecting 2): "
  either error part1inner $ parseOnly parseInput1 s
  s2 <- TIO.readFile "debug2.txt"
  putStr "Part 1a debug (expecting 6): "
  either error part1inner $ parseOnly parseInput1 s2
  s3 <- TIO.readFile "debug3.txt"
  putStr "Part 2 debug (expecting 6): "
  either error part2inner $ parseOnly parseInput2 s3

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
  -- TIO.putStrLn ""
  -- mapM_ print . M.elems . snd $ input
  print . countSteps $ input

-- print . product . map getPossibilities $ input

part2inner :: Input2 -> IO ()
part2inner input = do
  print . countStepsMultiple $ input

-- print . getPossibilities $ input

type Input1 = ([Direction], Nodes)

type Input2 = Input1

parseInput1 :: Parser Input1
parseInput1 = do
  directions <- many1' parseDirection
  endOfLine
  endOfLine
  lNodes <- sepBy1' parseNode endOfLine
  let nodes = M.fromList . map (\n -> (nLabel n, n)) $ lNodes
  return (directions, nodes)

parseInput2 :: Parser Input2
parseInput2 = parseInput1

data Direction = DirLeft | DirRight deriving (Show)

type NodeLabel = T.Text

data Node = Node
  { nLabel :: NodeLabel,
    nLeft :: NodeLabel,
    nRight :: NodeLabel
  }
  deriving (Show)

type Nodes = M.Map NodeLabel Node

parseDirection :: Parser Direction
parseDirection = char 'L' $> DirLeft <|> char 'R' $> DirRight

parseNode :: Parser Node
parseNode = do
  nLabel <- P.take 3
  _ <- " = ("
  nLeft <- P.take 3
  _ <- ", "
  nRight <- P.take 3
  _ <- ")"
  return Node {..}

walk :: Nodes -> Direction -> NodeLabel -> NodeLabel
walk nodes d l = case M.lookup l nodes of
  Just next -> walkNode d next
  Nothing -> error $ "node not found: " ++ show l

walkNode :: Direction -> Node -> NodeLabel
walkNode DirLeft Node {..} = nLeft
walkNode DirRight Node {..} = nRight

countSteps :: ([Direction], Nodes) -> Int
countSteps (dirs, nodes) = countStepsStart (dirs, nodes) "AAA"

countStepsStart :: ([Direction], Nodes) -> NodeLabel -> Int
countStepsStart (dirs, nodes) = go 0 (cycle dirs)
  where
    go :: Int -> [Direction] -> NodeLabel -> Int
    go total (d : dd) current =
      let next = walk nodes d current
       in if T.isSuffixOf "Z" next
            then total + 1
            else go (total + 1) dd next
    go _ [] _ = error "directions should never run out"

countStepsMultiple :: ([Direction], Nodes) -> Int
countStepsMultiple input@(_, nodes) = foldl1 lcm . map (countStepsStart input) $ startingNodes
  where
    startingNodes = filter (T.isSuffixOf "A") . M.keys $ nodes
