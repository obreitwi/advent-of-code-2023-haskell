{-# LANGUAGE DuplicateRecordFields #-}{-# LANGUAGE NamedFieldPuns #-}
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
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Internal.Fusion.Common (findBy)
import Debug.Trace
import GHC.RTS.Flags (ProfFlags (heapProfileInterval))
import Prelude hiding (takeWhile)

part0 :: IO ()
part0 = do
  putStr "Part 1 debug simple (expecting 4): "
  s1 <- TIO.readFile "debug.txt"
  either error part1inner $ parseOnly parseInput1 s1
  putStr "Part 1 debug complex (expecting 8): "
  s2 <- TIO.readFile "debug2.txt"
  either error part1inner $ parseOnly parseInput1 s2
  putStr "Part 2 debug (expecting 71503): "
  either error part2inner $ parseOnly parseInput2 s1

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
  let m= toMap input
  -- print m
  let start = getStart input
  let pipeStart = determinePipe m start
  -- putStrLn $ "start is: " ++ show pipeStart
  let distances = computeDistances (M.insert start pipeStart m) start
  -- mapM_ print $ M.assocs distances
  print . maximum $ M.elems distances

-- print . product . map getPossibilities $ input

part2inner :: Input2 -> IO ()
part2inner input = do
  print input

-- print . getPossibilities $ input

type Input1 = [[Tile]]

type Input2 = ()

parseInput1 :: Parser Input1
parseInput1 = parseTiles

parseInput2 :: Parser Input2
parseInput2 = return ()

-- | is a vertical pipe connecting north and south.
-- - is a horizontal pipe connecting east and west.
-- L is a 90-degree bend connecting north and east.
-- J is a 90-degree bend connecting north and west.
-- 7 is a 90-degree bend connecting south and west.
-- F is a 90-degree bend connecting south and east.
-- . is ground; there is no pipe in this tile.
-- S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
data Tile
  = Ground
  | Start
  | PipeT Pipe
  deriving (Show, Eq)

data Pipe = Pipe DirVertical DirHorizontal
  deriving (Show, Eq)

data DirVertical = NorthSouth | Horizontal | North | South deriving (Show, Eq)

data DirHorizontal = WestEast | Vertical | East | West deriving (Show, Eq)

parseTile :: Parser Tile
parseTile =
  char '.' $> Ground
    <|> char 'S' $> Start
    <|> char '|' $> PipeT (Pipe NorthSouth Vertical)
    <|> char '-' $> PipeT (Pipe Horizontal WestEast)
    <|> char 'L' $> PipeT (Pipe North East)
    <|> char 'J' $> PipeT (Pipe North West)
    <|> char '7' $> PipeT (Pipe South West)
    <|> char 'F' $> PipeT (Pipe South East)

parseTiles :: Parser [[Tile]]
parseTiles = sepBy1' (many1' parseTile) endOfLine

getPipes :: [((Int, Int), Tile)] -> [Location]
getPipes = mapMaybe go
  where
    go :: ((Int, Int), Tile) -> Maybe Location
    go (idx, PipeT p) = Just (idx, p)
    go _ = Nothing

type PipeMap = M.Map (Int, Int) Pipe

toMap :: [[Tile]] -> PipeMap
toMap = M.fromList . getPipes . coords

coords :: [[Tile]] -> [((Int, Int), Tile)]
coords rows = [((i, j), tile) | (j, row) <- enumerate rows, (i, tile) <- enumerate row]

enumerate :: [a] -> [(Int, a)]
enumerate = go 0
  where
    go :: Int -> [a] -> [(Int, a)]
    go _ [] = []
    go i (x : xs) = (i, x) : go (i + 1) xs

type Location = ((Int, Int), Pipe)

getStart :: [[Tile]] -> (Int, Int)
getStart = fst . goFilter . coords
  where
    goFilter x = case find ((== Start) . snd) x of
      Just s -> s
      Nothing -> error "no start tile"

goesNorth :: Maybe Pipe -> Bool
goesNorth (Just (Pipe North _)) = True
goesNorth (Just (Pipe NorthSouth _)) = True
goesNorth _ = False

goesSouth :: Maybe Pipe -> Bool
goesSouth (Just (Pipe South _)) = True
goesSouth (Just (Pipe NorthSouth _)) = True
goesSouth _ = False

goesWest :: Maybe Pipe -> Bool
goesWest (Just (Pipe _ West)) = True
goesWest (Just (Pipe _ WestEast)) = True
goesWest _ = False

goesEast :: Maybe Pipe -> Bool
goesEast (Just (Pipe _ East)) = True
goesEast (Just (Pipe _ WestEast)) = True
goesEast _ = False

-- check which pipe lies at the given coordinates
-- (assumes only one pipe fits)
determinePipe :: PipeMap -> (Int, Int) -> Pipe
determinePipe m (x, y)
  | goesSouth northNeighbor && goesNorth southNeighbor = Pipe NorthSouth Vertical
  | goesEast westNeighbor && goesWest eastNeighbor = Pipe Horizontal WestEast
  | goesSouth northNeighbor && goesWest eastNeighbor = Pipe North East
  | goesSouth northNeighbor && goesEast westNeighbor = Pipe North West
  | goesNorth southNeighbor && goesEast westNeighbor = Pipe South West
  | goesNorth southNeighbor && goesWest eastNeighbor = Pipe South East
  where
    get = flip M.lookup m
    northNeighbor = get (x, y - 1)
    southNeighbor = get (x, y + 1)
    eastNeighbor = get (x + 1, y)
    westNeighbor = get (x - 1, y)
determinePipe _ crds = error $ "could not determine pipe for " ++ show crds

computeDistances :: PipeMap -> (Int, Int) -> M.Map (Int, Int) Int
computeDistances pipeMap start = bfs (M.singleton start 0) [start]
  where
    bfs :: M.Map (Int, Int) Int -> [(Int, Int)] -> M.Map (Int, Int) Int
    bfs distances [] = distances
    bfs distances (current:queue) = let
        nn = neighbors current (pipeMap ! current)

        foldedUpdate :: M.Map (Int, Int) Int -> (Int, Int) -> M.Map (Int, Int) Int
        foldedUpdate = flip $ M.alter $ update (distances ! current)

        updatedDistances :: M.Map (Int, Int) Int
        updatedDistances = foldl' foldedUpdate distances nn

        appendedQueue = (queue ++ filter (`M.notMember` distances) nn)
      in
        bfs updatedDistances appendedQueue

    update :: Int -> Maybe Int -> Maybe Int
    update currentDistance Nothing = Just $ currentDistance+1
    update _ (Just d) = Just d


neighbors :: (Int, Int) -> Pipe -> [(Int, Int)]
neighbors (x, y) (Pipe NorthSouth Vertical) = [(x, y-1), (x, y+1)]
neighbors (x, y) (Pipe Horizontal WestEast) = [(x-1, y), (x+1, y)]
neighbors (x, y) (Pipe North East) = [(x, y-1), (x+1, y)]
neighbors (x, y) (Pipe North West) = [(x, y-1), (x-1, y)]
neighbors (x, y) (Pipe South West) = [(x,y+1), (x-1, y)]
neighbors (x, y) (Pipe South East) = [(x,y+1), (x+1, y)]
neighbors _ p = error $ "invalid pipe: " ++ show p
