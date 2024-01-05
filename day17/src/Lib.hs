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

import Data.Attoparsec.Text hiding (take)
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Rebase.Data.UUID as T
import Rebase.Prelude hiding (takeWhile)
import Prelude ()

-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as U

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 102): "
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
  -- print input
  -- let (CB nC v) = input
  -- print . U.length $ v
  -- putStrLn ""
  -- printCB input
  let (dimX, dimY) = dimensions input
  print $ findPathMinHeat input (0, 0) (dimX - 1, dimY - 1)

part2inner :: Input2 -> IO ()
part2inner input = do
  print input

type Input1 = CityBlocks

type Input2 = ()

parseInput1 :: Parser Input1
parseInput1 = parseCityBlocks

parseInput2 :: Parser Input2
parseInput2 = return ()

data CityBlocks = CB Int (U.Vector Int) deriving (Show, Eq)

parseCityBlocks :: Parser CityBlocks
parseCityBlocks = do
  rows <- sepBy1' (many1' digit) endOfLine
  let rowSize = length . head $ rows
  unless (all ((== rowSize) . length) rows) $ error "not all rows have same length"
  return $ CB rowSize (U.fromList . map digitToInt . concat $ rows)

type Position = (Int, Int)

type Momentum = (Position, Direction)

data Direction = DirLeft | DirRight | DirUp | DirDown deriving (Show, Eq, Ord)

turnOptions :: Direction -> [Direction]
turnOptions DirUp = [DirLeft, DirRight]
turnOptions DirRight = [DirUp, DirDown]
turnOptions DirDown = [DirRight, DirLeft]
turnOptions DirLeft = [DirDown, DirUp]

getHeat :: CityBlocks -> Position -> Maybe Int
getHeat (CB nC v) (i, j)
  | i < 0 || j < 0 || i >= nC || j * nC >= U.length v = Nothing
  | otherwise = Just $ v U.! (j * nC + i)

unsafeGetHeat :: CityBlocks -> Position -> Int
unsafeGetHeat (CB nC v) (i, j) = v U.! (j * nC + i)

printCB :: CityBlocks -> IO ()
printCB (CB nC v)
  | U.null v = return ()
  | otherwise = printVector (U.take nC v) >> printCB (CB nC (U.drop nC v))

printVector :: (U.Unbox a) => (Show a) => U.Vector a -> IO ()
printVector = putStrLn . filter (/= '"') . show

advance :: Momentum -> Momentum
advance ((i, j), DirLeft) = ((i - 1, j), DirLeft)
advance ((i, j), DirRight) = ((i + 1, j), DirRight)
advance ((i, j), DirUp) = ((i, j - 1), DirUp)
advance ((i, j), DirDown) = ((i, j + 1), DirDown)

dimensions :: CityBlocks -> (Int, Int)
dimensions (CB numCols vec) = (numCols, U.length vec `quot` numCols)

type Heat = Int

data Trajectory = Trajectory
  { m :: Momentum,
    straightStepsLeft :: Int,
    heat :: Int
  }
  deriving (Show, Eq)

instance Ord Trajectory where
  compare l r = (heat l, fst . m $ l, straightStepsLeft l, snd . m $ l) `compare` (heat r, fst . m $ r, straightStepsLeft r, snd . m $ r)

-- tracePre :: Show a => String -> a -> a
-- tracePre label x = trace (label ++ show x) x
tracePre :: String -> a -> a
tracePre _ x = x

step :: Trajectory -> (Momentum, Int)
step t = (m t, straightStepsLeft t)

findPathMinHeat :: CityBlocks -> Position -> Position -> Int
findPathMinHeat cb start target = go (S.map step startPos) startPos
  where
    startPos :: S.Set Trajectory
    startPos = tracePre "startPos: " $ foldl1' S.union $ map ((\m -> setupTrajectory m (getH . fst $ m)) . advance . (start,)) [DirLeft, DirRight, DirUp, DirDown]

    setupTrajectory :: Momentum -> Maybe Heat -> S.Set Trajectory
    setupTrajectory _ Nothing = S.empty
    setupTrajectory m (Just heat) = S.singleton Trajectory {straightStepsLeft = 2, ..}

    getH = getHeat cb

    go :: S.Set (Momentum, Int) -> S.Set Trajectory -> Int
    go visited tt =
      let (cur, tt') = S.deleteFindMin tt
          newTrajectories = possibleTrajectories cur
          filteredTrajectories = S.filter (flip S.notMember visited . step) newTrajectories
          updatedVisited = S.union visited $ S.map step newTrajectories
          tt'' = tracePre "current trajectories: " $ filteredTrajectories `S.union` tt'
       in if (fst . m) cur == target
            then heat cur
            else go updatedVisited tt''

    -- directory should be different from current
    advanceTrajectoryDiffDir :: Trajectory -> Direction -> S.Set Trajectory
    advanceTrajectoryDiffDir Trajectory {..} d =
      updateHeat
        Trajectory
          { m = advance (fst m, d),
            straightStepsLeft = 2,
            ..
          }

    possibleTrajectories :: Trajectory -> S.Set Trajectory
    possibleTrajectories t@Trajectory {..} = tracePre ("new trajectories from " ++ show t ++ ": ") $ goStraight t `S.union` (foldl1' S.union . map (advanceTrajectoryDiffDir t)) (turnOptions . snd $ m)

    -- check if trajectory can go straight
    goStraight :: Trajectory -> S.Set Trajectory
    goStraight Trajectory {..}
      | straightStepsLeft == 0 = S.empty
      | otherwise =
          updateHeat
            Trajectory
              { m = advance m,
                straightStepsLeft = straightStepsLeft - 1,
                ..
              }

    updateHeat :: Trajectory -> S.Set Trajectory
    updateHeat Trajectory {m = m@(p, _), ..} =
      case getH p of
        Just delta ->
          S.singleton
            Trajectory
              { heat = heat + delta,
                ..
              }
        Nothing -> S.empty

trajHeat :: Trajectory -> Int
trajHeat Trajectory {..} = heat

trajPos :: Trajectory -> Position
trajPos Trajectory {..} = fst m

-- go :: M.Map Position Heat -> [Trajectory] -> M.Map Position Heat
-- go visited [] = visited
-- go visited ((m@(p, _), straightStepsLeft) : ss) =
-- let mNext@(pNext, nD) = advance m
-- mbHeatNext = getH pNext
-- in case mbHeatNext of
-- Nothing -> go visited ss -- outside of field -> discard
-- Just heatDelta -> undefined

-- updateHeat :: M.Map Position Heat -> Momentum -> M.Map Position Heat
-- updateHeat visited m@(pos, _)
-- \| heatNext < heatPrev = M.insert posNext heatNext visited
-- \| otherwise = visited
-- where
-- (posNext, _) = advance m
-- heatParent = visited M.! pos

-- heatPrev :: Int
-- heatPrev = fromMaybe maxBound $ visited M.!? posNext

-- heatNext = heatParent + delta

-- delta = mustGetH posNext
