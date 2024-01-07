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

import Data.Attoparsec.Text
import qualified Data.IntMap as V
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (generate)
import qualified Data.Vector.Unboxed as U
import qualified Rebase.Data.UUID as T
import Rebase.Prelude hiding (take, takeWhile)
import Prelude ()

-- import qualified Data.Vector as V

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 62): "
  either error part1inner $ parseOnly parseInput1 s
  putStr "Part 2 debug (expecting 952408144115): "
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
  -- putStrLn ""
  -- mapM_ print input
  let outline = drawOutline input
  let filled = fillOutline outline
  -- printOutline filled
  -- putStrLn ""
  print . S.size $ filled
  putStrLn ""

mockInput :: [(Direction, Int)]
mockInput =
  [ (DirUp, 1000),
    (DirRight, 900),
    (DirDown, 100),
    (DirRight, 100),
    (DirDown, 900),
    (DirLeft, 1000)
  ]

part2inner :: Input2 -> IO ()
part2inner input = do
  -- putStrLn ""
  -- mapM_ print input
  -- let input = mockInput
  let outline = drawOutlinePointwise input
  let rg = rescaleGrid . S.fromList $ outline
  let rescaled = drawOutlineRescaled rg input
  let filled = fillOutline rescaled
  let area = totalArea rg outline filled
  -- mapM_ print rescaled
  -- putStrLn ""
  -- putStrLn "Rescaled:"
  -- putStrLn ""
  -- printOutline rescaled
  -- putStrLn ""
  -- putStrLn "Filled:"
  -- putStrLn ""
  -- printOutline filled
  -- putStrLn ""
  -- putStr "Diff from want: "
  -- print $ 952408144115 - area

  print area

type Input1 = [(Direction, Int)]

type Input2 = Input1

parseInput1 :: Parser Input1
parseInput1 = do
  sepBy1' parseInstruction endOfLine

parseInput2 :: Parser Input2
parseInput2 = do
  sepBy1' parseInstructionHex endOfLine

data Direction = DirLeft | DirRight | DirUp | DirDown deriving (Show, Eq, Ord)

parseInstructionHex :: Parser (Direction, Int)
parseInstructionHex = do
  _ <- parseDirection
  _ <- skipSpace
  _ <- decimal
  _ <- " (#"
  i <- parseHex
  d <- parseDirectionDigit
  _ <- char ')'
  return (d, i)

parseDirectionDigit :: Parser Direction
parseDirectionDigit = char '0' $> DirRight <|> char '1' $> DirDown <|> char '2' $> DirLeft <|> char '3' $> DirUp

parseHex :: Parser Int
parseHex = do
  c <- take 5
  return $ hexToInt . T.unpack $ c

parseInstruction :: Parser (Direction, Int)
parseInstruction = do
  d <- parseDirection
  _ <- skipSpace
  i <- decimal
  _ <- takeWhile ('\n' /=)
  return (d, i)

parseDirection :: Parser Direction
parseDirection = char 'R' $> DirRight <|> char 'L' $> DirLeft <|> char 'U' $> DirUp <|> char 'D' $> DirDown

type Position = (Int, Int)

drawOutline :: [(Direction, Int)] -> S.Set Position
drawOutline = go (S.singleton (0, 0)) (0, 0)
  where
    go :: S.Set Position -> Position -> [(Direction, Int)] -> S.Set Position
    go visited _ [] = visited
    go visited cur (i : ii) =
      let newPos = walk cur i
       in go (visited `S.union` S.fromList newPos) (last newPos) ii

drawOutlinePointwise :: [(Direction, Int)] -> [Position]
drawOutlinePointwise = go [(0, 0)] (0, 0)
  where
    go :: [Position] -> Position -> [(Direction, Int)] -> [Position]
    go visited _ [] = visited
    go visited cur ((d, i) : ii) =
      let newPos = fst $ advanceN i (cur, d)
       in go (newPos : visited) newPos ii

drawOutlineRescaled :: RescaledGrid -> [(Direction, Int)] -> S.Set Position
drawOutlineRescaled rg@RescaledGrid {..} = go (S.singleton (shrink rg (0, 0))) (0, 0)
  where
    go :: S.Set Position -> Position -> [(Direction, Int)] -> S.Set Position
    go visited _ [] = visited
    go visited cur ((d, i) : ii) =
      let m = (cur, d)
          newPos = fst $ advanceN i m

          newPosRescaled = tracePre "newPosRescaled: " $ shrink rg newPos
          curRescaled = tracePre "curRescaled: " $ shrink rg cur

          dist :: Int
          dist = tracePre "dist: " $ newPosRescaled `absDistance` curRescaled

          newPoints = tracePre "newPoints: " $ map (fst . (`advanceN` (curRescaled, d))) [1 .. dist]
       in go (visited `S.union` S.fromList newPoints) newPos ii

-- tracePre :: (Show a) => String -> a -> a
-- tracePre label x = trace (label ++ show x) x

tracePre :: String -> a -> a
tracePre _ x = x

absDistance :: Position -> Position -> Int
absDistance (lX, lY) (rX, rY) = max (abs (lX - rX)) (abs (lY - rY))

fillOutline :: S.Set Position -> S.Set Position
fillOutline outline = inner `S.union` outline
  where
    inner = allPoints (bounds outline) S.\\ foldl flood outline (S.toList $ findPointsOutside outline)

type Bounds = ((Int, Int), (Int, Int))

outOfBounds :: Bounds -> Position -> Bool
outOfBounds ((xMin, yMin), (xMax, yMax)) (x, y) = (x < xMin) || (x > xMax) || (y < yMin) || (y > yMax)

allDirections :: [Direction]
allDirections = [DirLeft, DirRight, DirUp, DirDown]

flood :: S.Set Position -> Position -> S.Set Position
flood boundary start
  | start `S.member` boundary = boundary
  | otherwise -- trace ("Flooding for " ++ show start) $
    =
      go boundary [start]
  where
    bnds = bounds boundary

    traceStack s = trace ("[" ++ show start ++ "] stack size: " ++ (show . length $ s)) s

    go :: S.Set Position -> [Position] -> S.Set Position
    go visited [] = visited
    go visited (p : pp) = go (visited `S.union` S.fromList newPositions) (newPositions ++ pp)
      where
        newPositions = filter (`S.notMember` visited) . filter (not . outOfBounds bnds) . map (fst . advance . (p,)) $ allDirections

-- checkInner :: S.Set Position -> Bounds -> Position -> Bool
-- checkInner outline bounds p = (sum . map ((`rem` 2) . (go 0 False) . (p,)) $ [DirLeft, DirRight, DirUp, DirDown]) `rem` 4 == 0
-- where
-- go :: Int -> Bool -> Momentum -> Int
-- go numTransitions onOutline cur
-- \| outOfBounds bounds $ fst cur = numTransitions
-- \| otherwise = case (onOutline, fst cur `S.member` outline) of
-- (True, True) -> go numTransitions True $ advance cur
-- (False, True -> go (numTransitions + 1) True $ advance cur
-- (_, False) -> go numTransitions False $ advance cur

allPoints :: Bounds -> S.Set Position
allPoints ((xMin, yMin), (xMax, yMax)) = S.fromList [(i, j) | i <- [xMin .. xMax], j <- [yMin .. yMax]]

findPointsOutside :: S.Set Position -> S.Set Position
findPointsOutside outline = circumference S.\\ outline
  where
    ((xMin, yMin), (xMax, yMax)) = bounds outline
    circumference = S.fromList $ [(xMin, j) | j <- [yMin .. yMax]] ++ [(xMax, j) | j <- [yMin .. yMax]] ++ [(i, yMin) | i <- [xMin .. xMax]] ++ [(i, yMax) | i <- [xMin .. xMax]]

walk :: Position -> (Direction, Int) -> [Position]
walk _ (_, 0) = []
walk p (d, i) =
  let next = fst . advance $ (p, d)
   in next : walk next (d, i - 1)

type Momentum = (Position, Direction)

advance :: Momentum -> Momentum
advance = advanceN 1

advanceN :: Int -> Momentum -> Momentum
advanceN n ((i, j), DirLeft) = ((i - n, j), DirLeft)
advanceN n ((i, j), DirRight) = ((i + n, j), DirRight)
advanceN n ((i, j), DirUp) = ((i, j - n), DirUp)
advanceN n ((i, j), DirDown) = ((i, j + n), DirDown)

bounds :: S.Set Position -> (Position, Position)
bounds pos =
  ( (S.findMin . S.map fst $ pos, S.findMin . S.map snd $ pos),
    (S.findMax . S.map fst $ pos, S.findMax . S.map snd $ pos)
  )

printOutline :: S.Set Position -> IO ()
printOutline pos = mapM_ printRow [yMin .. yMax]
  where
    ((xMin, yMin), (xMax, yMax)) = bounds pos

    printRow j = mapM_ (printCell . (,j)) [xMin .. xMax] >> putStrLn ""

    printCell p
      | p `S.member` pos = putChar '#'
      | otherwise = putChar '.'

hexToInt :: String -> Int
hexToInt = go 0
  where
    go c [] = c
    go c (s : ss) = go (c * 16 + hexDigitToInt s) ss

hexDigitToInt :: Char -> Int
hexDigitToInt '0' = 0
hexDigitToInt '1' = 1
hexDigitToInt '2' = 2
hexDigitToInt '3' = 3
hexDigitToInt '4' = 4
hexDigitToInt '5' = 5
hexDigitToInt '6' = 6
hexDigitToInt '7' = 7
hexDigitToInt '8' = 8
hexDigitToInt '9' = 9
hexDigitToInt 'a' = 10
hexDigitToInt 'b' = 11
hexDigitToInt 'c' = 12
hexDigitToInt 'd' = 13
hexDigitToInt 'e' = 14
hexDigitToInt 'f' = 15
hexDigitToInt _ = error "invalid hex digit"

data RescaledGrid = RescaledGrid
  { stepsX :: U.Vector Int,
    stepsY :: U.Vector Int,
    invX :: M.Map Int Int,
    invY :: M.Map Int Int
  }
  deriving (Show)

rescaleGrid :: S.Set Position -> RescaledGrid
rescaleGrid pos = RescaledGrid {..}
  where
    stepsX = U.fromList . insertMidPoints . sort . S.toList . S.map fst $ pos
    stepsY = U.fromList . insertMidPoints . sort . S.toList . S.map snd $ pos

    invX = M.fromList $ zipWith (curry swap) [0 ..] $ U.toList stepsX
    invY = M.fromList $ zipWith (curry swap) [0 ..] $ U.toList stepsY

-- insert points on the middle between existing points
insertMidPoints :: [Int] -> [Int]
insertMidPoints [] = []
insertMidPoints [f] = [f - 1, f, f + 1]
insertMidPoints (f : s : rest) = f - 1 : f : f + 1 : insertMidPoints (s : rest)

-- where
-- delta = (s - f) `quot` 2
-- mid = f + delta

-- transform large points into simple indices
shrink :: RescaledGrid -> Position -> Position
shrink RescaledGrid {..} (i, j) = (invX M.! i, invY M.! j)

expand :: RescaledGrid -> Position -> Position
expand RescaledGrid {..} (i, j) = (stepsX U.! i, stepsY U.! j)

-- return all points which have their corresponding rectangle (in which they are top left) fully contained in the set
containedRectangles :: S.Set Position -> S.Set Position
-- containedRectangles pos = (\after -> trace (printf "Filtered %d entries: " (S.size pos - S.size after) ++ show (pos S.\\ after)) after) $ S.filter (all (`S.member` pos) . rectangleFromTopLeft) pos
containedRectangles pos = S.filter (all (`S.member` pos) . rectangleFromTopLeft) pos
  where
    rectangleFromTopLeft (i, j) = [(i + 1, j), (i, j + 1), (i + 1, j + 1)]

areaRectangle :: RescaledGrid -> Position -> Int
areaRectangle rg topLeftIdx = height * width
  where
    topLeft = expand rg topLeftIdx

    topRight = expand rg (fst topLeftIdx + 1, snd topLeftIdx)

    bottomLeft = expand rg (fst topLeftIdx, snd topLeftIdx + 1)

    height = topLeft `absDistance` bottomLeft
    width = topLeft `absDistance` topRight

innerArea :: RescaledGrid -> S.Set Position -> Int
innerArea rg = sum . map (areaRectangle rg) . S.toList . containedRectangles

lengthOutline :: [Position] -> Int
lengthOutline [] = 0
lengthOutline [s] = 0
lengthOutline (f : s : other) = absDistance f s + lengthOutline (s : other)

totalArea :: RescaledGrid -> [Position] -> S.Set Position -> Int
totalArea rg outline inner = 1 + innerArea rg inner + (lengthOutline outline `quot` 2)
