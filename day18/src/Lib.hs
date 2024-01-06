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
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Rebase.Data.UUID as T
import Rebase.Prelude hiding (takeWhile, take)
import Prelude ()

-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as U

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 62): "
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
  putStrLn ""
  -- mapM_ print input
  let outline = drawOutline input
  let filled = fillOutline outline
  printOutline filled
  putStrLn ""
  print . S.size $ filled

part2inner :: Input2 -> IO ()
part2inner input = do
  mapM_ print  input
  mapM_ print . foldl1' S.intersection . traceShowId . map (primeFactors . snd) $ input

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

-- fillOutline :: S.Set Position -> S.Set Position
-- fillOutline outline = outline `S.union` S.filter (checkInner outline bnds) ((S.fromList [(i, j) | i <- [xMin .. xMax], j <- [yMin .. yMax]]) S.\\ outline)
-- where
-- bnds@((xMin, yMin), (xMax, yMax)) = bounds outline

fillOutline :: S.Set Position -> S.Set Position
fillOutline outline = inner `S.union` outline
  where
    inner = allPoints (bounds outline) S.\\ foldl (\visited pos -> flood visited pos) outline (S.toList $ findPointsOutside outline)

type Bounds = ((Int, Int), (Int, Int))

outOfBounds :: Bounds -> Position -> Bool
outOfBounds b@((xMin, yMin), (xMax, yMax)) (x, y) = (x < xMin) || (x > xMax) || (y < yMin) || (y > yMax)

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
        -- newPositions = traceShowId $ filter (`S.notMember` visited) . traceShowId . filter (not . outOfBounds bnds) . traceShowId .  map (fst . advance . (p,)) $ allDirections
        newPositions = filter (`S.notMember` visited) . filter (not . outOfBounds bnds) . map (fst . advance . (p,)) $ allDirections

-- checkInner :: S.Set Position -> Bounds -> Position -> Bool
-- checkInner outline bounds p = (sum . map ((`rem` 2) . (go 0 False) . (p,)) $ [DirLeft, DirRight, DirUp, DirDown]) `rem` 4 == 0
-- where
-- go :: Int -> Bool -> Momentum -> Int
-- go numTransitions onOutline cur
-- \| outOfBounds bounds $ fst cur = numTransitions
-- \| otherwise = case (onOutline, fst cur `S.member` outline) of
-- (True, True) -> go numTransitions True $ advance cur
-- (False, True) -> go (numTransitions + 1) True $ advance cur
-- (_, False) -> go numTransitions False $ advance cur

allPoints :: Bounds -> S.Set Position
allPoints ((xMin, yMin), (xMax, yMax)) = (S.fromList [(i, j) | i <- [xMin .. xMax], j <- [yMin .. yMax]])

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
advance ((i, j), DirLeft) = ((i - 1, j), DirLeft)
advance ((i, j), DirRight) = ((i + 1, j), DirRight)
advance ((i, j), DirUp) = ((i, j - 1), DirUp)
advance ((i, j), DirDown) = ((i, j + 1), DirDown)

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

primeFactors :: Int -> S.Set Int
primeFactors = S.fromList . go [] 2
  where
    go :: [Int] -> Int -> Int -> [Int]
    go factors toTest 1 = factors
    go factors toTest cur
      | cur `rem` toTest == 0 = go (toTest:factors) 2 (cur `quot` toTest)
      | otherwise = go factors (toTest+1) cur
