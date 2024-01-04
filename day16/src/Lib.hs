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

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting XXX): "
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
  mapM_ printVector input
  putStrLn ""
  printBeam input $ castBeam input ((0, 0), DirRight)
  print . length $ castBeam input ((0, 0), DirRight)

part2inner :: Input2 -> IO ()
part2inner layout = do
  print . maximumBy (compare `on` snd) . map (\p -> (p, S.size . castBeam layout $ p)) $ possibleStarts layout
  -- print maximumBy (compare `on` )

type Layout = V.Vector (U.Vector Char)

type Input1 = Layout

type Input2 = Input1

emptySpace :: Char
emptySpace = '.'

mirrorBack :: Char
mirrorBack = '\\'

mirrorForth :: Char
mirrorForth = '/'

splitterH :: Char
splitterH = '-'

splitterV :: Char
splitterV = '|'

parseInput1 :: Parser Input1
parseInput1 = parseLayout

parseLayout :: Parser Layout
parseLayout = do
  rows <-
    sepBy1'
      ( many1' $
          char emptySpace
            <|> char mirrorBack
            <|> char mirrorForth
            <|> char splitterH
            <|> char splitterV
      )
      endOfLine
  let vows = map U.fromList rows
  return $ V.fromList vows

parseInput2 :: Parser Input2
parseInput2 = parseInput1

printVector :: (U.Unbox a) => (Show a) => U.Vector a -> IO ()
printVector = TIO.putStrLn . T.replace "\\\\" "\\" . T.pack . filter (/= '"') . show

data Direction = DirLeft | DirRight | DirUp | DirDown deriving (Show, Eq, Ord)

data Cell = EmptySpace | MirrorBack | MirrorForth | SplitterH | SplitterV deriving (Show, Ord, Eq)

charToCell :: Char -> Cell
charToCell c
  | c == emptySpace = EmptySpace
  | c == mirrorBack = MirrorBack
  | c == mirrorForth = MirrorForth
  | c == splitterV = SplitterV
  | c == splitterH = SplitterH
  | otherwise = error $ printf "invalid char: %s" c

type Position = (Int, Int)

type Momentum = (Position, Direction)

getCell :: Layout -> Position -> Maybe Cell
getCell l = fmap charToCell . getc
  where
    getc :: Position -> Maybe Char
    getc (i, j)
      | i < 0 = Nothing
      | j < 0 = Nothing
      | i >= numCols = Nothing
      | j >= numRows = Nothing
      | otherwise = Just $ (l V.! j) U.! i

    (numRows, numCols) = dimensions l

dimensions :: Layout -> (Int, Int)
dimensions l = (V.length l, U.length . V.head $ l)

castBeam :: Layout -> Momentum -> S.Set Position
castBeam layout momentum = S.fromList . map fst . S.toList $ go S.empty [momentum]
  where
    go :: S.Set Momentum -> [Momentum] -> S.Set Momentum
    go seen [] = seen
    go seen (m@(p, _) : mm)
      | m `S.member` seen = go seen mm
      | otherwise = walk seen (m : mm) (getCell layout p)

    walk :: S.Set Momentum -> [Momentum] -> Maybe Cell -> S.Set Momentum
    walk seen [] _ = seen
    walk seen (_ : mm) Nothing = go seen mm
    walk seen (m : mm) (Just c) = walkIn (S.insert m seen) (m : mm) c

    walkIn :: S.Set Momentum -> [Momentum] -> Cell -> S.Set Momentum
    walkIn seen (m : mm) EmptySpace = go seen $ advance m : mm
    walkIn seen (m : mm) MirrorBack = go seen $ reflect MirrorBack m : mm
    walkIn seen (m : mm) MirrorForth = go seen $ reflect MirrorForth m : mm
    walkIn seen mm SplitterH = split seen SplitterH mm
    walkIn seen mm SplitterV = split seen SplitterV mm
    walkIn _ [] _ = error "invalid empty queue"

    split :: S.Set Momentum -> Cell -> [Momentum] -> S.Set Momentum

    split seen SplitterH ((p, DirLeft) : mm) = go seen $ advance (p, DirLeft) : mm
    split seen SplitterH ((p, DirRight) : mm) = go seen $ advance (p, DirRight) : mm
    split seen SplitterH ((p, DirUp) : mm) = go seen $ splitH p ++ mm
    split seen SplitterH ((p, DirDown) : mm) = go seen $ splitH p ++ mm
    split seen SplitterV ((p, DirUp) : mm) = go seen $ advance (p, DirUp) : mm
    split seen SplitterV ((p, DirDown) : mm) = go seen $ advance (p, DirDown) : mm
    split seen SplitterV ((p, DirLeft) : mm) = go seen $ splitV p ++ mm
    split seen SplitterV ((p, DirRight) : mm) = go seen $ splitV p ++ mm
    split _ _ [] = error "split called with empty stack"
    split _ _ _ = error "split called with non-splitter"

    splitH :: Position -> [Momentum]
    splitH p = [advance (p, DirLeft), advance (p, DirRight)]

    splitV :: Position -> [Momentum]
    splitV p = [advance (p, DirUp), advance (p, DirDown)]

printBeam :: Layout -> S.Set Position -> IO ()
printBeam l energized = mapM_ printRow [0 .. (numRows - 1)] >> putStrLn ""
  where
    (numRows, numCols) = dimensions l

    printRow :: Int -> IO ()
    printRow j = mapM_ printRay [(i, j) | i <- [0 .. (numCols - 1)]] >> putStrLn ""

    printRay :: Position -> IO ()
    printRay p
      | p `elem` energized = putChar '#'
      | otherwise = putChar '.'

advance :: Momentum -> Momentum
advance ((i, j), DirLeft) = ((i - 1, j), DirLeft)
advance ((i, j), DirRight) = ((i + 1, j), DirRight)
advance ((i, j), DirUp) = ((i, j - 1), DirUp)
advance ((i, j), DirDown) = ((i, j + 1), DirDown)

reflect :: Cell -> Momentum -> Momentum
reflect MirrorBack (p, DirDown) = advance (p, DirRight)
reflect MirrorBack (p, DirLeft) = advance (p, DirUp)
reflect MirrorBack (p, DirRight) = advance (p, DirDown)
reflect MirrorBack (p, DirUp) = advance (p, DirLeft)
reflect MirrorForth (p, DirDown) = advance (p, DirLeft)
reflect MirrorForth (p, DirLeft) = advance (p, DirDown)
reflect MirrorForth (p, DirRight) = advance (p, DirUp)
reflect MirrorForth (p, DirUp) = advance (p, DirRight)
reflect _ _ = error "reflect called with non-mirror"

possibleStarts :: Layout -> [Momentum]
possibleStarts l =
  [((i, 0), DirDown) | i <- [0 .. (numCols - 1)]]
    ++ [((i, numRows - 1), DirUp) | i <- [0 .. (numCols - 1)]]
    ++ [((0, j), DirRight) | j <- [0 .. (numRows - 1)]]
    ++ [((numCols - 1, j), DirLeft) | j <- [0 .. (numRows - 1)]]
  where
    (numRows, numCols) = dimensions l
