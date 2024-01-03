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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Rebase.Prelude hiding (takeWhile)
import Prelude ()
import qualified Rebase.Data.UUID as T

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 1320): "
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
  -- putStrLn ""
  -- mapM_ (print . \i -> (i, hashSingle i)) input
  print . sum . map hashSingle $ input

part2inner :: Input2 -> IO ()
part2inner input = do
  print input

type Input1 = [Text]

type Input2 = ()

parseInput1 :: Parser Input1
parseInput1 = sepBy1' (takeWhile (`notElem` [',', '\n'])) (char ',')

parseInput2 :: Parser Input2
parseInput2 = return ()

hashSingle :: Text -> Int
hashSingle = hash' 0 . T.unpack
  where
    hash' :: Int -> String -> Int
    hash' h [] = h
    hash' h (c:cc) = hash' (((h + fromEnum c) * 17) `rem` 256) cc
