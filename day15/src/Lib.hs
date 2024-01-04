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
import qualified Rebase.Data.UUID as T
import Rebase.Prelude hiding (takeWhile)
import Prelude ()

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 1320): "
  either error part1inner $ parseOnly parseInput1 s
  putStr "Part 2 debug (expecting 145): "
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
  -- mapM_ print input
  -- print . sum . map hashInstruction $ input
  -- mapM_ print . M.toList . performInstructions $ input
  print  . lensScore . performInstructions $ input

type Input1 = [Text]

type Input2 = [Instruction]

parseInput1 :: Parser Input1
parseInput1 = sepBy1' (takeWhile (`notElem` [',', '\n'])) (char ',')

parseInput2 :: Parser Input2
parseInput2 = sepBy1' parseInstruction (char ',')

hashSingle :: Text -> Int
hashSingle = hash' 0 . T.unpack
  where
    hash' :: Int -> String -> Int
    hash' h [] = h
    hash' h (c : cc) = hash' (((h + fromEnum c) * 17) `rem` 256) cc

data Instruction = Inst Text Operation deriving (Show, Eq, Ord)

data Operation = Removal | Replace Int deriving (Show, Eq, Ord)

parseInstruction :: Parser Instruction
parseInstruction = Inst <$> takeWhile isAlpha <*> parseOperation

parseOperation :: Parser Operation
parseOperation = char '-' $> Removal <|> char '=' $> Replace <*> decimal

hashInstruction :: Instruction -> Int
hashInstruction (Inst label _ ) = hashSingle label

textInstruction :: Instruction -> Text
textInstruction (Inst label operation) = label <> textOperation operation

textOperation :: Operation -> Text
textOperation Removal = T.singleton '-'
textOperation (Replace val) = T.singleton '=' <> T.pack (show val)

data Lens = Lens {lensLabel :: Text, lensValue :: Int} deriving (Show, Eq, Ord)

performInstructions :: [Instruction] -> M.Map Int [Lens]
performInstructions = foldl' (flip perform) M.empty
  where
    perform :: Instruction -> M.Map Int [Lens] -> M.Map Int [Lens]
    perform i = M.alter (applyInstruction i) (hashInstruction i)

applyInstruction :: Instruction -> Maybe [Lens] -> Maybe [Lens]
applyInstruction (Inst _ Removal) Nothing = Nothing
applyInstruction (Inst _ Removal) (Just []) = Nothing
applyInstruction (Inst label Removal) (Just box) = Just $ filter ((/= label) . lensLabel) box
applyInstruction (Inst lensLabel (Replace lensValue)) Nothing = Just [Lens {..}]
applyInstruction (Inst lensLabel (Replace lensValue)) (Just []) = Just [Lens {..}]
applyInstruction (Inst label (Replace value)) (Just box)
  | isNothing . find ((== label) . lensLabel) $ box = Just $ box ++ [Lens {lensLabel = label, lensValue = value}]
  | otherwise = Just $ map updateLens box
  where
    updateLens lens
      | lensLabel lens == label = Lens {lensValue = value, lensLabel = label }
      | otherwise = lens

lensScore :: M.Map Int [Lens] -> Int
lensScore = sum . map (\(idx, box) -> boxScore (idx+1) box) . M.toList

boxScore :: Int -> [Lens] -> Int
boxScore boxIdx = sum . zipWith (\lensIdx lens -> boxIdx * lensIdx * lensValue lens) [1..]
