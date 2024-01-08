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

-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as U

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug (expecting 19114): "
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
part1inner input@(workflows, ratings) = do
  -- putStrLn ""
  -- mapM_ print . fst $ input
  -- putStrLn ""
  -- mapM_ print . snd $ input
  -- putStrLn ""
  -- mapM_ print . testRatings workflows $ ratings
  -- putStrLn ""
  print . sum . map sumR  . testRatings workflows $ ratings

part2inner :: Input2 -> IO ()
part2inner input = do
  print input

type Input1 = ([Workflow], [Rating])

type Input2 = ()

parseInput1 :: Parser Input1
parseInput1 = (,) <$> sepBy1' parseWorkflow endOfLine <*> (endOfLine *> endOfLine *> sepBy1' parseRating endOfLine)

parseInput2 :: Parser Input2
parseInput2 = return ()

data Workflow = Workflow
  { wLabel :: T.Text,
    wSteps :: [Step],
    wFollowUp :: FollowUp
  }
  deriving (Show)

data FollowUp = Acceptance | Rejected | GoTo Text deriving (Show, Eq)

data Step = Step Condition FollowUp deriving (Show)

data Condition = Condition RLens Operation Int deriving (Show)

data Operation = More | Less deriving (Show, Eq)

data Rating = Rating
  { rX :: Int,
    rM :: Int,
    rA :: Int,
    rS :: Int
  }
  deriving (Show)

parseWorkflow :: Parser Workflow
parseWorkflow = do
  wLabel <- parseLabel
  _ <- "{"
  wSteps <- sepBy1' parseStep (char ',')
  _ <- ","
  wFollowUp <- parseFollowUp
  _ <- "}"
  return Workflow {..}

parseStep :: Parser Step
parseStep = Step <$> parseCondition <*> (char ':' *> parseFollowUp)

parseFollowUp :: Parser FollowUp
parseFollowUp = char 'A' $> Acceptance <|> char 'R' $> Rejected <|> GoTo <$> parseLabel

parseLabel :: Parser Text
parseLabel = takeWhile (inClass "a-z")

parseOperation :: Parser Operation
parseOperation = char '<' $> Less <|> char '>' $> More

parseCondition :: Parser Condition
parseCondition = Condition <$> parseRLens <*> parseOperation <*> decimal

data RLens = RLens Char (Rating -> Int)

applyRLens :: RLens -> Rating -> Int
applyRLens (RLens _ f) = f

instance Show RLens where
  show (RLens label _) = [label]

parseRLens :: Parser RLens
parseRLens = char 'x' $> RLens 'x' rX <|> char 'm' $> RLens 'm' rM <|> char 'a' $> RLens 'a' rA <|> char 's' $> RLens 's' rS

parseRating :: Parser Rating
parseRating = do
  _ <- "{x="
  rX <- decimal
  _ <- ",m="
  rM <- decimal
  _ <- ",a="
  rA <- decimal
  _ <- ",s="
  rS <- decimal
  _ <- "}"
  return Rating {..}

testRatings :: [Workflow] -> [Rating] -> [Rating]
testRatings workflows = filter (go $ wsm M.! "in")
  where
    wsm = M.fromList $ map (\w -> (wLabel w, w)) workflows

    go :: Workflow -> Rating -> Bool
    go w r = case applyW w r of
      Rejected -> False
      Acceptance -> True
      GoTo l -> go (wsm M.! l) r

checkC :: Condition -> Rating -> Bool
checkC (Condition rl Less val) r = applyRLens rl r < val
checkC (Condition rl More val) r = applyRLens rl r > val

applyW :: Workflow -> Rating -> FollowUp
applyW w = go (wSteps w)
  where
    go [] r = wFollowUp w
    go ((Step c f) : ss) r
      | checkC c r = f
      | otherwise = go ss r

sumR :: Rating -> Int
sumR r = rX r + rM r + rA r + rS r
