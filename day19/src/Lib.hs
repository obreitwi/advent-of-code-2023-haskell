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
import Data.Text.Internal.Encoding.Utf8 (DecoderResult (Accept))
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
  putStr "Part 2 debug (expecting 167409079868000): "
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
  print . sum . map sumR . testRatings workflows $ ratings

part2inner :: Input2 -> IO ()
part2inner input = do
  -- mapM_ print input
  -- mapM_ print $ testRanges input inputRating
  print . sum . map combinations $ testRanges input inputRating

inputRating :: RangedRating
inputRating =
  RangedRating
    { rrX = inputRange,
      rrM = inputRange,
      rrA = inputRange,
      rrS = inputRange
    }

inputRange :: Range
inputRange = Range 1 4000

type Input1 = ([Workflow], [Rating])

type Input2 = [RangedWorkflow]

parseInput1 :: Parser Input1
parseInput1 = (,) <$> sepBy1' parseWorkflow endOfLine <*> (endOfLine *> endOfLine *> sepBy1' parseRating endOfLine)

parseInput2 :: Parser Input2
parseInput2 = sepBy1' parseRangedWorkflow endOfLine

data Workflow = Workflow
  { wLabel :: T.Text,
    wSteps :: [Step],
    wFollowUp :: FollowUp
  }
  deriving (Show)

data RangedWorkflow = RangedWorkflow
  { rwLabel :: T.Text,
    rwSteps :: [RangedStep],
    rwFollowUp :: FollowUp
  }
  deriving (Show)

data FollowUp = Acceptance | Rejected | GoTo Text deriving (Show, Eq)

data Step = Step Condition FollowUp deriving (Show)

data RangedStep = RangedStep RangedCondition FollowUp deriving (Show)

data Condition = Condition RLens Operation Int deriving (Show)

data RangedCondition = RangedCondition RRLens Operation Int deriving (Show)

data Operation = More | Less deriving (Show, Eq)

data Rating = Rating
  { rX :: Int,
    rM :: Int,
    rA :: Int,
    rS :: Int
  }
  deriving (Show)

data RangedRating = RangedRating
  { rrX :: Range,
    rrM :: Range,
    rrA :: Range,
    rrS :: Range
  }
  deriving (Show)

data Range = Range Int Int deriving (Show)

parseWorkflow :: Parser Workflow
parseWorkflow = do
  wLabel <- parseLabel
  _ <- "{"
  wSteps <- sepBy1' parseStep (char ',')
  _ <- ","
  wFollowUp <- parseFollowUp
  _ <- "}"
  return Workflow {..}

parseRangedWorkflow :: Parser RangedWorkflow
parseRangedWorkflow = do
  rwLabel <- parseLabel
  _ <- "{"
  rwSteps <- sepBy1' parseRangedStep (char ',')
  _ <- ","
  rwFollowUp <- parseFollowUp
  _ <- "}"
  return RangedWorkflow {..}

parseStep :: Parser Step
parseStep = Step <$> parseCondition <*> (char ':' *> parseFollowUp)

parseRangedStep :: Parser RangedStep
parseRangedStep = RangedStep <$> parseRangedCondition <*> (char ':' *> parseFollowUp)

parseFollowUp :: Parser FollowUp
parseFollowUp = char 'A' $> Acceptance <|> char 'R' $> Rejected <|> GoTo <$> parseLabel

parseLabel :: Parser Text
parseLabel = takeWhile (inClass "a-z")

parseOperation :: Parser Operation
parseOperation = char '<' $> Less <|> char '>' $> More

parseCondition :: Parser Condition
parseCondition = Condition <$> parseRLens <*> parseOperation <*> decimal

parseRangedCondition :: Parser RangedCondition
parseRangedCondition = RangedCondition <$> parseRRLens <*> parseOperation <*> decimal

data RLens = RLens Char (Rating -> Int)

data RRLens = RRLens Char (RangedRating -> Range)

applyRLens :: RLens -> Rating -> Int
applyRLens (RLens _ f) = f

applyRRLens :: RRLens -> RangedRating -> Range
applyRRLens (RRLens _ f) = f

setRRLens :: RRLens -> RangedRating -> Range -> RangedRating
setRRLens (RRLens 'x' _) rr r = rr {rrX = r}
setRRLens (RRLens 'm' _) rr r = rr {rrM = r}
setRRLens (RRLens 'a' _) rr r = rr {rrA = r}
setRRLens (RRLens 's' _) rr r = rr {rrS = r}
setRRLens _ _ _ = error "invalid RRLens"

instance Show RLens where
  show (RLens label _) = [label]

instance Show RRLens where
  show (RRLens label _) = [label]

parseRLens :: Parser RLens
parseRLens = char 'x' $> RLens 'x' rX <|> char 'm' $> RLens 'm' rM <|> char 'a' $> RLens 'a' rA <|> char 's' $> RLens 's' rS

parseRRLens :: Parser RRLens
parseRRLens = char 'x' $> RRLens 'x' rrX <|> char 'm' $> RRLens 'm' rrM <|> char 'a' $> RRLens 'a' rrA <|> char 's' $> RRLens 's' rrS

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

testRanges :: [RangedWorkflow] -> RangedRating -> [RangedRating]
testRanges workflows = go [] . applyRW (wsm M.! "in")
  where
    wsm = M.fromList $ map (\w -> (rwLabel w, w)) workflows

    go :: [RangedRating] -> [(RangedRating, FollowUp)] -> [RangedRating]
    go accepted [] = accepted
    go accepted ((rr, Acceptance) : remaining) = go (rr : accepted) remaining
    go accepted ((rr, Rejected) : remaining) = go accepted remaining
    go accepted ((rr, GoTo label) : remaining) = go accepted $ applyRW (wsm M.! label) rr ++ remaining

applyRW :: RangedWorkflow -> RangedRating -> [(RangedRating, FollowUp)]
applyRW rw = go (rwSteps rw)
  where
    go :: [RangedStep] -> RangedRating -> [(RangedRating, FollowUp)]
    go [] rr = [(rr, rwFollowUp rw)]
    go ((RangedStep rc fu) : ss) rr =
      let (isWithin, isOut) = splitRC rc rr
       in determineIn isWithin ++ determineOut isOut
      where
        determineIn Nothing = []
        determineIn (Just r) = [(r, fu)]

        determineOut Nothing = []
        determineOut (Just r) = go ss r

splitRC :: RangedCondition -> RangedRating -> (Maybe RangedRating, Maybe RangedRating)
splitRC (RangedCondition rl op val) rr = join bimap (fmap $ setRRLens rl rr) $ go op val (applyRRLens rl rr)
  where
    go :: Operation -> Int -> Range -> (Maybe Range, Maybe Range)
    go Less v r@(Range low high)
      | high < v = (Just r, Nothing)
      | v < low = (Nothing, Just r)
      | otherwise = (Just $ Range low (v-1), Just $ Range v high)
    go More v r@(Range low high)
      | v < low = (Just r, Nothing)
      | high < v = (Nothing, Just r)
      | otherwise = (Just $ Range (v+1) high, Just $ Range low v)

combinations :: RangedRating -> Int
combinations rr = product . map rangeDist $ [rrX rr, rrM rr, rrA rr, rrS rr]

rangeDist :: Range -> Int
rangeDist (Range from to) = to - from + 1
