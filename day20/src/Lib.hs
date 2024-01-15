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
import qualified Data.Bifunctor as B
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Rebase.Data.Bifunctor.Flip (Flip)
import Rebase.Prelude hiding (takeWhile)
import Prelude ()

-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as U

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  putStr "Part 1 debug #1 (expecting 32000000): "
  either error part1inner $ parseOnly parseInput1 s
  s2 <- TIO.readFile "debug2.txt"
  putStr "Part 1 debug #2 (expecting 11687500): "
  either error part1inner $ parseOnly parseInput1 s2
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
  print . multiplyStats . snd $ processN 1000 input

part2inner :: Input2 -> IO ()
part2inner input = do
  print input

type Input1 = Circuit

type Input2 = ()

parseInput1 :: Parser Input1
parseInput1 = parseCircuit

parseInput2 :: Parser Input2
parseInput2 = return ()

data Circuit = Circuit
  { connects :: M.Map Label Targets,
    components :: M.Map Label Component
  }
  deriving (Show)

type Label = T.Text

class Perform a where
  label :: a -> Label
  send :: a -> Label -> Pulse -> (a, Maybe Pulse)

data Component = CompB BroadCast | CompC Conjunction | CompF FlipFlop deriving (Show)

data Pulse = High | Low deriving (Show, Eq)

data BroadCast = BroadCast deriving (Show)

type Targets = [Label]

data Conjunction = Conjunction
  { cLabel :: Label,
    cInputs :: M.Map Label Pulse
  }
  deriving (Show)

data FlipFlop = FlipFlop
  { fLabel :: Label,
    fState :: Pulse
  }
  deriving (Show)

parseCircuit :: Parser Circuit
parseCircuit = do
  rawComponents <- sepBy1' parseComponent endOfLine
  let connects = M.fromList $ map (B.first label) rawComponents
  let uniqTargets = S.fromList . M.keys $ connects
  let inputs = M.fromList $ map (\u -> (u, map fst . filter (elem u . snd) $ M.toList connects)) . S.toList $ uniqTargets
  let components = M.fromList $ map ((\c -> (label c, addInputs c (inputs M.! label c))) . fst) rawComponents
  return Circuit {..}
  where
    addInputs :: Component -> [Label] -> Component
    addInputs (CompC Conjunction {..}) inputs = CompC (Conjunction {cInputs = M.fromList . map (,Low) $ inputs, ..})
    addInputs c _ = c

parseComponent :: Parser (Component, Targets)
parseComponent = (B.first CompB <$> parseBroadCast) <|> (B.first CompC <$> parseConjunction) <|> (B.first CompF <$> parseFlipFlop)

parseBroadCast :: Parser (BroadCast, Targets)
parseBroadCast = do
  _ <- "broadcaster -> "
  targets <- parseTargets
  return (BroadCast, targets)

parseConjunction :: Parser (Conjunction, Targets)
parseConjunction = do
  _ <- char '&'
  cLabel <- takeWhile isAlpha
  _ <- " -> "
  targets <- parseTargets
  let cInputs = M.empty
  return (Conjunction {..}, targets)

parseFlipFlop :: Parser (FlipFlop, Targets)
parseFlipFlop = do
  _ <- char '%'
  fLabel <- takeWhile isAlpha
  _ <- " -> "
  let fState = High
  targets <- parseTargets
  return (FlipFlop {..}, targets)

parseTargets :: Parser Targets
parseTargets = sepBy1' (takeWhile isAlpha) ", "

instance Perform BroadCast where
  label _ = "broadcaster"
  send b _ p = (b, Just p)

instance Perform Conjunction where
  label = cLabel
  send Conjunction {..} input p = (Conjunction {cInputs = updated, ..}, Just nextPulse)
    where
      updated :: M.Map Label Pulse
      updated = M.insert input p cInputs

      nextPulse :: Pulse
      nextPulse
        | all (== High) . M.elems $ updated = Low
        | otherwise = High

instance Perform FlipFlop where
  label = fLabel
  send f _ High = (f, Nothing)
  send FlipFlop {..} _ Low = (FlipFlop {fState = togglePulse fState, ..}, Just fState)

instance Perform Component where
  label (CompB b) = label b
  label (CompC c) = label c
  label (CompF f) = label f
  send (CompB b) i p = B.first CompB $ send b i p
  send (CompC c) i p = B.first CompC $ send c i p
  send (CompF f) i p = B.first CompF $ send f i p

togglePulse :: Pulse -> Pulse
togglePulse Low = High
togglePulse High = Low

process :: Circuit -> (Circuit, (Int, Int))
process crct = go crct (0, 0) [(Low, "", "broadcaster")]
  where
    go :: Circuit -> (Int, Int) -> [(Pulse, Label, Label)] -> (Circuit, (Int, Int))
    go c stats [] = (c, stats)
    go c stats ((pls, src, trgt) : rest) = go nextC (countPulse pls stats) (rest ++ newPulses)
      where
        (nextC, newPulses) = sendPulse c pls src trgt

processN :: Int -> Circuit -> (Circuit, (Int, Int))
processN = go (0, 0)
  where
    go stats 0 c = (c, stats)
    go stats n c = go (addStats stats nextStats) (n - 1) nextC
      where
        (nextC, nextStats) = process c

multiplyStats :: (Int, Int) -> Int
multiplyStats (low, high) = low * high

addStats :: (Int, Int) -> (Int, Int) -> (Int, Int)
addStats (l, h) (l', h') = (l + l', h + h')

countPulse :: Pulse -> (Int, Int) -> (Int, Int)
countPulse Low (l, h) = (l + 1, h)
countPulse High (l, h) = (l, h + 1)

sendPulse :: Circuit -> Pulse -> Label -> Label -> (Circuit, [(Pulse, Label, Label)])
sendPulse Circuit {..} p src trgt
  | M.notMember trgt components = (Circuit {..}, [])
  | otherwise = toTargets $ send (components M.! trgt) src p
  where
    toTargets :: (Component, Maybe Pulse) -> (Circuit, [(Pulse, Label, Label)])
    toTargets (c, Nothing) = (update c, [])
    toTargets (c, Just pls) = (update c, map (pls,trgt,) $ connects M.! trgt)

    update :: Component -> Circuit
    update cmpnt = Circuit {components = M.insert trgt cmpnt components, ..}
