{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( part0,
    part1,
    part2,
  )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Prelude hiding (id)

part0 :: IO ()
part0 = do
  parseTest parseBlue "3 blue,"
  parseTest parseGreen "5 green"
  parseTest parseRed "8 red"
  parseTest parseCubes "3 blue, 5 green, 8 red"
  printParse parseDraw "3 blue, 5 green, 8 red"
  printParse parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

  s <- TIO.readFile "debug.txt"
  TIO.putStrLn s
  printParse parseGames s

  putStr "\nTesting part 1 (expecting 8): "
  either error (print . sumValidGames 14 13 12) $ parseOnly (parseGames <* endOfInput) s
  putStr "\n"

  putStr "Testing part 2 (expecting 2286): "
  either error (print . sum . map power) $ parseOnly (parseGames <* endOfInput) s
  putStr "\n"

part1 :: IO ()
part1 = do
  s <- TIO.readFile "input.txt"
  putStr "Part 1: "
  either error (print . sumValidGames 14 13 12) $ parseOnly (parseGames <* endOfInput) s
  putStr "\n"

part2 :: IO ()
part2 = do
  s <- TIO.readFile "input.txt"
  putStr "Part 2: "
  either error (print . sum . map power) $ parseOnly (parseGames <* endOfInput) s
  putStr "\n"

data Game = Game
  { gameId :: Int,
    draws :: [Draw]
  }
  deriving (Show)

newtype Draw = Draw [Cubes] deriving (Show)

data Cubes = Blue Int | Green Int | Red Int deriving (Show)

blue :: Draw -> Int
blue (Draw ((Blue i) : _)) = i
blue (Draw (_ : cc)) = blue (Draw cc)
blue (Draw []) = 0

green :: Draw -> Int
green (Draw ((Green i) : _)) = i
green (Draw (_ : cc)) = green (Draw cc)
green (Draw []) = 0

red :: Draw -> Int
red (Draw ((Red i) : _)) = i
red (Draw (_ : cc)) = red (Draw cc)
red (Draw []) = 0

ensureParsed :: Result a -> Result a
ensureParsed (Partial p) = ensureParsed $ feed (Partial p) ""
ensureParsed r = r

printParse :: (Show a) => Parser a -> T.Text -> IO ()
printParse p i = print . ensureParsed $ parse p i

parseGames :: Parser [Game]
parseGames = many1' $ parseGame <* endOfLine

parseGame :: Parser Game
parseGame = do
  "Game "
  gameId <- decimal
  ": "
  draws <- sepBy1' parseDraw "; "
  return
    Game
      { gameId,
        draws
      }

parseDraw :: Parser Draw
parseDraw = Draw <$> sepBy1' parseCubes ", "

parseCubes :: Parser Cubes
parseCubes = skipSpace *> (parseBlue <|> parseGreen <|> parseRed)

parseBlue :: Parser Cubes
parseBlue = (Blue <$> decimal) <* skipSpace <* "blue"

parseGreen :: Parser Cubes
parseGreen = (Green <$> decimal) <* skipSpace <* "green"

parseRed :: Parser Cubes
parseRed = (Red <$> decimal) <* skipSpace <* "red"

checkLimitDraw :: (Draw -> Int) -> Int -> Draw -> Bool
checkLimitDraw check limit draw = check draw <= limit

checkGame :: (Draw -> Bool) -> Game -> Bool
checkGame checker = all checker . draws

-- returns the sum of ids of games within limits
sumValidGames :: Int -> Int -> Int -> [Game] -> Int
sumValidGames b g r =
  sum
    . map gameId
    . filter (checkGame (checkLimitDraw blue b))
    . filter (checkGame (checkLimitDraw green g))
    . filter (checkGame (checkLimitDraw red r))

minCube :: (Draw -> Int) -> Game -> Int
minCube view = foldl' max 0 . map view . draws

power :: Game -> Int
power g = minCube blue g * minCube green g * minCube red g
