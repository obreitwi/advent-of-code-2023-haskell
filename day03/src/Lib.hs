{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( part0,
    part1,
  )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude

part0 :: IO ()
part0 = do
  s <- TIO.readFile "debug.txt"
  -- print . ensureParsed $ parse parseSchematic s
  TIO.putStr "\nTesting part 1 (expecting 4361): "
  either error (part1inner . snd) $ parseOnly (parseSchematic <* endOfInput) s

  return ()

part1 :: IO ()
part1 = do
  s <- TIO.readFile "input.txt"
  -- print . ensureParsed $ parse parseSchematic s
  TIO.putStr "\nPart 1: "
  either error (part1inner . snd) $ parseOnly (parseSchematic <* endOfInput) s
  return ()

part1inner :: SchematicScanner -> IO ()
part1inner s = do
  TIO.putStrLn "Symbols:"
  _ <- mapM print . reverse . symbols $ s
  TIO.putStrLn "\nNumbers:"
  _ <- mapM print . reverse . numbers $ s
  TIO.putStrLn ""
  let numAdjacent = filterSchematic s
  TIO.putStrLn "\nAdjacent numbers:"
  _ <- mapM print . reverse $ numAdjacent
  TIO.putStrLn ""
  print . sum . map value $ numAdjacent

data SchematicNumber = NumberS
  { value :: Int,
    numRow :: Int,
    numColStart :: Int,
    numColStop :: Int
  }
  deriving (Show)

data SchematicSymbol = SymbolS
  { symRow :: Int,
    symCol :: Int,
    symbol :: Char
  }
  deriving (Show)

data SchematicScanner = SchematicS
  { scanCurrentCol :: Int,
    scanCurrentRow :: Int,
    scanCurrentNumber :: Int,
    symbols :: [SchematicSymbol],
    numbers :: [SchematicNumber]
  }
  deriving (Show)

emptySchematic :: SchematicScanner
emptySchematic =
  SchematicS
    { scanCurrentCol = 0,
      scanCurrentRow = 0,
      scanCurrentNumber = 0,
      symbols = [],
      numbers = []
    }

parseSchematic :: Parser (T.Text, SchematicScanner)
parseSchematic = runScanner emptySchematic scanSchematic

scanSchematic :: SchematicScanner -> Char -> Maybe SchematicScanner
scanSchematic prev@SchematicS {..} c = case c of
  '\n' -> Just ((advanceRow . noteNumber) prev)
  '.' -> Just ((advanceCol . noteNumber) prev)
  _
    | isDigit c ->
        Just
          ( advanceCol
              SchematicS
                { scanCurrentNumber = scanCurrentNumber * 10 + digitToInt c,
                  ..
                }
          )
  _ -> Just ((advanceCol . noteNumber . noteSymbol c) prev)

advanceCol :: SchematicScanner -> SchematicScanner
advanceCol SchematicS {..} =
  SchematicS
    { scanCurrentCol = scanCurrentCol + 1,
      ..
    }

advanceRow :: SchematicScanner -> SchematicScanner
advanceRow SchematicS {..} =
  SchematicS
    { scanCurrentCol = 0,
      scanCurrentRow = scanCurrentRow + 1,
      ..
    }

noteNumber :: SchematicScanner -> SchematicScanner
noteNumber prev@SchematicS {..} = case scanCurrentNumber of
  0 -> prev
  _ ->
    SchematicS
      { numbers =
          NumberS
            { numRow = scanCurrentRow,
              numColStart = scanCurrentCol - amountOfDigits scanCurrentNumber,
              numColStop = scanCurrentCol - 1,
              value = scanCurrentNumber
            }
            : numbers,
        scanCurrentNumber = 0,
        ..
      }

noteSymbol :: Char -> SchematicScanner -> SchematicScanner
noteSymbol c SchematicS {..} =
  SchematicS
    { symbols =
        SymbolS
          { symCol = scanCurrentCol,
            symRow = scanCurrentRow,
            symbol = c
          }
          : symbols,
      ..
    }

-- only works for integers
amountOfDigits :: Int -> Int
amountOfDigits = Prelude.length . show

ensureParsed :: Result a -> Result a
ensureParsed (Partial p) = ensureParsed $ feed (Partial p) ""
ensureParsed r = r

filterSchematic :: SchematicScanner -> [SchematicNumber]
filterSchematic SchematicS {..} = filterAdjacentNumbers symbols numbers

filterAdjacentNumbers :: [SchematicSymbol] -> [SchematicNumber] -> [SchematicNumber]
filterAdjacentNumbers symbols = filter anyAdjacent
  where
    symbolsAdjacent :: [SchematicNumber -> Bool]
    symbolsAdjacent = map isAdjacent symbols

    anyAdjacent :: SchematicNumber -> Bool
    anyAdjacent num = any (\f -> f num) symbolsAdjacent

isAdjacent :: SchematicSymbol -> SchematicNumber -> Bool
isAdjacent SymbolS {..} NumberS {..} = rowAdjacent && colAdjacent
  where
    rowAdjacent = symRow - 1 <= numRow && numRow <= symRow + 1
    -- colAdjacent = symCol - 1 <= numColStop && numColStart <= symCol + 1
    colAdjacent = not $ numColStop < symCol - 1 || numColStart > symCol + 1
