module Lib
    ( part0
    , part1
    , part2
    ) where

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

part0 :: IO ()
part0 = do
   stars <- getInput "debug.txt"
   TIO.putStr . T.unlines $ stars
   putStr "[part0-1] sum of digits: "
   let sumDebug1 = sumDigits . filterDigits $ stars
   putStrLn . show $ assert sumDebug1 142

   stars2 <- getInput "debug2.txt"
   putStr "[part0-2] sum of digits: "
   let sumDebug2 = sumDigits . filterDigitsAdvanced $ stars2
   putStrLn . show $ assert sumDebug2 281

part1 :: IO ()
part1 = do
   stars <- getInput "input.txt"
   putStr "[part1] sum of digits: "
   let sum_ = sumDigits . filterDigits $ stars
   putStrLn . show $ sum_

part2 :: IO ()
part2 = do
   stars <- getInput "input.txt"
   putStr "[part2] sum of digits: "
   let sum_ = sumDigits . filterDigitsAdvanced $ stars
   putStrLn . show $ sum_

assert :: (Eq a , Show a) => a -> a -> a
assert got want | got == want = got
                | True = error $ "Equality failed, wanted: " ++ (show want) ++ " got: " ++ (show got)

type Star = T.Text
type Digits = T.Text

filterDigits :: [Star] -> [Digits]
filterDigits = map $ T.filter isDigit

-- part B (very inefficient)
filterDigitsAdvanced :: [Star] -> [Digits]
filterDigitsAdvanced = map filterDigitsAdvancedSingle

filterDigitsAdvancedSingle :: Star -> Digits
filterDigitsAdvancedSingle = T.filter isDigit . T.intercalate T.empty . map reduceDigits . iterateChars

reduceDigits :: T.Text -> T.Text
reduceDigits input = join $ checkEach input
    where
        checkEach :: T.Text -> [T.Text]
        checkEach input = map (\f -> f input)
             [ reduceDigit (T.pack "one")    '1'
             , reduceDigit (T.pack "two" )   '2'
             , reduceDigit (T.pack "three" ) '3'
             , reduceDigit (T.pack "four" )  '4'
             , reduceDigit (T.pack "five" )  '5'
             , reduceDigit (T.pack "six" )   '6'
             , reduceDigit (T.pack "seven" ) '7'
             , reduceDigit (T.pack "eight" ) '8'
             , reduceDigit (T.pack "nine" )  '9'
             , reduceDigit (T.pack "zero" )  '0'
             , reduceActualDigit
             ]
        join :: [T.Text] -> T.Text
        join = T.intercalate T.empty

reduceActualDigit :: T.Text -> T.Text
reduceActualDigit t = go (T.head t)
    where
        go :: Char -> T.Text
        go c | isDigit c = T.singleton c
        go _ = T.empty

reduceDigit :: T.Text -> Char -> T.Text -> T.Text
reduceDigit prefix c input | T.isPrefixOf prefix input = T.singleton c
reduceDigit _ _ _ = T.empty

iterateChars :: T.Text -> [T.Text]
iterateChars t = case T.uncons t of
    Nothing -> []
    Just (_, tail_) -> t : iterateChars tail_

sumDigits :: [Digits] -> Int
sumDigits = sum . (map calibrationValue)

calibrationValue :: Digits -> Int
calibrationValue d = (digitToInt (T.head d)) * 10 + (digitToInt (T.last d))

getInput :: FilePath -> IO [Star]
getInput path = do
  content <- TIO.readFile path
  let split = T.lines content
  let nonEmpty = filter (not . T.null) split
  return $ nonEmpty
