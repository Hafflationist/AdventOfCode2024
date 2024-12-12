{-# LANGUAGE OverloadedStrings #-}

module T01.Tag01
    ( someFunc
    ) where

import Flow
import qualified Data.Bifunctor as B
import qualified Data.List as L
import qualified Data.Text as T


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T01/input01.txt"
  let parsedInput = input |> T.pack |> parseInput
  let result1 = calculateDistance parsedInput
  print result1
  let result2 = calculateSimilarity parsedInput
  print result2
  putStrLn "Hallo Welt 2"


parseInput :: T.Text -> ([Int], [Int])
parseInput inputText =
  inputText
  |> T.lines
  |> L.map parseSingleLine
  |> transposeMy


calculateDistance :: ([Int], [Int]) -> Int
calculateDistance pairOfLists =
  pairOfLists
    |> B.first L.sort
    |> B.second L.sort
    |> uncurry L.zip
    |> L.map (uncurry (-))
    |> L.map abs
    |> L.sum  
 
 
parseSingleLine :: T.Text -> (Int, Int)
parseSingleLine line =
  let
    [a, b] = T.splitOn "   " line
    x = read (T.unpack a) :: Int
    y = read (T.unpack b) :: Int
  in (x, y)


transposeMy :: [(Int, Int)] -> ([Int], [Int])
transposeMy listOfPairs =
  let
    a = L.map fst listOfPairs
    b = L.map snd listOfPairs
  in (a, b)


calculateSimilarity :: ([Int], [Int]) -> Int
calculateSimilarity (xs, ys) =
  xs
  |> L.map (productWithCount ys)
  |> L.sum
 

productWithCount :: [Int] -> Int -> Int
productWithCount ys x = (countOccurences ys x) * x

countOccurences :: [Int] -> Int -> Int
countOccurences ys x =
  ys
  |> L.filter (x ==)
  |> L.length
