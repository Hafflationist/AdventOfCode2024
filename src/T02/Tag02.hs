{-# LANGUAGE OverloadedStrings #-}

module T02.Tag02
    ( someFunc
    ) where

import Flow
import qualified Data.List as L
import qualified Data.Text as T


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T02/input02.txt"
  let parsedInput = input |> T.pack |> parseInput
  let result1 = countSafeLines parsedInput
  print result1
  let result2 = countSafeLinesWithException parsedInput
  print result2
  putStrLn "Hallo Welt 2"


countSafeLines :: [[Int]] -> Int
countSafeLines = L.length . L.filter isSafe


parseInput :: T.Text -> [[Int]]
parseInput = L.map parseSingleLine . T.lines


isSafe :: [Int] -> Bool
isSafe xs =
  let
    xs1 = cycle xs
    xs2 = xs |> cycle |> tail
    listOfDiffs = 
      L.zip xs1 xs2 
      |> L.take ((L.length xs) - 1)
      |> L.map (uncurry (-))
    noEq = 0 `notElem` listOfDiffs
    onlyIncrease =
      listOfDiffs
      |> L.all (\x -> -3 <= x && x <= -1)
    onlyDecrease =
      listOfDiffs
      |> L.all (\x -> 1 <= x && x <= 3)
      
  in noEq && (onlyDecrease || onlyIncrease)

 
parseSingleLine :: T.Text -> [Int]
parseSingleLine line =
  line
  |> T.splitOn " "
  |> L.map T.unpack
  |> L.map read


countSafeLinesWithException :: [[Int]] -> Int
countSafeLinesWithException myLines =
  myLines
  |> L.map blowLine
  |> L.filter (L.any isSafe)
  |> L.length


blowLine :: [Int] -> [[Int]]
blowLine xs =
  let
    n = L.length xs
    reducedLists = 
      [0..(n-1)]
      |> L.map (\n' -> removeAt n' xs)
  in xs : reducedLists


removeAt :: Int -> [a] -> [a]
removeAt n xs =
  let (a, _:b) = L.splitAt n xs
  in a ++ b
