{-# LANGUAGE OverloadedStrings #-}

module T05.Tag05
    ( someFunc
    ) where

import Flow
import Text.Read
import Data.List
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Maybe as M
import qualified Data.Text as T


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T05/input05.txt"
  -- input <- readFile "./src/T05/inputTest05.txt"
  let (rules, updateList) = input |> T.pack |> parseInput
  let result1 = sumOfValidUpdateMiddle rules updateList
  print result1
  let result2 = sumOfInvalidCorrectedUpdateMiddle rules updateList
  print result2
  putStrLn "Hallo Welt 5"


parseInput :: T.Text -> ([(Int, Int)], [[Int]])
parseInput txt =
  let
    [rulesRaw, updateListRaw] = txt |> T.lines |> LS.splitOn [""]
    rules = parseRules rulesRaw
    updateList = parseUpdateList updateListRaw
  in (rules, updateList)


parseRules :: [T.Text] -> [(Int, Int)]
parseRules txtLines =
  txtLines
  |> L.map parseSingleRule
  where
    parseSingleRule txt =
      let [before, after] = txt |> T.splitOn "|" |> L.map (T.unpack .> read)
      in (before, after)


parseUpdateList :: [T.Text] -> [[Int]]
parseUpdateList = L.map (T.splitOn "," .> L.map (T.unpack .> read))


sumOfValidUpdateMiddle :: [(Int, Int)] -> [[Int]] -> Int
sumOfValidUpdateMiddle rules updateList =
  updateList
  |> L.filter (checkUpdate rules)
  |> L.map middle
  |> L.sum


sumOfInvalidCorrectedUpdateMiddle :: [(Int, Int)] -> [[Int]] -> Int
sumOfInvalidCorrectedUpdateMiddle rules updateList =
  updateList
  |> L.filter (checkUpdate rules .> not)
  |> L.map (sortUpdate rules)
  |> L.map middle
  |> L.sum


sortUpdate :: [(Int, Int)] -> [Int] -> [Int]
sortUpdate rules = L.sortBy (compareWithRule rules)


compareWithRule :: [(Int, Int)] -> Int -> Int -> Ordering
compareWithRule rules a b
  | (a, b) `elem` rules = LT
  | (b, a) `elem` rules = GT
  | otherwise = EQ


middle :: [a] -> a
middle list = L.head (m list)
  where
      m l@(_:_:_:_) = l |> L.init |> L.tail |> m
      m [_, _] = undefined
      m l = l


checkUpdate :: [(Int, Int)] -> [Int] -> Bool
checkUpdate rules update = L.all (`checkUpdateSingleRule` update) rules


checkUpdateSingleRule :: (Int, Int) -> [Int] -> Bool
checkUpdateSingleRule (ruleBefore, ruleAfter) update =
  let
    beforeIdx = L.elemIndex ruleBefore update |> M.fromMaybe minBound
    afterIdx = L.elemIndex ruleAfter update |> M.fromMaybe maxBound
  in beforeIdx < afterIdx
