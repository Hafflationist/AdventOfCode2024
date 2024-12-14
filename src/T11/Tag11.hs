{-# LANGUAGE OverloadedStrings #-}

module T11.Tag11
    ( someFunc
    ) where

import Flow
-- import Control.Parallel.Strategies
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.MemoTrie as MT
import Debug.Trace


data CountedStone =
  CountedStone { stoneCount :: Integer, stoneId :: Integer }
  deriving Show


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T11/input11.txt"
  -- input <- readFile "./src/T11/inputTest11.txt"
  let lineOfStones = input |> parseInput
  let result1 = lineOfStones |> evolution 25 |> L.map stoneCount |> L.sum
  print result1
  let result2 = lineOfStones |> evolution 75 |> L.map stoneCount |> L.sum
  print result2
  putStrLn "Hallo Welt 11"


parseInput :: [Char] -> [CountedStone]
parseInput =
  words
  .> L.map read
  .> L.map (\ sid -> CountedStone { stoneCount = 1, stoneId = sid})


evolution :: Integer -> [CountedStone] -> [CountedStone]
evolution steps lineOfStones =
  [1 .. steps]
  |> L.foldl (\ a b -> traceWith (\aNew -> "Schritt: " ++ show b ++ "  ("++ (aNew |> L.length |> show) ++ ")") (evolutionOneStep a)) lineOfStones


evolutionOneStep :: [CountedStone] -> [CountedStone]
evolutionOneStep lineOfStones = lineOfStones >>= evolveOneStoneMapped |> consolidate -- `using` parListChunk 100000000 rdeepseq


consolidate :: [CountedStone] -> [CountedStone]
consolidate css =
  css
  |> L.map stoneId
  |> L.nub
  |> L.map consolidateForSid
  where
    consolidateForSid sid =
      let
        newStoneCount =
          css
          |> L.filter (stoneId .> (== sid))
          |> L.map stoneCount
          |> L.sum
      in CountedStone { stoneCount = newStoneCount, stoneId = sid }


evolveOneStoneMapped :: CountedStone -> [CountedStone]
evolveOneStoneMapped (CountedStone { stoneCount = oldStoneCount, stoneId = oldStoneId }) =
  evolveOneStone oldStoneId
  |> L.map (\ sid -> CountedStone { stoneCount = oldStoneCount, stoneId = sid })


evolveOneStoneMemo :: Integer -> [Integer]
evolveOneStoneMemo = MT.memo evolveOneStone


evolveOneStone :: Integer -> [Integer]
evolveOneStone stone
  | stone == 0       = [1]
  | evenDigits stone = splitDigits stone
  | otherwise        = [stone * 2024]


evenDigits :: Integer -> Bool
evenDigits = show .> L.length .> (`mod` 2) .> (== 0)


splitDigits :: Integer -> [Integer]
splitDigits stone =
  let
    stoneStr = show stone
    numberOfDigits = L.length stoneStr
    (a, b) = stoneStr |> T.pack |> T.splitAt (numberOfDigits `div` 2)
  in [a, b] |> L.map (T.unpack .> read)
