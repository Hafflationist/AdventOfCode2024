{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module T14.Tag14
    ( someFunc
    ) where

import Flow
-- import Control.Parallel.Strategies
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.List.Extra as LE
import qualified Data.List.Split as LS
import qualified Data.Ratio as R
import qualified Data.Text as T
import qualified Data.Maybe as M
import qualified Data.MemoTrie as MT
import Debug.Trace


data Robot = Robot { rPos :: (Int, Int), rVel :: (Int, Int) }
  deriving (Show, Eq)


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T14/input14.txt"
  let areaMetrics = (101, 103)
  -- input <- readFile "./src/T14/inputTest14.txt"
  -- let areaMetrics = (11, 7)
  let robots = parseInput input
  -- let result1 = robots |> moveSwarmNTimes areaMetrics 100 |> L.map rPos
  let result1 = robots |> moveSwarmNTimes areaMetrics 100 |> countQuadrant areaMetrics
  print result1
  let result2 = robots |> moveUntilTree areaMetrics
  -- putStrLn result2
  writeFile "./hugo.txt" result2
  putStrLn "Hallo Welt 14"


parseInput :: String -> [Robot]
parseInput txt =
  txt
  |> lines
  |> L.map parseSingleLine


parseSingleLine :: String -> Robot
parseSingleLine txt =
  let
    [pPair, vPair] = txt |> LS.splitOn " " |> L.map (LS.splitOn "=" .> L.tail .> L.head .> parsePair)
  in Robot { rPos = pPair, rVel = vPair }
  where
    parsePair :: String -> (Int, Int)
    parsePair txtPair =
      txtPair
      |> LS.splitOn ","
      |> L.map read
      |> (\ [a, b] -> (a, b))


moveUntilTree border swarm =
  -- repeat [1]
  [1 .. 10000]
  |> L.scanl (\ (_, a) idx -> (idx, moveSwarm border a)) (0, swarm)
  |> L.dropWhile (\ (idx, rs) -> not (tree rs))
  |> L.head
  |> (\ (idx, rs) -> show idx ++ ":\n" ++ drawSwarm border rs)
  -- |> L.map (\ (idx, rs) -> show idx ++ ":\n" ++ drawSwarm border rs)
  -- |> L.intercalate "\n\n-----------------------------------------------------------------------------------------------------------------\n\n"


tree swarm =
  let
    coords = L.map rPos swarm
  in
    coords |> L.any (hasLine coords)
  where
    hasLine coords (x, y) =
      [1 .. 10]
      |> L.map ((+ y) .> (x,))
      |> L.all (`elem` coords)


drawSwarm :: (Int, Int) -> [Robot] -> String
drawSwarm (xBorder, yBorder) swarm =
  let
    rCoords = L.map rPos swarm
  in
    [[(x, y) | x <- [0 .. (xBorder - 1)]] | y <- [0 .. (yBorder - 1)]]
    |> L.map (L.map (\ pos -> if pos `elem` rCoords then '#' else ' '))
    |> L.intercalate "\n"


moveSwarmNTimes :: Functor f => (Int, Int) -> Int -> f Robot -> f Robot
moveSwarmNTimes border n swarm =
  [1 .. n]
  |> L.foldl (\ a _ -> moveSwarm border a) swarm


moveSwarm :: Functor f => (Int, Int) -> f Robot -> f Robot
moveSwarm border = (moveBot border <$>)


moveBot :: (Int, Int) -> Robot -> Robot
moveBot (xBorder, yBorder) Robot { rPos = (x, y), rVel = rv@(vx, vy) } =
  let
   newX = (x + vx) `mod` xBorder
   newY = (y + vy) `mod` yBorder
  in Robot { rPos = (newX, newY), rVel = rv }


-- countQuadrant :: (Int, Int) -> [Robot] -> Int
countQuadrant (xBorder, yBorder) rs =
    let
      xMin1 = 0
      xMax1 = ((xBorder - 1) `div` 2) - 1
      xMin2 = ((xBorder - 1) `div` 2) + 1
      xMax2 = xBorder - 1
      yMin1 = 0
      yMax1 = ((yBorder - 1) `div` 2) - 1
      yMin2 = ((yBorder - 1) `div` 2) + 1
      yMax2 = yBorder - 1
      bots1 = countBotsInQuadrant (xMin1,  xMax1, yMin1, yMax1)
      bots2 = countBotsInQuadrant (xMin2,  xMax2, yMin1, yMax1)
      bots3 = countBotsInQuadrant (xMin1,  xMax1, yMin2, yMax2)
      bots4 = countBotsInQuadrant (xMin2,  xMax2, yMin2, yMax2)
      -- bots1 = countBotsInQuadrant (0,  49, 0, 50)
      -- bots2 = countBotsInQuadrant (51, 100, 0, 50)
      -- bots3 = countBotsInQuadrant (0,  49, 51, 102)
      -- bots4 = countBotsInQuadrant (51, 100, 51, 102)
      -- bots1 = countBotsInQuadrant (0,  4, 0, 2)
      -- bots2 = countBotsInQuadrant (6, 10, 0, 2)
      -- bots3 = countBotsInQuadrant (0,  4, 4, 6)
      -- bots4 = countBotsInQuadrant (6, 10, 4, 6)
    in bots1 * bots2 * bots3 * bots4
    -- in (bots1 , bots2 , bots3 , bots4)
  where
    inQuadrant :: (Int, Int, Int, Int) -> (Int, Int) -> Bool
    inQuadrant (xMin, xMax, yMin, yMax) (rx, ry) =
      xMin <= rx && rx <= xMax
      && yMin <= ry && ry <= yMax
    countBotsInQuadrant ::  (Int, Int, Int, Int) -> Int
    countBotsInQuadrant quadrant =
      rs
      |> L.map rPos
      |> L.filter (inQuadrant quadrant)
      |> L.length
