{-# LANGUAGE OverloadedStrings #-}

module T12.Tag12
    ( someFunc
    ) where

import Flow
-- import Control.Parallel.Strategies
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.List.Extra as LE
import qualified Data.Text as T
import qualified Data.MemoTrie as MT
import Debug.Trace


type Plant = Char
type Area = [[Plant]]
type Position = (Int, Int)
type Region = [Position]


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T12/input12.txt"
  -- input <- readFile "./src/T12/inputTest12.txt"
  let area = input |> parseInput
  let result1 = area |> clustering |> L.map (costOfFence area) |> L.sum
  print result1
  let result2 = area |> clustering |> L.map (costOfFenceWithDiscount area) |> L.sum
  print result2
  putStrLn "Hallo Welt 12"


parseInput :: [Char] -> Area
parseInput = L.lines


clustering :: Area -> [Region]
clustering area =
  area
  |> allIndices
  |> L.foldl (addPosition area) []


costOfFenceWithDiscount :: Area -> Region -> Int
costOfFenceWithDiscount area r@((a, b):_) =
  let
    rPlant = area !! a !! b
    rArea = L.length r
    rPerimeter = perimeterWithDiscount area r
  -- in (rPlant, rArea , rPerimeter)
  in rArea * rPerimeter


perimeterWithDiscount :: Area -> Region -> Int
perimeterWithDiscount area r =
  r
  |> L.map (countNonTouchingSidesWithDiscount area r)
  |> L.sum


countNonTouchingSidesWithDiscount :: Area -> Region -> Position -> Int
countNonTouchingSidesWithDiscount area region (r, c) =
  [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
  |> L.filter (`notElem` region)
  |> L.filter (noNeighbourOriented area (r, c))
  |> L.length


noNeighbourOriented :: Area -> Position -> Position -> Bool
noNeighbourOriented area (rRegion, cRegion) (rOutside, cOutside) =
  let
    (rNeighbourDelta, cNeighbourDelta) = (cRegion - cOutside, rRegion - rOutside)
    (rRegion', cRegion') = (rRegion + rNeighbourDelta, cRegion + cNeighbourDelta)
    (rOutside', cOutside') = (rOutside + rNeighbourDelta, cOutside + cNeighbourDelta)
    plantOfRegion = area !! rRegion !! cRegion
    plantOfRegion' = saveDoubleAccess area rRegion' cRegion'
    plantOfOutside' = saveDoubleAccess area rOutside' cOutside'
    samePlant = Just plantOfRegion == plantOfRegion'
    isReallyOutside' = plantOfOutside' /= Just plantOfRegion
    -- sameBorder = 
  in not (samePlant && isReallyOutside')


costOfFence :: Area -> Region -> Int
costOfFence area r@((a, b):_) =
  let
    rPlant = area !! a !! b
    rArea = L.length r
    rPerimeter = perimeter r
  -- in (rPlant, rArea , rPerimeter)
  in rArea * rPerimeter


perimeter :: Region -> Int
perimeter r =
  r
  |> L.map (countNonTouchingSides r)
  |> L.sum


countNonTouchingSides :: Region -> Position -> Int
countNonTouchingSides region (r, c) =
  [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
  |> L.filter (`notElem` region)
  |> L.length


addPosition :: Area -> [Region] -> Position -> [Region]
addPosition area regions pos =
  regions |> L.filter (inRegion area pos) |> addOrMakeNew
  where
    addOrMakeNew []  = [pos] : regions
    addOrMakeNew rs = addToRegion regions rs pos


addToRegion :: [Region] -> [Region] -> Position -> [Region]
addToRegion rs oldRs pos =
  let
    mergedOldRs = CM.join oldRs
    newR = pos : mergedOldRs
  in
    rs
    |> L.map (\ r -> if r `elem` oldRs then newR else r)
    |> L.nub


inRegion :: Area -> Position -> Region -> Bool
inRegion area pos = L.any (touching area pos)


touching :: Area -> Position -> Position -> Bool
touching area (r1, c1) (r2, c2) =
  let
    plant1 = area !! r1 !! c1
    plant2 = area !! r2 !! c2
    horz = r1 == r2 && abs(c1 - c2) == 1
    vert = c1 == c2 && abs(r1 - r2) == 1
  in plant1 == plant2 && (horz || vert)


allIndices :: [[a]] -> [Position]
allIndices area =
  let
    rowsN = L.length area
    rowsIdx = [0..(rowsN - 1)]
    columnsN = area |> L.head |> L.length
    columnsIdx = [0..(columnsN - 1)]
  in [(r, c) | r <- rowsIdx, c <- columnsIdx]


saveDoubleAccess :: [[a]] -> Int -> Int -> Maybe a
saveDoubleAccess list idx1 idx2 =
  (list !? idx1) >>= (!? idx2)


(!?) :: [a] -> Int -> Maybe a

{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
