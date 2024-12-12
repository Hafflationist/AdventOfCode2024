{-# LANGUAGE OverloadedStrings #-}

module T08.Tag08
    ( someFunc
    ) where

import Flow
import Text.Read
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Maybe as M
import qualified Data.Text as T


type Area = [[Char]]
type Antenna = Char


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T08/input08.txt"
  -- input <- readFile "./src/T08/inputTest08.txt"
  let area = input |> lines
  let result1 = area |> antinodes False |> L.length
  print result1
  let result2 = area |> antinodes True |> L.length
  print result2
  putStrLn "Hallo Welt 8"


antinodes :: Bool -> Area -> [(Int, Int)]
antinodes partTwo area =
  allAntennaTypes area
  >>= antinodePerAntenna partTwo area
  |> L.nub


antinodePerAntenna :: Bool -> Area -> Antenna -> [(Int, Int)]
antinodePerAntenna partTwo area antenna =
  let 
    coords = antennaToCoords area antenna
    idxs = allIndices area
  in 
    [(a, b) | a <- coords, b <- coords, a /= b]
    >>= uncurry (antinodePerAntennaPair partTwo)
    |> L.filter (`elem` idxs)


antinodePerAntennaPair :: Bool -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodePerAntennaPair False (row1, column1) (row2, column2) =
  let
    (rowDelta, columnDelta) = (row2 - row1, column2 - column1)
  in [
    (row2 + rowDelta, column2 + columnDelta),
    (row1 - rowDelta, column1 - columnDelta)
  ]
antinodePerAntennaPair True (row1, column1) (row2, column2) =
  let
    (rowDelta, columnDelta) = (row2 - row1, column2 - column1)
  in 
    [-100..100]
    |> L.map (\ x -> (row1 + x * rowDelta, column1 + x * columnDelta))


allAntennaTypes :: Area -> [Antenna]
allAntennaTypes area =
  area >>= id
  |> L.filter (/= '.')
  |> L.nub


antennaToCoords :: Area -> Antenna -> [(Int, Int)]
antennaToCoords area antenna =
  allIndices area
  |> L.filter (\ (r, c) -> area !! r !! c == antenna)


allIndices :: [[a]] -> [(Int, Int)]
allIndices area =
  let
    rowsN = L.length area
    rowsIdx = [0..(rowsN - 1)]
    columnsN = area |> L.head |> L.length
    columnsIdx = [0..(columnsN - 1)]
  in [(r, c) | r <- rowsIdx, c <- columnsIdx]
