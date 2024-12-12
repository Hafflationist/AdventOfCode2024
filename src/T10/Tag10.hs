{-# LANGUAGE OverloadedStrings #-}

module T10.Tag10
    ( someFunc
    ) where

import Flow
import qualified Data.List as L


type Area = [[Int]]
type Position = (Int, Int)


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T10/input10.txt"
  -- input <- readFile "./src/T10/inputTest10.txt"
  let area = input |> parseInput
  let result1 = trailCount False area
  print result1
  let result2 = trailCount True area
  print result2
  putStrLn "Hallo Welt 10"


parseInput :: [Char] -> Area
parseInput = L.lines .> fmap (fmap ((:[]) .> read))


trailCount :: Bool -> Area -> Int
trailCount partTwo area =
  let
    reduction =
      if partTwo
      then id
      else L.map (L.head .> (:[])) .> L.nub
  in
    trailheads area
    |> L.map (:[])
    |> L.map (trailheadToPaths area)
    |> L.map reduction
    |> L.map L.length
    |> L.sum


trailheads :: Area -> [Position]
trailheads area =
  allIndices area
  |> L.filter (\ (r, c) -> (area !! r !! c) == 0)


trailheadToPaths :: Area -> [Position] -> [[Position]]
trailheadToPaths _ [] = undefined
trailheadToPaths area (lastPos@(r,c) : pp) 
  | (area !! r !! c) == 9 = [lastPos : pp]
  | otherwise =
    let
      nextPositionForPath = nextPosition area lastPos
      paths =
        (nextPositionForPath
        |> L.map (\ np -> np : lastPos : pp))
        >>= trailheadToPaths area
    in paths


nextPosition :: Area -> Position -> [Position]
nextPosition area (r, c) = 
  [
    if saveDoubleAccess area (r + 1) c == Just (heigth + 1) then [(r + 1, c)] else [],
    if saveDoubleAccess area (r - 1) c == Just (heigth + 1) then [(r - 1, c)] else [],
    if saveDoubleAccess area r (c + 1) == Just (heigth + 1) then [(r, c + 1)] else [],
    if saveDoubleAccess area r (c - 1) == Just (heigth + 1) then [(r, c - 1)] else []
  ]
  >>= id
  where heigth = area !! r !! c


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
