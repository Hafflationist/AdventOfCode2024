{-# LANGUAGE OverloadedStrings #-}

module T04.Tag04
    ( someFunc
    ) where

import Flow
import qualified Data.List as L
import qualified Data.Maybe as M


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T04/input04.txt"
  -- input <- readFile "./src/T04/inputTest04.txt"
  let parsedInput = input |> lines
  let result1 = countXmas parsedInput 
  print result1
  let result2 = countMasCross parsedInput
  print result2
  putStrLn "Hallo Welt 4"


countXmas :: [[Char]] -> Int
countXmas area =
  allIndices area
  |> L.filter (\(r, c) -> area !! r !! c == 'X')
  |> L.map (countXmasForSingleX area)
  |> L.sum


countMasCross :: [[Char]] -> Int
countMasCross area =
  allIndices area
  |> L.filter (\(r, c) -> area !! r !! c == 'A')
  |> L.map (countXmasForSingleA area)
  |> L.map (min 1)
  |> L.sum


allIndices :: [[a]] -> [(Int, Int)]
allIndices area =
  let
    rowsN = L.length area
    rowsIdx = [0..(rowsN - 1)]
    columnsN = area |> L.head |> L.length
    columnsIdx = [0..(columnsN - 1)]
  in [(r, c) | r <- rowsIdx, c <- columnsIdx]


countXmasForSingleA :: [[Char]] -> (Int, Int) -> Int
countXmasForSingleA area coordOfA =
  let
    directions = [
        [(-1,  1), ( 1,  1), ( 1, -1), (-1, -1)]
      ] :: [[(Int, Int)]]
  in 
    directions
    |> L.filter (\x -> checkForStringInOneDirection area "MMSS" coordOfA x 
                    || checkForStringInOneDirection area "SMMS" coordOfA x
                    || checkForStringInOneDirection area "SSMM" coordOfA x
                    || checkForStringInOneDirection area "MSSM" coordOfA x
                    )
    |> L.length


countXmasForSingleX :: [[Char]] -> (Int, Int) -> Int
countXmasForSingleX area coordOfX =
  let
    directions = [
        [(-1, -1), (-2, -2), (-3, -3)],
        [(-1,  0), (-2,  0), (-3,  0)],
        [(-1,  1), (-2,  2), (-3,  3)],
        [( 0,  1), ( 0,  2), ( 0,  3)],
        [( 1,  1), ( 2,  2), ( 3,  3)],
        [( 1,  0), ( 2,  0), ( 3,  0)],
        [( 1, -1), ( 2, -2), ( 3, -3)],
        [( 0, -1), ( 0, -2), ( 0, -3)]
      ] :: [[(Int, Int)]]
  in 
    directions
    |> L.filter (checkForStringInOneDirection area "MAS" coordOfX)
    |> L.length


checkForStringInOneDirection :: [[Char]] -> String -> (Int, Int) -> [(Int, Int)] -> Bool
checkForStringInOneDirection area text coordOfX direction =
  let
    (xFst, xSnd) = coordOfX
    txtInDirection =
      direction 
      |> L.map (\dir -> saveDoubleAccess area (xFst + fst dir) (xSnd + snd dir))
      |> L.map (M.fromMaybe '_')
  in txtInDirection == text


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
