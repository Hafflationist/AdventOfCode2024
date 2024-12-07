{-# LANGUAGE OverloadedStrings #-}

module T06.Tag06
    ( someFunc
    ) where

import Flow
import Text.Read
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Maybe as M
import qualified Data.Text as T


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T06/input06.txt"
  -- input <- readFile "./src/T06/inputTest06.txt"
  let area = input |> lines
  let result1 = strideMaxxing area
  print result1
  let withoutLoop = [1, 2, 3, 4, 5, 6, 7, 8, 9]
  let withLoop = cycle [1, 2, 3, 4, 5, 6, 7, 8, 9]
  let withLoop2 = 0 : cycle [1, 2, 3, 4, 5, 6, 7, 8, 9]

  let result2 = loops area
  print result2
  putStrLn "Hallo Welt 6"


loops :: [[Char]] -> Int
loops area =
  obstructionize area
  |> L.map path
  |> L.filter containsLoop
  |> L.length


obstructionize :: [[Char]] -> [[[Char]]]
obstructionize area =
  let
    -- obsCoords = allIndices area
    obsCoords =
      path area
      |> L.map (\ (x, y, _) -> (x, y))
      |> L.nub
  in L.map (obstructionizeForOne area) obsCoords


obstructionizeForOne :: [[Char]] -> (Int, Int) -> [[Char]]
obstructionizeForOne area (r, c) =
  let
    row = area !! r
    newRow = replace c '#' row
  in replace r newRow area
  where
    replace pos newVal list = L.take pos list ++ newVal : L.drop (pos+1) list


containsLoop :: Eq a => [a] -> Bool
containsLoop positions =
  cl positions []
  where
    cl [] xsAlreadyKnown = False
    cl (x:xs) xsAlreadyKnown =
      x `elem` xsAlreadyKnown || cl xs (x:xsAlreadyKnown)


strideMaxxing :: [[Char]] -> Int
strideMaxxing area =
  path area
  |> L.map (\ (x, y, _) -> (x, y))
  |> L.nub
  |> L.length


path :: [[Char]] -> [(Int, Int, (Int, Int))]
path area =
  findGuard area
  |> L.iterate (>>= moveOrTurn area) 
  |> L.takeWhile M.isJust
  |> M.catMaybes
  |> L.take 17000


allIndices :: [[a]] -> [(Int, Int)]
allIndices area =
  let
    rowsN = L.length area
    rowsIdx = [0..(rowsN - 1)]
    columnsN = area |> L.head |> L.length
    columnsIdx = [0..(columnsN - 1)]
  in [(r, c) | r <- rowsIdx, c <- columnsIdx]


findGuard :: [[Char]] -> Maybe (Int, Int, (Int, Int))
findGuard area =
  allIndices area
  |> L.find (\(r, c) -> area !! r !! c == '^')
  |> fmap (\(r, c) -> (r, c, (-1, 0)))


moveOrTurn :: [[Char]] -> (Int, Int, (Int, Int)) -> Maybe (Int, Int, (Int, Int))
moveOrTurn area guard@(guardX, guardY, (directionX, directionY))
  | nextValue == Just '#' = Just (turn guard)
  | M.isNothing nextValue = Nothing
  | otherwise = Just (move guard)
  where 
    nextPos@(nextX, nextY) = (guardX + directionX, guardY + directionY)
    nextValue = saveDoubleAccess area nextX nextY


turn :: (Int, Int, (Int, Int)) -> (Int, Int, (Int, Int))
turn (gx, gy, (1, 0)) = (gx, gy, (0, -1))
turn (gx, gy, (0, -1)) = (gx, gy, (-1, 0))
turn (gx, gy, (-1, 0)) = (gx, gy, (0, 1))
turn (gx, gy, (0, 1)) = (gx, gy, (1, 0))
turn _ = undefined


move :: (Int, Int, (Int, Int)) -> (Int, Int, (Int, Int))
move (guardX, guardY, (directionX, directionY)) =
  (guardX + directionX, guardY + directionY, (directionX, directionY))



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
