{-# LANGUAGE OverloadedStrings #-}

module T03.Tag03
    ( someFunc
    ) where

import Flow
import Text.Read
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T03/input03.txt"
  let parsedInput = input |> T.pack
  let result1 = sumOfMulExpressions parsedInput 
  print result1
  let result2 = parsedInput |> removeDisabledParts |> sumOfMulExpressions
  print result2
  putStrLn "Hallo Welt 3"


sumOfMulExpressions :: T.Text -> Int
sumOfMulExpressions txt =
  txt
  |> T.splitOn "mul("
  |> L.map multiplicate
  |> L.map (M.fromMaybe 0)
  |> L.sum


multiplicate :: T.Text -> Maybe Int
multiplicate txt =
  let
    txtWithoutClosingBracket = T.splitOn ")" txt |> L.head
    rawFactors = T.splitOn "," txtWithoutClosingBracket
  in 
    -- Hier das letzte Element muss nicht ')' sein. Erst darauf splitten!
    if ')' `notElem` T.unpack txt || L.length rawFactors /= 2 then Nothing
    else let
        [rawFactor1, rawFactor2] = rawFactors
        factor1 = readMaybe . T.unpack $ rawFactor1 :: Maybe Int
        factor2 = readMaybe . T.unpack $ rawFactor2 :: Maybe Int
      in maybeDoubleMap factor1 factor2 
         |> fmap (uncurry (*))


maybeDoubleMap :: Maybe a -> Maybe a -> Maybe (a, a)
maybeDoubleMap (Just x) (Just y) = Just (x, y)
maybeDoubleMap _ _ = Nothing


removeDisabledParts :: T.Text -> T.Text
removeDisabledParts txt =
  ("do()" ++ T.unpack txt ++ "do()")
  |> T.pack
  |> T.splitOn "don't()"
  |> L.map (T.splitOn "do()" .> L.tail .> T.concat)
  |> T.concat
  
