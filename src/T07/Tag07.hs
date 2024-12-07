{-# LANGUAGE OverloadedStrings #-}

module T07.Tag07
    ( someFunc
    ) where

import Flow
import Text.Read
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Maybe as M
import qualified Data.Text as T


data Operator = Product | Coproduct | Concatenation
  deriving (Eq, Show)


someFunc :: IO ()
someFunc = do
  -- input <- readFile "./src/T07/input07.txt"
  input <- readFile "./src/T07/inputTest07.txt"
  let equations = input |> T.pack |> parseInput
  let result1 = sumOfCorrectEquations False equations
  print result1
  let result2 = sumOfCorrectEquations True equations
  print result2
  putStrLn "Hallo Welt 7"


parseInput :: T.Text -> [(Int, [Int])]
parseInput txt =
  txt
  |> T.lines
  |> L.map parseSingleLine


parseSingleLine :: T.Text -> (Int, [Int])
parseSingleLine txtLine =
  let
    [resultRaw, operandsRaw] = T.splitOn ": " txtLine
    result = resultRaw |> T.unpack |> read
    operands = operandsRaw |> T.splitOn " " |> L.map (T.unpack .> read)
  in (result, operands)


sumOfCorrectEquations :: Bool -> [(Int, [Int])] -> Int
sumOfCorrectEquations withConcat equations =
  equations
  |> L.filter (numberOfSolutions withConcat .> (1 <=))
  |> L.map fst
  |> L.sum


numberOfSolutions :: Bool -> (Int, [Int]) -> Int
numberOfSolutions withConcat (result, operands) =
  operands
  |> L.length
  |> flip (-) 1
  |> generateOperators withConcat
  |> L.map (calc operands)
  |> L.filter (== result)
  |> L.length


calc :: [Int] -> [Operator] -> Int
calc [operand] _ = operand
calc (value1:value2:vs) (o:os)
  | o == Product = calc ((value1 * value2) : vs) os
  | o == Coproduct = calc ((value1 + value2) : vs) os
  | o == Concatenation = calc (concatInteger value1 value2 : vs) os
calc _ _ = undefined


concatInteger :: Int -> Int -> Int
concatInteger a b =
  let
    digits = b |> show |> T.pack |> T.length
    factor10 = 10 ^ digits
  in a * factor10 + b


generateOperators :: Bool -> Int -> [[Operator]]
generateOperators True 1 = [[Product], [Coproduct], [Concatenation]]
generateOperators True n =
  generateOperators True (n - 1) >>= (\ os -> [Product : os, Coproduct : os, Concatenation : os])
generateOperators False 1 = [[Product], [Coproduct]]
generateOperators False n =
  generateOperators False (n - 1) >>= (\ os -> [Product : os, Coproduct : os])
