{-# LANGUAGE OverloadedStrings #-}

module T13.Tag13
    ( someFunc
    ) where

import Flow
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Ratio as R
import qualified Data.Maybe as M


data ClawMachine =
  ClawMachine { clawMachineA :: (Integer, Integer), clawMachineB :: (Integer, Integer), clawMachinePrize :: (Integer, Integer) }
  deriving Show

someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T13/input13.txt"
  -- input <- readFile "./src/T13/inputTest13.txt"
  let clawMachines = parseInput input
  let result1 = clawMachines |> minTokensToWinAll
  print result1
  let result2 = clawMachines |> L.map (increasePrize 10000000000000) |> minTokensToWinAll
  print result2
  putStrLn "Hallo Welt 13"


parseInput :: String -> [ClawMachine]
parseInput txt =
  txt
  |> lines
  |> LS.splitOn [""]
  |> L.filter (L.length .> (3 ==))
  |> L.map parseSingleMachine


parseSingleMachine :: [String] -> ClawMachine
parseSingleMachine [txtA, txtB, txtPrize] =
  let
    cmA = parseButton txtA
    cmB = parseButton txtB
    prizeX = txtPrize |> LS.splitOn "X=" |> L.last |> LS.splitOn "," |> L.head |> read
    prizeY = txtPrize |> LS.splitOn "Y=" |> L.last |> read
  in ClawMachine { clawMachineA = cmA, clawMachineB = cmB, clawMachinePrize = (prizeX, prizeY) }
parseSingleMachine _ = undefined


increasePrize :: Integer -> ClawMachine -> ClawMachine
increasePrize inc ClawMachine { clawMachineA = (cmAXInt, cmAYInt), clawMachineB = (cmBXInt, cmBYInt), clawMachinePrize = (cmPXInt, cmPYInt) } =
  ClawMachine { clawMachineA = (cmAXInt, cmAYInt), clawMachineB = (cmBXInt, cmBYInt), clawMachinePrize = (cmPXInt + inc, cmPYInt + inc) }


parseButton :: String -> (Integer, Integer)
parseButton txt =
  let
    x = txt |> LS.splitOn ": X+" |> L.last |> LS.splitOn "," |> L.head |> read
    y = txt |> LS.splitOn ", Y+" |> L.last |> read
  in (x, y)


minTokensToWinAll :: [ClawMachine] -> Integer
minTokensToWinAll cms =
  cms
  |> L.map costOfClawMachine
  |> M.catMaybes
  |> L.sum


costOfClawMachine :: ClawMachine -> Maybe Integer
costOfClawMachine ClawMachine { clawMachineA = (cmAXInt, cmAYInt), clawMachineB = (cmBXInt, cmBYInt), clawMachinePrize = (cmPXInt, cmPYInt) } =
  let
    cmAX = toRational cmAXInt
    cmAY = toRational cmAYInt
    cmBX = toRational cmBXInt
    cmBY = toRational cmBYInt
    cmPX = toRational cmPXInt
    cmPY = toRational cmPYInt
    b = ((cmPY / cmAY) - (cmPX / cmAX)) / ((cmBY / cmAY) - (cmBX / cmAX))
    a = (cmPX - cmBX * b) / cmAX
  in
    if isInteger a && isInteger b
    then Just (round a * 3 + round b)
    else Nothing


isInteger :: Rational -> Bool
isInteger = R.denominator .> (== 1)
