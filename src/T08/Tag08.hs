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


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T08/input08.txt"
  -- input <- readFile "./src/T08/inputTest08.txt"
  let area = input |> lines
  let result1 = 0
  print result1
  let result2 = 0
  print result2
  putStrLn "Hallo Welt 8"
