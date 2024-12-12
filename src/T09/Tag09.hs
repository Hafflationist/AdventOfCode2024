{-# LANGUAGE OverloadedStrings #-}

module T09.Tag09
    ( someFunc
    ) where

import Flow
import qualified Data.Bifunctor as B
import qualified Data.List as L
import qualified Data.Maybe as M


type FileId = Int
type Diskmap = [Int]
data FileIndexed = FileIndexed { fileId :: FileId, filePos :: Int, fileSize :: Int } deriving Show


someFunc :: IO ()
someFunc = do
  input <- readFile "./src/T09/input09.txt"
  -- input <- readFile "./src/T09/inputTest09.txt"
  let diskmap = input |> parseInput
  let result1 = checksumAfterCompacting diskmap
  print result1
  let result2 = checksumAfterCompactingWholeFiles diskmap
  print result2
  putStrLn "Hallo Welt 9"


parseInput :: [Char] -> Diskmap
parseInput = lines .> L.head .> L.map ((:[]) .> read)


checksumAfterCompacting :: Diskmap -> Int
checksumAfterCompacting = flattenDiskmap .> compacting .> checksum


checksumAfterCompactingWholeFiles :: Diskmap -> Int
checksumAfterCompactingWholeFiles = indexing .> compactingWholeFiles .> checksumIndexed


flattenDiskmap :: Diskmap -> [FileId]
flattenDiskmap diskmap = fdFile diskmap 0 []
  where 
    fdFile [] _ acc = acc
    fdFile (size:dm) n acc = fdSpace dm n (acc ++ (repeat n |> L.take size))
    fdSpace [] _ acc = acc
    fdSpace (size:dm) n acc = fdFile dm (n + 1) (acc ++ (repeat (-1) |> L.take size))


compacting :: [FileId] -> [FileId]
compacting fileIds =
  let
    newFileIdsWithoutCut = ci fileIds (L.reverse fileIds)
    lengthWithoutSpace = fileIds |> L.filter (/= -1) |> L.length
  in L.take lengthWithoutSpace newFileIdsWithoutCut
  where
    ci :: [FileId] -> [FileId] -> [FileId]
    ci [] _             = []
    ci (f:fs) []        = f  : ci fs []
    ci fs (-1:fsr)      = ci fs fsr
    ci (-1:fs) (fr:fsr) = fr : ci fs fsr
    ci (f:fs) (fr:fsr)  = f  : ci fs (fr:fsr)


indexing :: Diskmap -> [FileIndexed]
indexing diskmap = fdFile diskmap 0 0 []
  where 
    fdFile [] _ _ acc = acc
    fdFile (size:dm) n pos acc = fdSpace dm n (pos + size) (acc ++ [FileIndexed { fileId = n, filePos = pos, fileSize = size }])
    fdSpace [] _ _ acc = acc
    fdSpace (size:dm) n pos acc = fdFile dm (n + 1) (pos + size) acc


compactingWholeFiles :: [FileIndexed] -> [FileIndexed]
compactingWholeFiles fileIndexed =
  let
    fileIndexedReversed = L.reverse fileIndexed
  in
    [0 .. (L.length fileIndexed - 1)]
    |> L.reverse
    |> L.foldl compactingWholeFilesStep fileIndexed


compactingWholeFilesStep :: [FileIndexed] -> FileId -> [FileIndexed]
compactingWholeFilesStep fis fid =
  let
    fiInter@FileIndexed { fileId = id, filePos = pod, fileSize = size } =
      fis |> L.filter (fileId .> (== fid)) |> L.head
  in
    L.zip fis (L.tail fis)
    |> L.find (\ (fi1, fi2) -> doesFit fi1 fiInter fi2 )
    |> fmap (\ (fi1, fi2) -> (fi1, fiInter { filePos = filePos fi1 + fileSize fi1 }))
    |> fmap (replaceFileIndexed fis)
    |> M.fromMaybe fis


replaceFileIndexed :: [FileIndexed] -> (FileIndexed, FileIndexed) -> [FileIndexed]
replaceFileIndexed fis (fiPrev, fiNew) =
  let
    posOld =
      fis 
      |> L.filter (fileId .> (== fileId fiNew))
      |> L.head 
      |> filePos
    posNew = filePos fiNew
  in
    if posOld > posNew
    then
      fis
      |> L.map (\ f -> if fileId f == fileId fiNew then fiNew else f)
      |> L.sortOn filePos
    else fis


doesFit :: FileIndexed -> FileIndexed -> FileIndexed -> Bool
doesFit
  (FileIndexed { filePos = pos1, fileSize = size1 })
  (FileIndexed { filePos = pos2, fileSize = size2 }) 
  (FileIndexed { filePos = pos3, fileSize = size3 }) =
  let
    ok12 = pos1 + size1 <= pos2
    ok23 = pos1 + size1 + size2 <= pos3
  in ok12 && ok23


-- checksumIndexed :: [FileIndexed] -> Int
checksumIndexed fis =
  let
    FileIndexed {fileId = _, filePos = lastPos, fileSize = lastSize} = L.last fis
    diskSize = lastPos + lastSize + 10 -- 10 für das gute Gewissen
  in
    [0 .. diskSize]
    |> L.map ci
    |> L.sum
  where
    ci :: Int -> Int
    ci idx =
      fis 
      |> L.find (\ fi -> filePos fi <= idx && idx <= filePos fi + fileSize fi - 1)
      |> fmap (\ fi -> fileId fi * idx)
      |> M.fromMaybe 0



checksum :: [FileId] -> Int
checksum fileIds =
  let
    positions = [0..(L.length fileIds - 1)]
    fileId2Int :: FileId -> Int
    fileId2Int (-1) = 0
    fileId2Int x = x
  in
    L.zip positions fileIds
    |> L.map (B.second fileId2Int)
    |> L.map (uncurry (*))
    |> L.sum
