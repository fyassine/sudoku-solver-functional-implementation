module Exercise where

import Text.Printf (printf)
import Data.List
    ( elemIndex, intercalate, intersect, nub, transpose )
import Data.Maybe

selectRow :: [[Int]] -> Int -> [Int]
selectRow xss i = xss !! i

selectColumn :: [[Int]] -> Int -> [Int]
selectColumn xss i = transpose xss !! i

intRoot :: Int -> Int
intRoot = floor . sqrt . fromIntegral

--return numbers in square as a list. squares are numbered  from left to right and top to bottom
--e.g. :
--[0,1,2]
--[3,4,5]
--[6,7,8]
selectSquare :: [[Int]] -> Int -> [Int]
selectSquare xss i = concat (takeRows xss rowNumber startingIndex squareLength)
  where
    squareLength = intRoot (length xss)
    rowNumber = (i `div` squareLength) * squareLength
    startingIndex = (i `mod` squareLength) * squareLength 

--helper
takeRows :: [[Int]] -> Int -> Int -> Int -> [[Int]]
takeRows xss rowNumber startingIndex squareLength = [[x] | x <- wantedRows, x <- take squareLength (drop startingIndex x)] 
    where
      wantedRows = take squareLength (drop rowNumber xss)

isValidSubsection :: [Int] -> Bool
isValidSubsection x = length (nub xm) == length xm
  where
    xm = x `intersect` [1,2,3,4,5,6,7,8,9]

isValidSquares :: [[Int]] -> Int -> Bool
isValidSquares xss n
  | n > (length xss - 1) = True
  | otherwise = isValidSubsection (selectSquare xss n) && isValidSquares xss (n+1)

isValidSubsections :: [[Int]] -> Bool
isValidSubsections [] = True
isValidSubsections (x:xs) = isValidSubsection x && isValidSubsections xs

isValidSudoku :: [[Int]] -> Bool
isValidSudoku xss = isValidSubsections xss && isValidSubsections (transpose xss) && isValidSquares xss 0

setCell :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
setCell xss (x, y) n = take x xss ++ [take y row ++ [n] ++ drop (y + 1) row] ++ drop (x + 1) xss 
  where
    -- split the row where the element to be replaced is
    row = head (drop x xss)

tryAssignments :: [[Int]] -> (Int, Int) -> [Int] -> [[Int]]
tryAssignments xss (x, y) list
  | null list = [] --no more assignments to try which means we tried all numbers from 0 to 9
  | isValidSudoku try && not (null probableSolution) = probableSolution --the assignment is valid
  | otherwise = tryAssignments xss (x, y) li --trying the next number in the "ints" list
  where
    (l:li) = list
    try = setCell xss (x, y) l
    probableSolution = solveSudoku try

isEmpty :: [[Int]] -> [[Int]] -> Int -> (Int, Int)
isEmpty xss xss2 i
  | null xss2 = (-1, -1) --xss2 (the leftover of xss) is empty which means we went beyond the last row
  | 0 `elem` x = (i, fromJust $ elemIndex 0 x) --found 0 in the row number i
  | otherwise = isEmpty xss xs (i+1) --look for 0 in the next row
  where
    x = head xss2
    xs = tail xss2

solveSudoku :: [[Int]] -> [[Int]]
solveSudoku xss
  | null xss = [] --not solvable
  | (x, y) == (-1, -1) = xss --succeeded
  | (x, y) /= (-1, -1) = solveSudoku (tryAssignments xss (x, y) assignments) --looping from 1 to 9 and checking if sudoku is solved
  where
    (x, y) = isEmpty xss xss 0
    assignments = [1..length xss]

--not solvable
easySudoku2 :: [[Int]]
easySudoku2 = [[1,0,0,0],
              [0,0,0,0],
              [2,1,0,0],
              [0,1,0,0]]

--solvable
easySudoku3 :: [[Int]]
easySudoku3 = [[0,0,0,0],
              [2,0,0,0],
              [4,0,0,0],
              [0,0,0,0]]

--solvable
easySudoku4 :: [[Int]]
easySudoku4 = [[0,0,0,0],
              [0,0,0,0],
              [1,0,0,0],
              [0,0,0,2]]


hardSudoku :: [[Int]]
hardSudoku = [[8,0,0,0,0,0,0,0,0],
              [0,0,3,6,0,0,0,0,0],
              [0,7,0,0,9,0,2,0,0],
              [0,5,0,0,0,7,0,0,0],  
              [0,0,0,0,4,5,7,0,0],
              [0,0,0,1,0,0,0,3,0],
              [0,0,1,0,0,0,0,6,8],
              [0,0,8,5,0,0,0,1,0],
              [0,9,0,0,0,0,4,0,0]]

easySudoku :: [[Int]]
easySudoku = [[0,0,0,2,6,0,7,0,1],
              [6,8,0,0,7,0,0,9,0],
              [1,9,0,0,0,4,5,0,0],
              [8,2,0,1,0,0,0,4,0],
              [0,0,4,6,0,2,9,0,0],  
              [0,5,0,0,0,3,0,2,8],
              [0,0,9,3,0,0,0,7,4],
              [0,4,0,0,5,0,0,3,6],
              [7,0,3,0,1,8,0,0,0]]
         
easySudokuPartiallySolved :: [[Int]]
easySudokuPartiallySolved = 
              [[4,3,5,2,6,9,7,8,1],
              [0,0,0,0,0,0,0,0,0],
              [1,9,7,8,3,4,5,6,2],
              [8,2,6,1,9,5,3,4,7],
              [3,7,4,6,8,2,9,1,5],  
              [0,0,0,0,0,0,0,0,0],
              [5,1,9,3,2,6,8,7,4],
              [2,4,8,9,5,7,1,3,6],
              [7,6,3,4,1,8,2,5,9]]

easySudokuSolved :: [[Int]]
easySudokuSolved = 
              [[4,3,5,2,6,9,7,8,1],
              [6,8,2,5,7,1,4,9,3],
              [1,9,7,8,3,4,5,6,2],
              [8,2,6,1,9,5,3,4,7],
              [3,7,4,6,8,2,9,1,5],  
              [9,5,1,7,4,3,6,2,8],
              [5,1,9,3,2,6,8,7,4],
              [2,4,8,9,5,7,1,3,6],
              [7,6,3,4,1,8,2,5,9]]
  
-- Utility method to show a sudoku
-- show sudoku with
-- >>> putStr (showSudoku sudoku)
showSudoku :: [[Int]] -> String
showSudoku xss = unlines $ intercalate [showDivider] $ chunksOf squareSize $ map showRow xss
  where
    size = length xss
    squareSize = intRoot size 
    numberSize = size `div` 10 + 1

    showRowSection xs = unwords $ map (printf ("%0" ++ show numberSize ++ "d")) xs
    showRow xs = intercalate "|" $ map showRowSection $ chunksOf squareSize xs
    showDivider = intercalate "+" $ replicate squareSize $ replicate ((numberSize + 1) * squareSize - 1) '-'

    chunksOf :: Int -> [e] -> [[e]]
    chunksOf i [] = []
    chunksOf i ls = take i ls : chunksOf i (drop i ls)
