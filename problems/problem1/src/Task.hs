module Task (solveMoves) where

import Data.List (elemIndices)

type Board = [[Int]]
type Cell = (Int, Int)


solveMoves :: Int -> Int -> Int -> Maybe Board
solveMoves r c size = case solveHelper (Just (r, c)) [] 1 size of
    Just result -> Just [[head (elemIndices (i, j) result)| j <- [0..size-1]] | i <- [0..size-1]]
    Nothing -> Nothing


solveHelper :: Maybe Cell -> [Cell] -> Int -> Int -> Maybe [Cell]
solveHelper Nothing _ _ _ = Nothing
solveHelper (Just cell) visited moveCount size
    | moveCount == size * size = Just (visited ++ [cell])
    | otherwise = findSolution cell visited moveCount size (filter (`notElem` visited) (nextPositions cell size))


findSolution :: Cell -> [Cell] -> Int -> Int -> [Cell] -> Maybe [Cell]
findSolution _ _ _ _ [] = Nothing
findSolution cell visited moveCount size (candidate:candidates) = 
    case solveHelper (Just candidate) (visited ++ [cell]) (moveCount + 1) size of
        Just solution -> Just solution
        Nothing -> findSolution cell visited moveCount size candidates
        
        
rowMoves :: [Int]
rowMoves = [-2, -1, 1, 2, 2, 1, -1, -2]
colMoves :: [Int]
colMoves = [1, 2, 2, 1, -1, -2, -2, -1]

nextPositions :: Cell -> Int -> [Cell]
nextPositions (r, c) size = 
    [(r + dr, c + dc) | (dr, dc) <- zip rowMoves colMoves, isValidPosition (r + dr) (c + dc)]
    where isValidPosition r' c' = r' >= 0 && r' < size && c' >= 0 && c' < size
