module Main (main) where

import System.Environment (getArgs)
import ParallelGaussianElimination (run)


-- Parse a string into a list of doubles
parseDoubles :: String -> [Double]
parseDoubles s = map read $ words s


-- Parse a string into a 2D list of doubles
read2dDoublesFromString :: String -> IO [[Double]]
read2dDoublesFromString content = do
    return (map parseDoubles (lines content))


-- Arguments: input_path threads_num
main :: IO ()
main = do
    args <- getArgs
    let filePath = head args :: String
        threadsNum = read $ args !! 1 :: Int
    fileContentString <- readFile filePath
    fileContentDoubles <- read2dDoublesFromString fileContentString
    let a = map init fileContentDoubles :: [[Double]]
    let b = map last fileContentDoubles :: [Double]
    putStrLn ("A: " ++ show a)
    putStrLn ("b: " ++ show b)
    result <- run a b threadsNum
    putStrLn ("\nRes: " ++ show result)
