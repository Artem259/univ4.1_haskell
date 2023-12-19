module Main (main) where

import System.IO

import Data.Maybe (mapMaybe)


-- Parse integers from strings
readInt :: String -> Maybe Int
readInt s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing


splitString :: Eq a => a -> [a] -> [[a]]
splitString _ [] = []
splitString delimiter list =
    let (before, rest) = break (== delimiter) list
    in before : case rest of
               [] -> []
               (_:xs) -> splitString delimiter xs


sumOfString :: String -> Maybe Int
sumOfString s = do
    let ints = mapMaybe readInt (splitString ',' s)
    return (sum ints)


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    putStrLn "Enter a comma-separated list of integers: "
    input <- getLine
    case sumOfString input of
        Just result -> putStrLn $ "Sum of integers: " ++ show result
        Nothing -> putStrLn "Invalid input. Valid input example: 1,0,3,-5"
