module Main (main) where

import Task (findPaths)

main :: IO ()
main = do
    let graph = [[1, 3, 4], [0, 2, 3], [1, 3], [0, 1, 2, 4], [0, 3]]
    print $ findPaths graph 0 1
