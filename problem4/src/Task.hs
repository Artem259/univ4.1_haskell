module Task (findPaths) where
  
type Graph = [[Int]]
type Path = [Int]


findPaths :: Graph -> Int -> Int -> [Path]
findPaths graph start end = dfs graph start end []


dfs :: Graph -> Int -> Int -> Path -> [Path]
dfs graph current end path
    | current == end = [path ++ [end]]
    | otherwise = concatMap (\neighbor -> dfs graph neighbor end (path ++ [current])) neighbors
    where neighbors = filter (`notElem` path) (graph !! current)
