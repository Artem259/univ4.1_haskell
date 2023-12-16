module Service where

import System.IO
  
  
concatWith :: [String] -> String -> String -> String
concatWith [] _ _= " "
concatWith [x] quotes delimiter = quotes ++ x ++ quotes
concatWith (x:xs) quotes delimiter = quotes ++ x ++ quotes ++ delimiter ++ concatWith xs quotes delimiter


inputRequest :: (String -> String) -> [String] -> IO [String]
inputRequest formatFunc fields = mapM (\x -> putStr (formatFunc x) >> getLine) fields
