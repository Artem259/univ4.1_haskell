module Main where

import System.IO

import DBManager
import MainMenu (createMainMenu)
import Menu


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    conn <- openDB
    run $ createMainMenu conn
    closeDB conn
