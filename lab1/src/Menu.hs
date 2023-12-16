module Menu where

import Text.Read (readMaybe)

import Table


class Menu a where
    title :: a -> String
    optionTitles :: a -> [String]
    optionActions :: a -> [Table -> IO ()]
    optionArgument :: a -> Int -> Table

    printMenu :: a -> IO ()
    printMenu menu = do
        putStrLn $ "______________________"
        putStrLn $ title menu ++ ":"
        mapM_ putStrLn $ zipWith (\i opt -> show i ++ " <- " ++ opt) [0..] ([".."] ++ optionTitles menu)

    run :: a -> IO ()
    run menu = do
        printMenu menu
        putStr  "\nSelect option: "
        opt <- getLine
        case readMaybe opt of
            Just intOpt -> do
                if intOpt == 0
                    then return ()
                    else do
                        if intOpt > 0 && intOpt <= (length $ optionActions menu)
                            then (optionActions menu !! (intOpt - 1)) (optionArgument menu (intOpt - 1))
                            else putStrLn "Error: unrecognised option."
                        run menu
            Nothing -> do
                putStrLn "Error: invalid input."
                run menu
