{-# LANGUAGE OverloadedStrings #-}
module DBManager where

import Data.Text as T
import Database.MySQL.Base
import System.IO.Streams ()
import qualified Data.ByteString.Lazy.Char8 as BtSt ( pack )
import Data.Int
import Control.Exception

import qualified Table
import Service


packQuery :: String -> Query
packQuery s = Query $ BtSt.pack s


openDB :: IO MySQLConn
openDB =
    connect
        defaultConnectInfo { ciPort = 3306,
                             ciUser = "root",
                             ciPassword = "root",
                             ciDatabase = "consultations"
                            }


closeDB :: MySQLConn -> IO ()
closeDB = close


insertRequest :: MySQLConn -> Table.Table -> IO ()
insertRequest conn (Table.Table name fields currId) = do    -- TODO currId
    values <- inputRequest (\x -> "Enter " ++ x ++ ": ") fields
    let query =  ("INSERT INTO " ++ name ++ " (" ++ (concatWith fields "" ",") ++ ") VALUES (" ++ (concatWith values "'" ",") ++ ")")
    res <- try $ execute_ conn (packQuery query)

    case res of
        Left (ERRException err) -> putStrLn ("Error executing query: " ++ show (errMsg  err))
        Right ok -> putStrLn ("Added successfully with id: " ++ show (okLastInsertID ok))
--    putStrLn ("Added successfully with id: " ++ show (okLastInsertID res))
--    case res of
--        OK _ _ _ _ -> putStrLn "Query executed successfully"
--        ERRException err -> putStrLn $ "Error executing query: " ++ T.unpack err
