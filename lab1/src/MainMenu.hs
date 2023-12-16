module MainMenu where

import Database.MySQL.Base
  
import Menu
import StudentsMenu
import Table


data MainMenu = MainMenu {
    title :: String,
    optionTitles :: [String],
    optionActions :: [Table -> IO ()]
}


createMainMenu :: MySQLConn -> MainMenu
createMainMenu conn = MainMenu 
        "Main menu"
        ["Students", "Staff", "Consultations"]
        [\_ -> run $ createStudentsMenu conn, \_ -> print 2, \_ -> print 3]   -- TODO


instance Menu MainMenu where
    title (MainMenu title' _ _) = title'
    optionTitles (MainMenu _ optionTitles' _) = optionTitles'
    optionActions (MainMenu _ _ optionActions') = optionActions'
    optionArgument _ _ = emptyTable
    