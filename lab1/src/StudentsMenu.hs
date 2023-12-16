module StudentsMenu where

import Database.MySQL.Base

import Menu
import Table
import DBManager


data StudentsMenu = StudentsMenu {
    title :: String,
    optionTitles :: [String],
    optionActions :: [Table -> IO ()]
}


createStudentsMenu :: MySQLConn -> StudentsMenu
createStudentsMenu conn = StudentsMenu
        "Students menu"
        ["New", "Edit", "Delete"]
        [\x -> insertRequest conn x, \_ -> print 2, \_ -> print 3]   -- TODO


instance Menu StudentsMenu where
    title (StudentsMenu title' _ _) = title'
    optionTitles (StudentsMenu _ optionTitles' _) = optionTitles'
    optionActions (StudentsMenu _ _ optionActions') = optionActions'
    optionArgument _ _ = Table "student" ["first_name", "last_name", "group_name", "graduation_year"] 1
