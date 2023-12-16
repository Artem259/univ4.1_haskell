module Table where


data Table = Table {
    name :: String,
    fields :: [String],
    currId :: Int
}


emptyTable :: Table
emptyTable = Table {
        name = "",
        fields = [],
        currId = 0
    }
