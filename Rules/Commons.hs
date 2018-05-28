module Rules.Commons where 


data Token = None | Company | Unit | Num | Product | Shop deriving Show

data Tagging = Matched [Token] [String] | Unmatched deriving Show

tokenFromString :: String -> Token
tokenFromString s = 
    case s of
        "wit$number" -> Num
        "company" -> Company
        "unit" -> Unit 
        "product" -> Product
        "shop" -> Shop