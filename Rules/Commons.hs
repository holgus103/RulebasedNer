module Rules.Commons where 


data Token = None | Company | Unit | Num | Product | Shop deriving Show


instance Eq Token where
    (==) None None = True
    (==) Company Company = True
    (==) Unit Unit = True
    (==) Num Num = True
    (==) Product Product = True
    (==) Shop Shop = True
    _ == _ = False

instance Ord Token where 
    (<=) x y = (<=) (tokenToInt x) (tokenToInt y) 

data Tagging = Matched [Token] [String] | Unmatched deriving Show

data Sample = Sample {
    words :: [String],
    labels :: [Token]
} deriving (Show)

tokenFromString :: String -> Token
tokenFromString s = 
    case s of
        "wit$number" -> Num
        "company" -> Company
        "unit" -> Unit 
        "product" -> Product
        "shop" -> Shop
        otherwise -> None

tokenToInt :: Token -> Int
tokenToInt x = 
    case x of 
        None -> 0
        Company -> 1
        Unit -> 2
        Num -> 3
        Product -> 4
        Shop -> 5
