module Rules.Commons where 


data Token = None | Company | Unit | Num | Product | Shop deriving Show

data Tagging = Matched [Token] [String] | Unmatched deriving Show