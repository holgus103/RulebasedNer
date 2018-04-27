module Rules.Product where 

import Rules.Commons
import Text.Read

productKeywords :: [String]
productKeywords = ["kupić", "poproszę", "chciałbym", "chciałabym"]

    -- "kupić" unit product
    -- "kupić" number unit product
    -- "kupić" number product

detectProducts :: [String] -> [Token]
detectProducts (x:xs) =
    if elem x productKeywords then None:(productNumorUnit xs)
    else None:(detectProducts xs)

productNumorUnit :: [String] -> [Token]
productNumorUnit (x:xs) = 
    case ((readMaybe x):: Maybe Int) of     
        Nothing -> Unit:(productUnit (xs))
        Just v -> Num:(productNumber (xs))

productNumorUnit [] = []

productUnit :: [String] -> [Token]
productUnit (x:xs) =
    if isProduct x then [Product]
    else [None]

productUnit [] = []

productNumber :: [String] -> [Token]
productNumber (x:xs) = 
    if isUnit x  then [Unit, Product]
    else [Product]


isUnit :: String -> Bool
isUnit _ = True

isProduct :: String -> Bool
isProduct _ = True

