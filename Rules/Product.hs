module Rules.Product where 

import Rules.Commons
import Text.Read

productKeywords :: [String]
productKeywords = ["kupić", "poproszę", "chciałbym", "chciałabym"]

units :: [String]
units = ["kg", "kilo", "paczka"]

numbers :: [String]
numbers = ["jeden","dwa", "trzy", "cztery", "pięć", "sześć", "siedem", "osiem", "dziewięć", "dziesięć"]
    -- "kupić" unit product
    -- "kupić" number unit product
    -- "kupić" number product

detectProducts :: [String] -> Tagging
detectProducts [] = Unmatched
detectProducts (x:xs) =
    if elem x productKeywords then 
        case productNumOrUnit xs  of Unmatched -> Unmatched
                                     Matched tokens rest -> Matched ([None] ++ tokens) rest
    else Unmatched

productNumOrUnit :: [String] -> Tagging
productNumOrUnit (x:xs) = 
    if isUnit x then 
        case productUnit xs of Matched tokens rest -> Matched ([Unit] ++ tokens) rest
                               Unmatched -> Unmatched
    else 
        if isNumber x then 
            case productNumber xs of Matched tokens rest -> Matched ([Num] ++ tokens) rest 
                                     Unmatched -> Unmatched
        else Unmatched

productNumOrUnit _ = Unmatched

productUnit :: [String] -> Tagging
productUnit (x:xs) =
    if isProduct x then Matched [Product] xs
    else Unmatched

productUnit _ = Unmatched
    

productNumber :: [String] -> Tagging
productNumber (x:xs) = 
    if isUnit x then
        case productUnit xs of Matched tokens rest -> Matched ([Unit] ++ tokens) rest
                               Unmatched -> Unmatched
    else Unmatched


isUnit :: String -> Bool
isUnit x = elem x units

isProduct :: String -> Bool
isProduct _ = True

isNumber :: String -> Bool
isNumber x = 
    case ((readMaybe x):: Maybe Int) of Just x -> True
                                        Nothing -> elem x numbers

