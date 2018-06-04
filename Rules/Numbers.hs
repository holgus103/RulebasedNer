module Rules.Numbers where 

import Flow
import Text.Read 
import Rules.Commons
import Data.List

detectNumbers :: [String] -> Tagging
detectNumbers v@(val:rest) =
    [tryParsing v, checkEndings v]
    |> find (\x -> case x of
        Matched token rest -> True
        Unmatched -> False )
    |> (\x -> case x of 
        Just res -> res
        Nothing -> Unmatched)

detectNumbers _ = Unmatched

numberEndings :: [String]
numberEndings = ["naście", "dziesiąt", "set"]

checkEndings :: [String] -> Tagging
checkEndings (val:rest) =
    if any (\x -> isSuffixOf x val) numberEndings then Matched [Num] rest
    else Unmatched

tryParsing :: [String] -> Tagging
tryParsing (val:rest) = 
    case (readMaybe val :: Maybe Float) of 
        Just x -> Matched [Num] rest
        Nothing -> Unmatched



