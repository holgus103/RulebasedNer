module NER where 

import Rules.Commons
import Rules.Company 
import Rules.Product

type Rule = ([String] -> Tagging)

rules :: [Rule]
rules = [detectCompany, detectProducts]

processText :: [String] -> [Token]
processText [] = []
processText text = 
    tokens ++ (processText rest)
    where
        (tokens, rest) = applyRules text rules
    

applyRules :: [String] -> [Rule] -> ([Token], [String])
applyRules text (rule:remainingRules) =
    case rule text of Unmatched -> applyRules text remainingRules
                      Matched tokens rest -> (tokens, rest) 

    
    