module NER where 

import Rules.Commons
import Rules.Company 
import Rules.Product
import Rules.Numbers
import qualified Data.Map as Map

type Rule = ([String] -> Tagging)




rules :: [Rule] 
rules = [detectCompany, detectProducts, detectNumbers]

processText :: [String] -> Map.Map String Token ->  [Token]
processText [] _ = []
processText text dictionary = 
    tokens ++ (processText rest dictionary)
    where
        (tokens, rest) = applyRules text rules dictionary
    

applyRules :: [String] -> [Rule] -> Map.Map String Token -> ([Token], [String])
applyRules text (rule:remainingRules) dictionary =
    case rule text of Unmatched -> applyRules text remainingRules dictionary
                      Matched tokens rest -> (tokens, rest) 

applyRules (word:text) [] dictionary =
    if Map.member word dictionary then ([dictionary Map.! word], text)
    else ([None], text) 



