module Rules.Company where 

import Rules.Commons

companyKeywords :: [String]
companyKeywords = ["firmy", "firma"]

detectCompany :: [String] -> Tagging
detectCompany (x:x2:xs) = 
    if elem x companyKeywords && isCompany x2 then Matched [None, Company] $ xs 
    else Unmatched

detectCompany [] = Unmatched

isCompany :: String -> Bool
isCompany x = True