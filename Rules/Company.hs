module Rules.Company where 

import Rules.Commons

companyKeywords :: [String]
companyKeywords = ["firmy", "firma"]

detectCompany :: [String] -> [Token] 
detectCompany (x:xs) = 
    if elem x companyKeywords then [None, Company] ++ (detectCompany $ tail xs)
    else None:(detectCompany xs) 

detectCompany [] = []