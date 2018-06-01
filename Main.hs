import Parser

module Main where 

main :: IO ()
main = do
    d <- loadDataset "data.json"
    $ buildDictionary d
    
