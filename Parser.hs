{-# LANGUAGE DuplicateRecordFields, DeriveDataTypeable #-}
module Parser where

import Text.JSON
import Text.JSON.Generic
import System.IO
import Rules.Commons
import qualified Data.Map as Map
import qualified Data.Set as Set
import Flow


loadDataset :: String -> IO [Sample]
loadDataset path = do
    input <- readFile path
    return $ map sampleFromRawEntry (decodeJSON input :: [[[String]]])
    

sampleFromRawEntry :: [[String]] -> Sample
sampleFromRawEntry entry =
    Sample {words = tail entry |> head, labels = head entry |> map tokenFromString }
    

buildDictionary :: [Sample] ->  Map.Map String Token 
buildDictionary input = 
    zip allWords allLabels
    |> filter (\(word, label) -> label /= None)
    |> Map.fromList
    where
        allWords = concatMap Rules.Commons.words input
        allLabels = concatMap Rules.Commons.labels input