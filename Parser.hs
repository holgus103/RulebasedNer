{-# LANGUAGE DuplicateRecordFields, DeriveDataTypeable #-}
module Parser where

import Text.JSON
import Text.JSON.Generic
import System.IO

loadDataset :: String -> IO [Subentity]
loadDataset path = do
    input <- readFile path
    return ((decodeJSON input) :: [Subentity])    

data Subentity = Subentity {
    end :: Int,
    value :: String,
    entity :: String,
    start :: Int
} deriving (Eq, Data, Show, Typeable)


buildDictionary :: [Subentity] -> Set
    