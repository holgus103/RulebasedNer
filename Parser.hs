{-# LANGUAGE DuplicateRecordFields, DeriveDataTypeable #-}
module Parser where

import Text.JSON
import Text.JSON.Generic
import System.IO

loadDataset :: String -> IO [Entry]
loadDataset path = do
    input <- readFile path
    return ((decodeJSON input)::[Entry]) 

data Subentity = Subentity {
    end :: Int,
    value :: String,
    entity :: String,
    start :: Int
} deriving (Eq, Show, Data, Typeable)

data MainEntity = MainEntity {
    end :: Int,
    value :: String,
    entity :: String,
    start :: Int,
    subentities :: [Subentity]
} deriving (Eq, Show, Data, Typeable)
    
data Entry = Entry {
    text :: String,
    entities :: [MainEntity]
    
} deriving (Eq, Show, Data, Typeable)