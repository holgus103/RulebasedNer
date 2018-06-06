module Main where

import Parser
import Rules.Commons
import Flow
import qualified Data.Map as Map
import NER
import System.Random
import System.Random.Shuffle

main :: IO (Map.Map Token (Float, Float, Float))
main = do
    d <- loadDataset "output.json"
    gen <- getStdGen
    let len = length d
    let shuffled = shuffle' d len gen
    let (train, test) = splitAt (round (0.7 * (fromIntegral len))) shuffled 
    let total = sum $ map (\x -> Rules.Commons.words x |> length) test
        f1calc (itp, ifp, ifn) = (tp/(tp + fp), tp/(tp + fn), 2.0 / ((tp + fp)/tp + (tp + fn)/tp))
            where
                tp = fromIntegral itp
                fp = fromIntegral ifp
                fn = fromIntegral ifn
        dict = (buildDictionary train) in 
            [None, Company, Unit, Num, Product, Shop]
            |> map (\x -> (x, (0, 0, 0))) 
            |> Map.fromList
            |> performTest test dict
            |> Map.map f1calc
            |> return


performTest :: [Sample] -> Map.Map String Token-> Map.Map Token (Int, Int, Int) -> Map.Map Token (Int, Int, Int)
performTest (sample:testData) dict map =
    let outputs = NER.processText (Rules.Commons.words sample) dict in
        compareOutputs (Rules.Commons.labels sample) outputs map
        |> performTest testData dict 

performTest [] _ map = map


compareOutputs :: [Token] -> [Token] -> Map.Map Token (Int, Int, Int) -> Map.Map Token (Int, Int, Int) 
compareOutputs (ex:expected) (o:outputs) map = 
    if ex == o then 
        Map.update (\(tp, fp, fn) -> Just (tp+1, fp, fn)) ex map
        |> compareOutputs expected outputs
    else
        Map.update (\(tp, fp, fn) -> Just (tp, fp, fn + 1)) ex map
        |> Map.update (\(tp, fp, fn) -> Just (tp, fp + 1, fn)) o 
        |> compareOutputs expected outputs

compareOutputs _ _ map = map







