{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Backend.SVG.CmdLine
import qualified Data.Map as Map
import Data.Function
import Data.List
import System.IO
import PositionNodes
import DiagramStyle





main :: IO ()
main = do
  readData <- readFile "listofwords.txt"
  let listOfWords = read readData :: [(String,Float)]
  readMap <- readFile "mapofwords.txt"
  let listOfListOfWords = read readMap :: [(String,[(String,Float)])]
  let mapOfWords = constructMap listOfListOfWords
  let positions = allPositions listOfWords mapOfWords 35
  mapM_ putStrLn $ map fst listOfWords
  mainWith $ example2 positions ( take 35 $ sortListBySecondElement listOfWords) mapOfWords

constructMap :: [(String,[(String,Float)])] -> Map.Map String (Map.Map String Float)
constructMap xs = Map.fromList $ map (\x -> (fst x, Map.fromList (snd x))) xs


sortListBySecondElement :: (Ord b) => [(a,b)] -> [(a,b)]
sortListBySecondElement xs = reverse $ sortBy (compare `on` snd) xs
