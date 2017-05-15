{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Backend.SVG.CmdLine
import qualified Data.Map as Map
import Data.Function
import Data.List
import System.IO
import Diagrams.Backend.SVG
import qualified Diagrams.Size as Size
import qualified Diagrams.TwoD.Size as TwoDSize
import PositionNodes
import DiagramStyle



--dimensions :: Size.SizeSpec [Maybe Float]
--dimensions = Size.mkSizeSpec [Just 400, Just 400]

main :: IO ()
main = do
  readData <- readFile "listofwords3.txt"
  let listOfWords = read readData :: [(String,Float)]
  readMap <- readFile "mapofwords3.txt"
  let listOfListOfWords = read readMap :: [(String,[(String,Float)])]
  let mapOfWords = constructMap listOfListOfWords
  let positions = allPositionsTraj listOfWords mapOfWords 10
  mapM_ putStrLn $ map fst listOfWords
  --mainWith $ example2 positions listOfWords mapOfWords
  let dimensions = (TwoDSize.mkSizeSpec2D (Just 400) (Just 400))
  let positionsWithIndex = zip positions [1..]
  a <- sequence $ map (\(x,i) -> renderSVG ("output"++ show i ++".svg") dimensions (example2 x listOfWords mapOfWords)) positionsWithIndex
  print a

constructMap :: [(String,[(String,Float)])] -> Map.Map String (Map.Map String Float)
constructMap xs = Map.fromList $ map (\x -> (fst x, Map.fromList (snd x))) xs


sortListBySecondElement :: (Ord b) => [(a,b)] -> [(a,b)]
sortListBySecondElement xs = reverse $ sortBy (compare `on` snd) xs
