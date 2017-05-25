module WordListFunctions
(
createZeroMap,
createMapOfWords,
filterMap,
groupWords,
divideSet,
conxnToGroup,
updateGroups,
newGrouping,
maxConnectionGroup,
connectivity,
connectionStrength
)where

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Function
import Data.List
import Data.Char
import Data.Maybe

connectionStrength :: String -> String -> Map.Map String (Map.Map String Float) -> Float
connectionStrength xs ys mp = fromJust $ fromJust $ fmap (Map.lookup ys) (Map.lookup xs mp)

createZeroMap :: [String] -> Map.Map String (Map.Map String Float)
createZeroMap xs = Map.fromList (zip xs (cycle [simpleList]) )
  where simpleList = Map.fromList (zip xs [0,0..])

createMapOfWords :: [[String]] -> Map.Map String (Map.Map String Float) -> Map.Map String (Map.Map String Float)
createMapOfWords xs emptyMap = foldl (foldingFunction) emptyMap xs

foldingFunction :: Map.Map String (Map.Map String Float) -> [String] -> Map.Map String (Map.Map String Float)
--foldingFunction xm xs =  foldl (\acc (a,b) -> Map.adjust (\q -> Map.adjust (+0.01) a q) b acc) xm [(x,y) | x <- xs, y <- xs]
foldingFunction xm xs = foldl (\acc (a,b) -> updateMap a b acc) xm [(x,y) | x <- xs, y <- xs]

updateMap :: String -> String -> Map.Map String (Map.Map String Float) -> Map.Map String (Map.Map String Float)
updateMap word1 word2 xm = Map.adjust (\q -> Map.adjust (+0.02) word2 q) word1 xm

filterMap :: Map.Map String (Map.Map String Float) -> [String] -> Map.Map String (Map.Map String Float)
filterMap mp xs = filterTopWords $ Map.map (filterTopWords) mp
  where filterTopWords = Map.filterWithKey (\k _ -> k `elem` xs)


groupWords :: Map.Map String (Map.Map String Float) -> [String] -> [Set.Set String]
groupWords mp xs = foldl (updateGroups mp) initialSet [1..100]
  where singletonSet = [Set.fromList xs]
        initialSet = divideSet singletonSet mp

divideSet :: [Set.Set String] -> Map.Map String (Map.Map String Float) -> [Set.Set String]
divideSet xs mp =
    let unconnectedSets = map (\x -> Set.filter (\y -> (conxnToGroup y x mp) < (meanConxnStrength x)) x ) xs
        unionOfUnconnectedSets = Set.unions $ unconnectedSets
        filteredSets = map (\x -> Set.difference x unionOfUnconnectedSets) xs
        newSet = filteredSets ++ [unionOfUnconnectedSets]
    in  newSet
  where meanConxnStrength p = (sum $ Set.map (\l -> conxnToGroup l p mp) p)/(fromIntegral $ Set.size p)

conxnToGroup :: String -> Set.Set String -> Map.Map String (Map.Map String Float) -> Float
conxnToGroup q qs mp = (sum $ Set.map (\p -> connectionStrength q p mp ) $ setWout)/ (fromIntegral $ Set.size setWout)
  where setWout= Set.delete q qs

updateGroups :: Map.Map String (Map.Map String Float) -> [Set.Set String] -> Int -> [Set.Set String]
updateGroups mp xs _ = foldl (newGrouping mp) xs [1..(length xs)]

newGrouping :: Map.Map String (Map.Map String Float) -> [Set.Set String] -> Int -> [Set.Set String]
newGrouping mp ys@(x:xs) _ = moveHeadToEnd $ foldl (maxConnectionGroup mp) ys x
    where moveHeadToEnd (q:qs) = qs++[q]

maxConnectionGroup :: Map.Map String (Map.Map String Float) -> [Set.Set String] -> String -> [Set.Set String]
maxConnectionGroup mp (x:y:[]) xw = if ((connectivity mp xw x) > (connectivity mp xw y))
                                    then (Set.insert xw x):[y]
                                    else (Set.delete xw x):[Set.insert xw y]
maxConnectionGroup mp (x:xs) xw = if ((connectivity mp xw x) > (maximum $ map (connectivity mp xw) xs))
                                  then (Set.insert xw x):xs
                                  else (Set.delete xw x):(maxConnectionGroup mp xs xw)

connectivity :: Map.Map String (Map.Map String Float) -> String -> Set.Set String -> Float
connectivity mp xw xs = meanConxnStrength (Set.insert xw xs) - meanConxnStrength (Set.delete xw xs)
  where meanConxnStrength p = (sum $ Set.map (\l -> conxnToGroup l p mp) p)/(fromIntegral $ Set.size p)
