module PositionNodes
(
allPositions
)where

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


import Data.Maybe
import qualified Data.Map as Map
import System.Random

connectionStrength :: String -> String -> Map.Map String (Map.Map String Float) -> Float
connectionStrength xs ys mp = fromJust $ fromJust $ fmap (Map.lookup ys) (Map.lookup xs mp)

allPositions :: [(String,Float)] -> Map.Map String (Map.Map String Float) -> Int -> [[Float]]
allPositions xs mp n = foldl (minimize xs mp) pos_init [1..2]
  --where pos_init = map(\(x,y) -> [Float.double2Float x, Float.double2Float y]) $ map unp2 (trailVertices $ regPoly (length xs) 1)
  --where pos_init = [[0,1],[1,0],[2,0]]
  where pos_init = tupleToList $ zip (take (length xs) $ randomRs(0,2) (mkStdGen 5)) (take (length xs) $ randomRs(0,2) (mkStdGen 8))
        tupleToList = map (\(x,y) -> [x,y])

minimize :: [(String,Float)] -> Map.Map String (Map.Map String Float) -> [[Float]] -> Int -> [[Float]]
minimize xs mp pos_init n = zipWith (\[x,y] [z,q] -> [x+0.05*z, y+0.05*q] ) pos_init derivatives
  where derivatives = derivs pos_init xs mp


derivs :: [[Float]] -> [(String,Float)] -> Map.Map String (Map.Map String Float) -> [[Float]]
derivs pos_init xs mp = map (\(_,x,_) -> x) $ forces triple (Map.fromList xs) mp
    where triple = map (\((a,b),(c,d)) -> (a,b,c)) $ zip (zip pos_init $ cycle [[0,0]]) xs
  --where toList = map (\(x,y) ->[x,y])

forces :: [([Float],[Float],String)] -> Map.Map String Float -> Map.Map String (Map.Map String Float) -> [([Float],[Float],String)]
forces [] xs mp = []
forces (x:[]) xs mp = [x]
forces (x:y:[]) xs mp = [pairTot x y xs mp, pairTot y x xs mp]
forces (y:ys) xs mp = (foldl(\q x -> pairTot q x xs mp) y ys) : (map(\q -> pairTot q y xs mp) ys)


sumLists :: (Num a) => [[a]] -> [a]
sumLists (x:[]) = x
sumLists (x:y:[]) = zipWith (+) x y
sumLists (x:xs) = zipWith (+) x $ sumLists xs

pairTot :: ([Float],[Float],String) -> ([Float],[Float],String) -> Map.Map String Float -> Map.Map String (Map.Map String Float) -> ([Float],[Float],String)
pairTot (a,b,c) (d,e,f) xs mp = (a, zipWith (+) b (pair a d w1 w2 cnxnValue), c)
    where cnxnValue = connectionStrength c f mp
          w1 = fromJust $ Map.lookup c xs
          w2 = fromJust $ Map.lookup f xs

pair :: [Float] -> [Float] -> Float -> Float -> Float -> [Float]
pair p1 p2 w1 w2 c = map( multFactor* ) $ zipWith (-) p1 p2
  where distance = sqrt $ sum $ map (^2) $ zipWith (-) p1 p2
        multFactor = -2*c*(distance-sqrt(w1*w2))/distance
