module PositionNodes
(
allPositions,
allPositionsTraj
)where

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Trail
import Diagrams.TwoD.Types
import Diagrams.TwoD.Shapes
import qualified GHC.Float as Float
import Data.Maybe
import qualified Data.Map as Map
import System.Random
import Data.Function
import Data.List

connectionStrength :: String -> String -> Map.Map String (Map.Map String Float) -> Float
connectionStrength xs ys mp = fromJust $ fromJust $ fmap (Map.lookup ys) (Map.lookup xs mp)

allPositions :: [(String,Float)] -> Map.Map String (Map.Map String Float) -> Int -> [[Float]]
allPositions xs mp n = foldl (minimize xs mp) pos_init [1..150]
  --where pos_init = map(\(x,y) -> [Float.double2Float x, Float.double2Float y]) $ map unp2 (trailVertices $ regPoly (length xs) 1)
  --where pos_init = [[0,1],[1,0],[2,0]]
  where pos_init = tupleToList $ zip (take (length xs) $ randomRs(0,2) (mkStdGen 6)) (take (length xs) $ randomRs(0,2) (mkStdGen 9))
        tupleToList = map (\(x,y) -> [x,y])

allPositionsTraj :: [(String,Float)] -> Map.Map String (Map.Map String Float) -> Int -> [[[Float]]]
allPositionsTraj xs mp n = scanl (minimize xs mp) pos_init [1..140]
  where pos_init = tupleToList $ zip (take (length xs) $ randomRs(0.0,2.0) (mkStdGen 6)) (take (length xs) $ randomRs(0.0,2.0) (mkStdGen 9))
  --where pos_init = map(\(x,y) -> [Float.double2Float x, Float.double2Float y]) $ map unp2 (trailVertices $ regPoly (length xs) 1)
        tupleToList = map (\(x,y) -> [x,y])


minimize :: [(String,Float)] -> Map.Map String (Map.Map String Float) -> [[Float]] -> Int -> [[Float]]
minimize xs mp pos_init n = zipWith (\[x,y] [z,q] -> [x+0.03*z, y+0.03*q] ) pos_init derivatives
  where derivatives = derivs pos_init xs mp


derivs :: [[Float]] -> [(String,Float)] -> Map.Map String (Map.Map String Float) -> [[Float]]
derivs pos_init xs mp = map (\(_,x,_) -> x) $ forces triple (Map.fromList xs) mp
    where triple = map (\((a,b),(c,d)) -> (a,b,c)) $ zip (zip pos_init $ cycle [[0,0]]) xs
  --where toList = map (\(x,y) ->[x,y])

forces :: [([Float],[Float],String)] -> Map.Map String Float -> Map.Map String (Map.Map String Float) -> [([Float],[Float],String)]
forces [] xs mp = []
forces (x:[]) xs mp = [x]
forces (x:y:[]) xs mp = [pairTot x y xs mp, pairTot y x xs mp]
forces (y:ys) xs mp = (foldl(\q x -> pairTot q x xs mp) y ys) : (map(\q ->  pairTot q y xs mp) $ forces ys xs mp)

neg :: ([Float],[Float],String) -> ([Float],[Float],String)
neg (a,xs,b) = (a, map((-1)*) xs, b)

sumLists :: (Num a) => [[a]] -> [a]
sumLists (x:[]) = x
sumLists (x:y:[]) = zipWith (+) x y
sumLists (x:xs) = zipWith (+) x $ sumLists xs


pairTot :: ([Float],[Float],String) -> ([Float],[Float],String) -> Map.Map String Float -> Map.Map String (Map.Map String Float) -> ([Float],[Float],String)
pairTot (a,b,c) (d,e,f) xs mp = (a, zipWith (+) b (pair a d w cnxnValue), c)
    where maxStrength = maximum [connectionStrength c l mp | l <- map fst $ Map.toList xs, l /= c]
          mostFrequentWords = map fst $ take 4 $ sortListBySecondElement $ Map.toList xs
          maxConnection = fst $ head $ sortListBySecondElement $ Map.toList $ Map.filterWithKey (\k _ -> k `elem` mostFrequentWords) $ fromJust $ Map.lookup c mp
          w = if ((c `elem` mostFrequentWords) && (f `elem` mostFrequentWords))
              then 1.8
              else 0.5
          cnxnValue
            | ((c `elem` mostFrequentWords) && (f `elem` mostFrequentWords)) = 1
            | f == maxConnection = 1
            | otherwise = 0
          --relativeCnxnValue = (connectionStrength c f mp)/(maxStrength)
          --cnxnValue = 1.0
          --cnxnValue = if relativeCnxnValue > 0.5
          --            then 1.0
          --            else 0.0
          --maxValue = snd $ Map.findMax xs
          --w1 = (fromJust $ Map.lookup c xs)/maxValue
          --w2 = (fromJust $ Map.lookup f xs)/maxValue
          --w = 1.5
          --w = if w1*w2 > 0.5
          --    then 1.5
          --    else 0.5


--pair :: [Float] -> [Float] -> Float -> Float -> [Float]
--pair p1 p2 w c = map( multFactor* ) $ zipWith (-) p1 p2
--  where distance = sqrt $ sum $ map (^2) $ zipWith (-) p1 p2
--        multFactor = -2*c*(distance-w)*distance

pair :: [Float] -> [Float] -> Float -> Float -> [Float]
pair p1 p2 w c = if (distance > 0.3 )
                 then map( multFactor* ) $ zipWith (-) p1 p2
                 else map( (multFactor + (1/distance))* ) $ zipWith (-) p1 p2
  where distance = sqrt $ sum $ map (^2) $ zipWith (-) p1 p2
        multFactor = -2*c*(distance-w)*distance

sortListBySecondElement :: (Ord b) => [(a,b)] -> [(a,b)]
sortListBySecondElement xs = reverse $ sortBy (compare `on` snd) xs
