module DiagramStyle
(
example,
example2
)where

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude as Prelude
import Data.Active as Active
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG
import Diagrams.TwoD.Arrow

import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified GHC.Float as Float



node :: Float -> (String,Float) -> Diagram B
node maxSize (n,x) = Prelude.text (show n) # fontSizeL 0.08 # fc white
      Prelude.<> circle (Float.float2Double (0.35*(x/maxSize))) # fc green # named n



shaft1 = trailFromVertices (map p2 [(0, 0), (1, 0), (1, 0.2), (2, 0.2)])

arrowOpts = with & gaps       .~ small
                 & headLength .~ local 0.000
                    -- & shaftStyle %~ lwL 0.1 .lc blue

--tournament2 takes a list of positions rather than constucting a polygon
tournament2 :: [(Float,Float)] -> [(String,Float)] -> Map.Map String (Map.Map String Float) -> Int -> Diagram B
tournament2 listOfPositions xs mp n = Prelude.text (show n) # fontSizeL 0.13 # translate (r2 (1,-0.3)) Prelude.<> atPoints (map p2 $ doublePositions) (map (node maxSize) xs)
        # applyAll [connectOutside' (arrowOpts & shaftStyle %~ lwL (Float.float2Double (0.05*(connectionStrength (fst j) (fst k) mp)/maxStrength)) .lc blue) (fst j) (fst k) | j <- xs, k <- xs]
          where maxStrength = maximum [connectionStrength (fst j) (fst k) mp | j <- xs, k <- xs]
                maxSize = maximum (map snd xs)
                doublePositions = map(\(x,y) -> (Float.float2Double x, Float.float2Double y)) listOfPositions


tournament :: [(String,Float)] -> Map.Map String (Map.Map String Float) -> Diagram B
tournament xs mp = atPoints (trailVertices $ regPoly (length xs) 1) (map (node maxSize) xs)
    # applyAll [connectOutside' (arrowOpts & shaftStyle %~ lwL (Float.float2Double (0.06*(connectionStrength (fst j) (fst k) mp)/maxStrength)) .lc blue) (fst j) (fst k) | j <- xs, k <- xs]
      where maxStrength = maximum [connectionStrength (fst j) (fst k) mp | j <- xs, k <- xs]
            maxSize = maximum (map snd xs)
   -- # applyAll [connectOutside' (with & gaps  .~ small & headLength .~ local 0.0 & shaftStyle lw 0.1) j k | j <- xs, k <- xs]

connectionStrength :: String -> String -> Map.Map String (Map.Map String Float) -> Float
connectionStrength xs ys mp = fromJust $ fromJust $ fmap (Map.lookup ys) (Map.lookup xs mp)


example2 :: [[Float]] -> [(String,Float)] -> Map.Map String (Map.Map String Float) -> Int -> Diagram B
example2 listOfPositions xs mp n = tournament2 (listToTuple listOfPositions) xs mp n
      where listToTuple = map (\[x,y] -> (x,y))

example :: [(String,Float)] -> Map.Map String (Map.Map String Float) -> Diagram B
example xs mp = tournament xs mp
