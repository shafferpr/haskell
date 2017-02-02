import Data.List
import System.Random


--data Node = EmptyNode | Node Int [Int]
--data Node = Node {identifier :: Int, totalValue :: Float, connections :: [Int], connectionWeights :: [Float], connectionDerivatives :: [Float]}
data SimpleNode = SimpleNode {identifier :: Int, connections :: [Int]} deriving (Show)
--type Layer = [Node]
--type Network = [Layer]
--Network = [Node]

--random walk

--randomWalk :: [Int] -> [[Int]]
--randomWalk [] = [[0,0]]
--randomWalk xs = foldl foldingFunction [] xs
--  where foldingFunction xs x = nextStep(head(xs)):xs

buildNetwork :: Int -> [SimpleNode]
buildNetwork a = foldl (addNodeToNetwork) [] [1..a]
--buildNetwork a = foldl (addNodeToNetwork) [] (take a randomRs(0,1.0) (mkStdGen 1))

addNodeToNetwork :: [SimpleNode] -> Int -> [SimpleNode]
--addNodeToNetwork xs a = (SimpleNode {identifier = a, connections = (map identifier xs)}):xs
addNodeToNetwork [] a = [SimpleNode {identifier = a, connections = [2,3]}]
addNodeToNetwork (x:[]) a = (SimpleNode {identifier = a, connections = [1,3]}):[x]
addNodeToNetwork xs@(x:y:[]) a = (SimpleNode {identifier = a, connections = [1,2]}):xs
addNodeToNetwork xs a = (SimpleNode {identifier = a, connections = n}):newNetwork
  where n =  map (\x -> (!! x) $ map identifier $ xs) (take 3 $ nub $ randomRs(0,(length xs)-1) (mkStdGen 3)) -- nub filters duplicates
        newNetwork = foldl (addConnectionToInternalNode a) xs n


--distanceBetweenNodes :: [SimpleNode] -> Int -> Int -> Int
--distanceBetweenNodes xs a a = 0
--distanceBetweenNodes xs a b = if (a `elem` conxns)
--      then 1
--      else 1 + (minimum $ map (distanceBetweenNodes xs a) conxns)
--      where conxns = connections $ head $ filter (\x -> identifier x == b) xs

distanceBetweenNodes :: [SimpleNode] -> Int -> Int -> [Int] -> Int
distanceBetweenNodes xs a b firstConnections
  | a == b = 0
  | b `elem` firstConnections = 1
  | otherwise = 1 + distanceBetweenNodes xs a b secondConnections
  -- | otherwise = 1 + (minimum' $ map (distanceBetweenNodes xs a) connectedNodes)
  where secondConnections = concat $ map (conxns xs) firstConnections


conxns :: [SimpleNode] -> Int -> [Int]
conxns xs b = connections $ head $ filter (\x -> identifier x == b) xs

minimum' :: [Int] -> Int
minimum' xs
  | 0 `elem` xs = 0
  | 1 `elem` xs = 1
  | 2 `elem` xs = 2
  | otherwise = minimum xs
--how to make probability of connection to well connected nodes higher

addConnectionToInternalNode :: Int -> [SimpleNode] -> Int -> [SimpleNode]
--addConnectionToInternalNode xs a n = (filter (\x -> identifier x > n) xs) : (SimpleNode {identifier = n, connections = connections $ head $ filter (\x -> identifier x == n) xs}) : (filter (\x -> identifier x < n) xs)
addConnectionToInternalNode a xs n = (filter (\x -> identifier x > n) xs) ++ [SimpleNode {identifier = n, connections = (connections $ head $ filter (\x -> identifier x == n) xs) ++ [a]}] ++ (filter (\x -> identifier x < n) xs)

stringFunction :: (Eq a) => [a] -> [[a]]
--stringFunction :: [Char] -> [[Char]]
stringFunction st = [filter(==c) st | c <- st]

numberOfEachElement :: (Eq a) => [a] -> [Int]
numberOfEachElement st = map length (stringFunction st)

--derivative :: Int -> Int -> Floating
--derivative x y =  map (\z -> directDerivative x z * derivative z y )
