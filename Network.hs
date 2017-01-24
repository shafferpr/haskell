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
addNodeToNetwork [] a = [SimpleNode {identifier = a, connections = [2]}]
addNodeToNetwork (x:[]) a = (SimpleNode {identifier = a, connections = [1]}):[x]
addNodeToNetwork xs a = (SimpleNode {identifier = a, connections = n}):newNetwork
  where n =  map (\x -> (!! x) $ map identifier $ xs) (take 2 $ randomRs(0,(length xs)-1) (mkStdGen 1))
        newNetwork = foldl (addConnectionToInternalNode a) xs n

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
