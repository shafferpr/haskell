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

buildEmptyNetwork :: Int -> [SimpleNode]
buildEmptyNetwork a = foldl (addUnconnectedNodeToNetwork) [] [1..a]

addUnconnectedNodeToNetwork :: [SimpleNode] -> Int -> [SimpleNode]
addUnconnectedNodeToNetwork xs a = (SimpleNode {identifier = a, connections = []}):xs

addRandomConnectionsToNetwork :: Int -> [SimpleNode] -> [SimpleNode]
addRandomConnectionsToNetwork a xs = foldl addConnectionToNetwork xs (zip (take a $ nub $ randomRs(1,length xs) (mkStdGen 3)) (take a $ nub $ randomRs(1,length xs) (mkStdGen 4)))

--take a $ nub $ randomRs(0,(length xs)-1) (mkStdGen 3)

addNodeToNetwork :: [SimpleNode] -> Int -> [SimpleNode]
--addNodeToNetwork xs a = (SimpleNode {identifier = a, connections = (map identifier xs)}):xs
addNodeToNetwork [] a = [SimpleNode {identifier = a, connections = [2,3]}]
addNodeToNetwork (x:[]) a = (SimpleNode {identifier = a, connections = [1,3]}):[x]
addNodeToNetwork xs@(x:y:[]) a = (SimpleNode {identifier = a, connections = []}):xs
addNodeToNetwork xs a = (SimpleNode {identifier = a, connections = n}):newNetwork
  where n =  map (\x -> (!! x) $ map identifier $ xs) (take 1 $ nub $ randomRs(0,(length xs)-1) (mkStdGen 3)) -- nub filters duplicates
        newNetwork = foldl (addConnectionToInternalNode a) xs n

addConnectionToNetwork :: [SimpleNode] -> (Int,Int) -> [SimpleNode]
addConnectionToNetwork xs (a,b) = addConnectionToInternalNode a newNetwork b
  where newNetwork = addConnectionToInternalNode b xs a

--this function should really return Maybe Int because two nodes may not be connected
distanceBetweenNodes2 :: [SimpleNode] -> Int -> [Int] -> Maybe Int
distanceBetweenNodes2 xs a b
  | a `elem` b = Just 0
  | a `elem` firstConnections = Just 1
  | null firstConnections = Nothing
  | otherwise = fmap (1+) (distanceBetweenNodes2 xs a (b ++ firstConnections))
  where firstConnections = nub $ filter(\x -> not (x `elem` b)) $ concat $ map (conxns xs) b
        --secondConnections = nub $ filter(\x -> not (x `elem` firstConnections)) $ concat $ map (conxns xs) firstConnections


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
addConnectionToInternalNode a xs n = (filter (\x -> identifier x > n) xs) ++ [SimpleNode {identifier = n, connections = (connections $ head $ filter (\x -> identifier x == n) xs) ++ [a]}] ++ (filter (\x -> identifier x < n) xs)

stringFunction :: (Eq a) => [a] -> [[a]]
--stringFunction :: [Char] -> [[Char]]
stringFunction st = [filter(==c) st | c <- st]

numberOfEachElement :: (Eq a) => [a] -> [Int]
numberOfEachElement st = map length (stringFunction st)

--derivative :: Int -> Int -> Floating
--derivative x y =  map (\z -> directDerivative x z * derivative z y )
