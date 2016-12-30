import Data.List
import System.Random

--data Node = EmptyNode | Node Int [Int]

--Network = [Node]

--random walk

--randomWalk :: [Int] -> [[Int]]
--randomWalk [] = [[0,0]]
--randomWalk xs = foldl foldingFunction [] xs
--  where foldingFunction xs x = nextStep(head(xs)):xs

stringFunction :: (Eq a) => [a] -> [[a]]
--stringFunction :: [Char] -> [[Char]]
stringFunction st = [filter(==c) st | c <- st]

numberOfEachElement :: (Eq a) => [a] -> [Int]
numberOfEachElement st = map length (stringFunction st)

nextStep :: [Int] -> Int -> [Int]
nextStep [x,y] a
  | a == 0 = [x+1,y]
  | a == 1 = [x,y+1]
  | a == 2 = [x+1, y+1]
  | a == 3 = [x-1,y]
  | a == 4 = [x, y-1]
  | a == 5 = [x-1,y-1]

--nextStep [x,y] = if()

addStep :: [[Int]] -> Int -> [[Int]]
addStep [[]] a = [[0,0]]
addStep xs a = if (nextStep (head xs) a) `elem` xs
      then xs
      else (nextStep (head xs) a) : xs


randomWalk :: Int -> [[Int]]
randomWalk n = foldl addStep [[0,0]] (take n $ randomRs(0,5) (mkStdGen 12))
--randomWalk n = foldl addStep [[0,0]] [1..10]
