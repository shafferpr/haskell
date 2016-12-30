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
nextStep[x,y,z] a
  | a == 0 = [x+1,y,z]
  | a == 1 = [x,y+1,z]
  | a == 2 = [x,y,z+1]
  | a == 3 = [x-1,y,z]
  | a == 4 = [x,y-1,z]
  | a == 5 = [x,y,z-1]
  | a == 6 = [x+1,y+1,z]
  | a == 7 = [x,y+1,z+1]
  | a == 8 = [x+1,y,z+1]
  | a == 9 = [x-1,y-1,z]
  | a == 10 = [x,y-1,z-1]
  | a == 11 = [x-1,y,z-1]
  | a == 12 = [x+1,y+1,z+1]
  | a == 13 = [x-1,y-1,z-1]
--nextStep [x,y] = if()

addStep :: Int -> [[Int]] -> Int -> [[Int]]
addStep 2 [[]] a = [[0,0]]
addStep 3 [[]] a = [[0,0,0]]
addStep b xs a = if (nextStep (head xs) a) `elem` xs
      then xs
      else (nextStep (head xs) a) : xs


randomWalk :: Int -> Int -> Int -> Int -> [[Int]]
randomWalk n d r m = foldl (addStep d) [[]] (take n $ randomRs(0,r) (mkStdGen m))
--randomWalk n = foldl addStep [[0,0]] [1..10]

vectorLength :: Floating a => [Int] -> a
vectorLength xs = sqrt $ sum $ map (^2) $ map fromIntegral xs

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

main = do
  putStrLn "what is the dimension?"
  d <- getLine
  putStrLn "how many moves are possible for each step?"
  r <- getLine
  let avLength = average $ map (vectorLength) $ map (head . randomWalk 100 (read d :: Int)  (read r :: Int)) [1..500]
  putStrLn $ show avLength
