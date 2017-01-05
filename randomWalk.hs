import Data.List
import System.Random
import System.IO
import Text.Printf
import Control.Monad
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

addStep :: Bool -> [[Int]] -> Int -> [[Int]]
--addStep 2 [[]] a = [[0,0]]
--addStep 3 [[]] a = [[0,0,0]]
addStep True xs a = if (nextStep (head xs) a) `elem` xs
      then xs
      else (nextStep (head xs) a) : xs
addStep False xs a = (nextStep (head xs) a) : xs

randomWalk :: String -> Int -> Int -> Int -> Int -> [[Int]]
randomWalk "Yes" n 2 r m = foldl (addStep True) [[0,0]] (take n $ randomRs(0,r) (mkStdGen m))
randomWalk "No" n 2 r m = foldl (addStep False) [[0,0]] (take n $ randomRs(0,r) (mkStdGen m))
randomWalk "Yes" n 3 r m = foldl (addStep True) [[0,0,0]] (take n $ randomRs(0,r) (mkStdGen m))
randomWalk "No" n 3 r m = foldl (addStep False) [[0,0,0]] (take n $ randomRs(0,r) (mkStdGen m))
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
  putStrLn "is the random walk self avoiding? Answer Yes or No"
  randomWalkSelfAvoiding <- getLine
  putStrLn "what would you like to call the output file?"
  outputFileName <- getLine
  --let avLength = map (\x -> average $ map (vectorLength) $ map (head . randomWalk randomWalkSelfAvoiding x (read d :: Int)  (read r :: Int)) [1..1000]) [10..100]
  let avlength = map (\y -> average $ map vectorLength $ map (!! y) $ filter (\xs -> length xs > 101) $ map (\x -> reverse (randomWalk randomWalkSelfAvoiding 1000 (read d :: Int) (read r :: Int) x)) [1..1000]) [0..100]
  --sequence (map print avlength)
  outh <- openFile outputFileName WriteMode
  hPutStr outh $ unlines $ map show avlength
  hClose outh
  --let x = [0.0..20.0]
  --writeDat "sa_2d.txt" avlength 3
  --putStrLn $ show avLength
