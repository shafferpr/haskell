--checkListForDuplicates :: [String] -> [String]

removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]


--checkStringForDuplicates :: String -> String
--checkStringForDuplicates st = [c | c <- st,

--markDups :: [Char] -> [Char]
--markDups = dupsHelp[]
--where dupsHelp c [] = c
--      dupsHelp c (x:xs)
--      	  | x `elem` c = dupsHelp (c ++ "_") xs
--	  | otherwise = dupshelp (c ++ [x]) xs

stringFunction :: (Eq a) => [a] -> [[a]]
--stringFunction :: [Char] -> [[Char]]
stringFunction st = [filter(==c) st | c <- st]

numberOfEachElement :: (Eq a) => [a] -> [Int]
numberOfEachElement st = map length (stringFunction st)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--shows how to use literals (like [7]) when pattern matching with a list
firstElementIsSeven :: [Int] -> Bool
firstElementIsSeven [] = False
firstElementIsSeven [7] = True
firstElementIsSeven [7,_] = True --this line will only match a list of two elements
firstElementIsSeven (x:xs)
    | x == 7 = True
    | otherwise = False


takeOddElements :: [a] -> [a]
takeOddElements [] = []
takeOddElements (x:[]) = [x]
takeOddElements (x:y:ys) = x : takeOddElements (ys)


takeEvenElements :: [a] -> [a]
takeEvenElements [] = []
takeEvenElements (x:[]) = []
takeEvenElements (x:y:ys) = y : takeEvenElements (ys)

filterPrime :: [Int] -> [Int]
filterPrime [] = []
filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]

--primes = filterPrime [2..]
--  where filterPrime (p:xs) =
--          p : filterPrime [x | x<- xs, x `mod` p /= 0]

--function that produces a chain of collatz numbers

chain :: (Integral a) =>  a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 +1)


--numLongChains :: Int -> Int
--numLongChains a = length ( filter isLong (map chain [1..a]))
--    where isLong xs = length xs > 15

numLongChains :: Int -> Int
numLongChains a = length ( filter (\xs -> length xs > 15) (map chain [1..a]))

numChainsLongerThanInput :: Int -> Int
numChainsLongerThanInput a = length ( filter (\xs -> length xs > head xs) (map chain [1..a]))

--product' :: (Num a) => [a] -> a
--product' xs = foldl (\acc x -> x*acc ) 1 xs

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

interaction :: (Num a) => [a] -> a
interaction (x:[]) = 0
interaction (x:y:[]) = (x-y)*(x-y)
interaction (y:xs) = interaction(xs) + foldl (\acc x -> acc + (x-y)*(x-y)) 0 xs

--alternative version of interaction which uses a map instead of a foldl

interaction2 :: (Num a, Ord a) => [a] -> a
interaction2 (x:[]) = 0
interaction2 (x:y:[]) = pairwiseInteraction x y
--interaction2 (y:xs) = interaction(xs) + sum (map (\x -> pairwiseInteraction x y) xs)
interaction2 (y:xs) = interaction2(xs) + sum (map (pairwiseInteraction y) xs)

pairwiseInteraction :: (Num a, Ord a) => a -> a -> a
pairwiseInteraction x y
    | abs (x-y) >= 10 = (20 - abs (x-y))*(20 - abs (x-y))
    | otherwise = (x-y)*(x-y)

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

--import Data.List

--solveRPN :: (Num a, Read a) => String -> a
--solveRPN = head . foldl foldingFunction [] . words-
--    where    foldingFunction (x:y:ys) "*" = (x * y):ys
--    	       foldingFunction (x:y:ys) "+" = (x + y):ys
--	           foldingFunction (x:y:ys) "-" = (y - x):ys
--	           foldingFunction xs numberString = read numberString:xs -- this is a new pattern match for me, matching to a numberString


--in foldingFunction we are using pattern matching, the first pattern matches the accumulator, the second pattern matches the current element of the list in the fold

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right
