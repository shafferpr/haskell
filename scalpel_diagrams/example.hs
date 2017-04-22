{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude as Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Arrow
import Text.HTML.Scalpel as Scalpel
import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Function
import Data.List
import qualified GHC.Float as Float
import Data.Char
import System.Random

type Author = String

data Comment
    = TextComment String
    deriving (Show, Eq)
--type Comment = String

takeComment :: Comment -> String
takeComment (TextComment a) = a
--takeComment (ImageComment a b) = b

takeComments :: Maybe [Comment] -> Maybe [String]
takeComments = fmap (map takeComment)


node :: Float -> (String,Float) -> Diagram B
node maxSize (n,x) = Prelude.text (show n) # fontSizeL 0.08 # fc white
      Prelude.<> circle (Float.float2Double (0.2*(x/maxSize))) # fc green # named n


shaft1 = trailFromVertices (map p2 [(0, 0), (1, 0), (1, 0.2), (2, 0.2)])

arrowOpts = with & gaps       .~ small
                 & headLength .~ local 0.000
                    -- & shaftStyle %~ lwL 0.1 .lc blue

--tournament2 takes a list of positions rather than constucting a polygon
tournament2 :: [(Float,Float)] -> [(String,Float)] -> Map.Map String (Map.Map String Float) -> Diagram B
tournament2 listOfPositions xs mp = atPoints (map p2 $ doublePositions) (map (node maxSize) xs)
        # applyAll [connectOutside' (arrowOpts & shaftStyle %~ lwL (Float.float2Double (0.06*(connectionStrength (fst j) (fst k) mp)/maxStrength)) .lc blue) (fst j) (fst k) | j <- xs, k <- xs]
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


example :: [[Float]] -> [(String,Float)] -> Map.Map String (Map.Map String Float) -> Diagram B
example listOfPositions xs mp = tournament2 (listToTuple listOfPositions) xs mp
      where listToTuple = map (\[x,y] -> (x,y))

getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile

main :: IO ()
main = do
  commonWords <- getLines "1000.txt"
  let commonWordsSet = Set.fromList $ map (map toLower) commonWords
  --mapM_ putStrLn commonWords
  xs <- sequence $ map (\x -> takeComments <$> (allComments x)) [1000..1100]
  --xs <- sequence $ map (\x -> fmap takeComments (fmap fromJust $ allComments x)) [12..13]
  --let set1 = Set.fromList $ concat $ map words $ map concat xs
  let fullList = listOfWords xs
  let set1 = Set.fromList $ fullList
  --let uniqueWords = Set.difference set1 $ commonWordsSet
  let reducedList = filterCommonWords fullList commonWordsSet --creates the list of unusual words, preserving duplicates
  let zeroMap = createZeroMap $ nub reducedList --creates the zero map (the map which has two keys for each pair of words and value of zero for each element), removing duplicate words
  let ys = nub $ map listOfWordsInPost xs --creates the list of lists of words in each post, removing duplicate posts
  let zs = map (\x -> filterCommonWords x commonWordsSet ) ys --creates the list of lists of words in each post, removing common words, and removing duplicate words from each post
  let mapOfWords = createMapOfWords zs zeroMap --creates the 2-key map
  let map1 = Map.fromListWith (+) (zip reducedList [0.1,0.1..]) --creates the map that counts the number of appearances of each word
  --mapM_ putStrLn commonWords
  let positions = allPositions (take 25 $ sortListBySecondElement (Map.toList map1) ) mapOfWords 25
  print positions
  mainWith $ example positions ( take 25 $ sortListBySecondElement (Map.toList map1)) mapOfWords
  mapM_ putStrLn ( take 25 $ map fst $ sortListBySecondElement (Map.toList map1))
  --mainWith $ example $ take 5 $ Set.elems set1

allPositions :: [(String,Float)] -> Map.Map String (Map.Map String Float) -> Int -> [[Float]]
allPositions xs mp n = foldl (minimize xs mp) pos_init [1..150]
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
        multFactor = -2*(distance-0.3)/distance

createZeroMap :: [String] -> Map.Map String (Map.Map String Float)
createZeroMap xs = Map.fromList (zip xs (cycle [simpleList]) )
  where simpleList = Map.fromList (zip xs [0,0..])

createMapOfWords :: [[String]] -> Map.Map String (Map.Map String Float) -> Map.Map String (Map.Map String Float)
createMapOfWords xs emptyMap = foldl (foldingFunction) emptyMap xs

foldingFunction :: Map.Map String (Map.Map String Float) -> [String] -> Map.Map String (Map.Map String Float)
--foldingFunction xm xs =  foldl (\acc (a,b) -> Map.adjust (\q -> Map.adjust (+0.01) a q) b acc) xm [(x,y) | x <- xs, y <- xs]
foldingFunction xm xs = foldl (\acc (a,b) -> updateMap a b acc) xm [(x,y) | x <- xs, y <- xs]


updateMap :: String -> String -> Map.Map String (Map.Map String Float) -> Map.Map String (Map.Map String Float)
updateMap word1 word2 xm = Map.adjust (\q -> Map.adjust (+0.00002) word2 q) word1 xm

--Map.adjust (\x -> Map.adjust (+1) "cat" x) "dog" xmap

sortListBySecondElement :: (Ord b) => [(a,b)] -> [(a,b)]
sortListBySecondElement xs = reverse $ sortBy (compare `on` snd) xs

listOfWords :: [Maybe [String]] -> [String]
listOfWords xs = map (map toLower) $ words $ concat $ concat $ concat <$> xs

filterCommonWords :: [String] -> Set.Set String -> [String]
filterCommonWords xs commonWordsSet = containsLetters $ filter (\x -> Set.notMember x commonWordsSet) xs --filter out common words, and only keep words that have letters in them
  where containsLetters ys = filter (\xw -> or $ map(\x -> elem x ['a'..'z']) xw) ys

listOfWordsInPost :: Maybe [String] -> [String]
listOfWordsInPost xs = map (map toLower) $ words $ concat $ concat <$> xs

allComments :: Int -> IO (Maybe [Comment]) --returns an IO action with a type of Maybe [Comment]
allComments a = scrapeURL ("https://www.biostars.org/p/" ++ show a ++ "/") comments
   where
       comments :: Scraper String [Comment]
       comments = chroots ("div"  @: [hasClass "post-body"]) comment
       --comments = chroots ("span" @: ["itemprop" @= "text"] // "p") comment

       comment :: Scraper String Comment
       comment = textComment
       --comment = textComment <|> imageComment

       textComment :: Scraper String Comment
       textComment = do
           --author      <- text $ "div" @: [hasClass "uname"]
           --commentText <- text $ "div"  @: [hasClass "post-body"] // "p"
           commentText <- Scalpel.text $ "span" @: ["itemprop" @= "text"]
           --commentText <- text $ "div"  @: [hasClass "content"]
           --commentText <- text $ anySelector
           return $ TextComment commentText


       --imageComment :: Scraper String Comment
       --imageComment = do
      --     author   <- text       $ "span" @: [hasClass "author"]
      --     imageURL <- attr "src" $ "img"  @: [hasClass "image"]
      --     return $ ImageComment author imageURL
