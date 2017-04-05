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

tournament :: [(String,Float)] -> Map.Map String (Map.Map String Float) -> Diagram B
tournament xs mp = atPoints (trailVertices $ regPoly (length xs) 1) (map (node maxSize) xs)
    # applyAll [connectOutside' (arrowOpts & shaftStyle %~ lwL (Float.float2Double (0.06*(connectionStrength (fst j) (fst k) mp)/maxStrength)) .lc blue) (fst j) (fst k) | j <- xs, k <- xs]
      where maxStrength = maximum [connectionStrength (fst j) (fst k) mp | j <- xs, k <- xs]
            maxSize = maximum (map snd xs)
   -- # applyAll [connectOutside' (with & gaps  .~ small & headLength .~ local 0.0 & shaftStyle lw 0.1) j k | j <- xs, k <- xs]

connectionStrength :: String -> String -> Map.Map String (Map.Map String Float) -> Float
connectionStrength xs ys mp = fromJust $ fromJust $ fmap (Map.lookup ys) (Map.lookup xs mp)


example :: [(String,Float)] -> Map.Map String (Map.Map String Float) -> Diagram B
example xs mp = tournament xs mp

getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile

getComment :: Int -> String -> IO (Maybe [String])
getComment x xs = takeComments <$> (allPosts x xs)

main :: IO ()
main = do
  commonWords <- getLines "1000.txt"
  let commonWordsSet = Set.fromList $ map (map toLower) commonWords
  --mapM_ putStrLn commonWords
  questions <- sequence $ map (\x -> getComment x "question") [50..250]
  answers <- sequence $ map (\x -> getComment x "answer") [50..250]
  let xs = zipWith (\x y -> (++) <$> x <*> y) questions answers
  --xs <- sequence $ map (\x -> (++) <$> (getComment x "question" ) <*> ( getComment x "answer")) [50..200]
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
  mainWith $ example ( take 7 $ sortListBySecondElement (Map.toList map1)) mapOfWords
  mapM_ putStrLn ( take 25 $ map fst $ sortListBySecondElement (Map.toList map1))
  --mainWith $ example $ take 5 $ Set.elems set1

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

allPosts :: Int -> String -> IO (Maybe [Comment]) --returns an IO action with a type of Maybe [Comment]
allPosts a xs = scrapeURL ("https://physics.stackexchange.com/questions/" ++ show a ++ "/") answerComments
   where
       answerComments :: Scraper String [Comment]
       answerComments =  (chroots ("div"  @: [hasClass xs]) comment)
       --comments = chroots (AnyTag  @: anySelector) comment

       questionComments :: Scraper String [Comment]
       questionComments =  chroots ("div"  @: [hasClass "question"]) comment

       comment :: Scraper String Comment
       comment = textComment

       textComment :: Scraper String Comment
       textComment = do
           --author      <- text $ "div" @: [hasClass "uname"]
           --commentText <- text $ "div"  @: [hasClass "post-body"] // "p"
           commentText <- Scalpel.text $ "div" @: ["itemprop" @= "text"]
           --commentText <- text $ "div"  @: [hasClass "content"]
           --commentText <- text $ anySelector
           return $ TextComment commentText


       --imageComment :: Scraper String Comment
       --imageComment = do
      --     author   <- text       $ "span" @: [hasClass "author"]
      --     imageURL <- attr "src" $ "img"  @: [hasClass "image"]
      --     return $ ImageComment author imageURL
