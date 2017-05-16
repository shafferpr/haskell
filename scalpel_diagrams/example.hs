{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Backend.SVG
import qualified Diagrams.Size as Size
import qualified Diagrams.TwoD.Size as TwoDSize
import Text.HTML.Scalpel as Scalpel
import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Function
import Data.List
import Data.Char
import Text.Printf
import System.IO
import PositionNodes
import DiagramStyle
import WordListFunctions

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


getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile

main :: IO [()]
main = sequence $ map createFigure [1..20]


createFigure :: Int -> IO ()
createFigure n = do
  commonWords <- getLines "1000.txt"
  let commonWordsSet = Set.fromList $ map (map toLower) commonWords
  --mapM_ putStrLn commonWords
  xs <- sequence $ map (\x -> takeComments <$> (allComments x)) [(n*1000)..(n*1000+999)]
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
  let wordList = take 13 $ sortListBySecondElement (Map.toList map1)
  let subMap = filterMap mapOfWords $ map fst wordList
  --outh <- openFile "listofwords3.txt" WriteMode
  --hPrint outh $ wordList
  --outh <- openFile "mapofwords3.txt" WriteMode
  --hPutStr outh $ show $ map(\(x,y) -> (x, Map.toList y)) $ Map.toList subMap
  let positions = allPositions wordList subMap 13
  let dimensions = (TwoDSize.mkSizeSpec2D (Just 400) (Just 400))
  --let positionsWithIndex = zip positions [1..]
  renderSVG ("Biostars" ++ show n ++ ".svg") dimensions (example2 positions wordList subMap)
  --a <- sequence $ map (\(x,i) -> renderSVG ("TEST" ++ show i ++ ".svg") dimensions (example2 x wordList subMap)) positionsWithIndex
  --mainWith $ example2 positions ( take 10 $ sortListBySecondElement (Map.toList map1)) mapOfWords
  --mapM_ putStrLn ( take 100 $ sortListBySecondElement (Map.toList map1))
  mapM_ putStrLn ( take 20 $ map fst $ sortListBySecondElement (Map.toList map1))
  --mainWith $ example $ take 5 $ Set.elems set1

sortListBySecondElement :: (Ord b) => [(a,b)] -> [(a,b)]
sortListBySecondElement xs = reverse $ sortBy (compare `on` snd) xs

listOfWords :: [Maybe [String]] -> [String]
listOfWords xs = map (filter ( /= '\"') . map toLower) $ words $ concat $ concat $ concat <$> xs

filterCommonWords :: [String] -> Set.Set String -> [String]
filterCommonWords xs commonWordsSet = containsLetters $ filter (\x -> Set.notMember x commonWordsSet) xs --filter out common words, and only keep words that have letters in them
  where containsLetters ys = filter (\xw -> or $ map(\x -> elem x ['a'..'z']) xw) ys

listOfWordsInPost :: Maybe [String] -> [String]
listOfWordsInPost xs = map (filter ( /= '\"') . map toLower) $ words $ concat $ concat <$> xs

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
