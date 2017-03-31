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

node :: (String,Float) -> Diagram B
node (n,x) = Prelude.text (show n) # fontSizeL 0.15 # fc white
      Prelude.<> circle (Float.float2Double x) # fc green # named n

arrowOpts = with & gaps       .~ small
                  & headLength .~ local 0.0

tournament :: [(String,Float)] -> Diagram B
tournament xs = atPoints (trailVertices $ regPoly (length xs) 1) (map node xs)
  -- # applyAll [connectOutside' arrowOpts j k | j <- xs, k <- xs]

example :: [(String,Float)] -> Diagram B
example xs = tournament xs

getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile

main :: IO ()
main = do
  commonWords <- getLines "1000.txt"
  let commonWordsSet = Set.fromList commonWords
  --mapM_ putStrLn commonWords
  xs <- sequence $ map (\x -> takeComments <$> (allComments x)) [13,14]
  --xs <- sequence $ map (\x -> fmap takeComments (fmap fromJust $ allComments x)) [12..13]
  --let set1 = Set.fromList $ concat $ map words $ map concat xs
  let fullList = listOfWords xs
  let set1 = Set.fromList $ fullList
  --let uniqueWords = Set.difference set1 $ commonWordsSet
  let reducedList = nub $ filterCommonWords fullList commonWordsSet
  let zeroMap = createZeroMap reducedList
  let ys = map listOfWordsInPost xs
  let zs = map (\x -> filterCommonWords x commonWordsSet ) ys
  --let mapOfWords = createMapOfWords zs zeroMap
  let map1 = Map.fromListWith (+) (zip reducedList [0.1,0.1..])
  mainWith $ example $ take 8 $ sortListBySecondElement (Map.toList map1)
  --mainWith $ example $ take 5 $ Set.elems set1

createZeroMap :: [String] -> Map.Map String (Map.Map String Float)
createZeroMap xs = Map.fromList (zip xs (cycle [simpleList]) )
  where simpleList = Map.fromList (zip xs [0,0..])

createMapOfWords :: [[String]] -> Map.Map String (Map.Map String Float) -> Map.Map String (Map.Map String Float)
createMapOfWords xs emptyMap = foldl (foldingFunction) emptyMap xs

foldingFunction :: Map.Map String (Map.Map String Float) -> [String] -> Map.Map String (Map.Map String Float)
foldingFunction xm xs =  foldl (\acc (a,b) -> Map.adjust (\q -> Map.adjust (+0.1) a q) b acc) xm [(x,y) | x <- xs, y <- xs]

--Map.adjust (\x -> Map.adjust (+1) "cat" x) "dog" xmap

sortListBySecondElement :: (Ord b) => [(a,b)] -> [(a,b)]
sortListBySecondElement xs = reverse $ sortBy (compare `on` snd) xs

listOfWords :: [Maybe [String]] -> [String]
listOfWords xs = words $ concat $ concat $ concat <$> xs

filterCommonWords :: [String] -> Set.Set String -> [String]
filterCommonWords xs commonWordsSet = filter (\x -> Set.notMember x commonWordsSet) xs

listOfWordsInPost :: Maybe [String] -> [String]
listOfWordsInPost xs = words $ concat $ concat <$> xs

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
