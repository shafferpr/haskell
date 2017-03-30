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


main :: IO ()
main = do
  xs <- sequence $ map (\x -> takeComments <$> (allComments x)) [13]
  --xs <- sequence $ map (\x -> fmap takeComments (fmap fromJust $ allComments x)) [12..13]
  --let set1 = Set.fromList $ concat $ map words $ map concat xs
  let fullList = listOfWords xs
  let set1 = Set.fromList $ fullList
  let map1 = Map.fromListWith (+) (zip fullList [0.1,0.1..])
  mainWith $ example $ take 6 $ sortListBySecondElement (Map.toList map1)
  --mainWith $ example $ take 5 $ Set.elems set1

sortListBySecondElement :: (Ord b) => [(a,b)] -> [(a,b)]
sortListBySecondElement xs = reverse $ sortBy (compare `on` snd) xs

listOfWords :: [Maybe [String]] -> [String]
listOfWords xs = words $ concat $ concat $ concat <$> xs

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
