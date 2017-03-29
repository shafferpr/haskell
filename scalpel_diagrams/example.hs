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

node :: String -> Diagram B
node n = Prelude.text (show n) # fontSizeL 0.15 # fc white
      Prelude.<> circle 0.2 # fc green # named n

arrowOpts = with & gaps       .~ small
                  & headLength .~ local 0.0

tournament :: [String] -> Diagram B
tournament xs = atPoints (trailVertices $ regPoly (length xs) 1) (map node xs)
  -- # applyAll [connectOutside' arrowOpts j k | j <- xs, k <- xs]

example :: [String] -> Diagram B
example xs = tournament xs


main :: IO ()
main = do
  xs <- sequence $ map (\x -> takeComments <$> (allComments x)) [13]
  --xs <- sequence $ map (\x -> fmap takeComments (fmap fromJust $ allComments x)) [12..13]
  --let set1 = Set.fromList $ concat $ map words $ map concat xs
  let set1 = Set.fromList $ listOfWords xs
  mainWith $ example $ take 5 $ Set.elems set1

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
