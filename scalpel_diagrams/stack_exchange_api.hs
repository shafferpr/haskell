{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


import Diagrams.Backend.SVG
import qualified Diagrams.Size as Size
import qualified Diagrams.TwoD.Size as TwoDSize
import Data.Aeson as Aeson
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Function
import Control.Concurrent
import qualified GHC.Float as Float
import DiagramStyle
import PositionNodes
import WordListFunctions
-- | Type of conversion, analogous to the JSON data obtainable
--   from the URL.

data Answer =
  Answer {
          answer_id :: Int
        , answer_body :: String
        --, owner :: User
         } deriving (Show, Generic)


data Question =
  Question {
            question_id :: Int
          , answer_count :: Int
          , tags :: [String]
          , answers :: Maybe [Answer]
          , body :: String
          , title :: String
            } deriving (Show, Generic)

data APIQuery =
  APIQuery {
            items :: [Question]
          , page :: Int
            } deriving (Show, Generic)

-- Automatically generated instances
instance FromJSON Question
instance ToJSON Question

instance FromJSON APIQuery
instance ToJSON APIQuery

instance ToJSON Answer where
  toJSON (Answer answer_id answer_body)=
    object [ "answer_id" Aeson..= answer_id
           , "body" Aeson..= answer_body
            ]

instance FromJSON Answer where
  parseJSON (Object v) =
        Answer <$> v .: "answer_id"
               <*> v .: "body"
  parseJSON _ = mzero


jsonURL :: Int -> String
jsonURL a = "https://api.stackexchange.com/2.2/questions?page=" ++ show a ++ "&pagesize=100&order=desc&sort=activity&site=dba&filter=!DER*bZIt1fz(_v-)6.c3jG15.0WMnEJGtH3Tl.9kKgRlWn(TVae"

getJSON :: Int -> IO B.ByteString
getJSON a = simpleHttp $ jsonURL a

getPage :: Int -> IO (Maybe [Question])
getPage a = (fmap items) <$> ((decode <$> getJSON a) :: IO (Maybe APIQuery))

getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile

main :: IO [()]
main = sequence $ map createFigure [1..2]

  --c <- (decode <$> getJSON 2) :: IO (Maybe APIQuery)
  --d <- (decode $ getJSON) :: IO (Either String APIQuery)
  --print ys

createFigure :: Int -> IO ()
createFigure n = do
  commonWords <- getLines "1000.txt"
  let commonWordsSet = Set.fromList $ map (map toLower) commonWords
  --let m = 1
  xs <- sequence $ map (\x -> fromJust <$> getPage x) [(30*n)..(30*n+29)]
  let allQuestions = concat xs
  let ys = listOfQuestions allQuestions
  let zs = listOfQuestionsAndAnswers allQuestions
  let wordsInPosts = map (map (map toLower)) $ map words zs --creates the list of list words in each thread
  let wordsInPostsFiltered = map (\x -> filterCommonWords x commonWordsSet ) wordsInPosts
  let fullList = concat wordsInPosts
  let reducedList = filterCommonWords fullList commonWordsSet
  let zeroMap = createZeroMap $ nub reducedList
  let mapOfWords = createMapOfWords wordsInPostsFiltered zeroMap
  let map1 = Map.fromListWith (+) (zip reducedList [0.1,0.1..])
  let wordList = take 13 $ sortListBySecondElement (Map.toList map1)
  let subMap = filterMap mapOfWords $ map fst wordList
  let positions = allPositions wordList subMap 13
  --mainWith $ example ( take 25 $ sortListBySecondElement (Map.toList map1)) mapOfWords
  let dimensions = (TwoDSize.mkSizeSpec2D (Just 400) (Just 400))
  let positionsWithIndex = zip positions [1..]
  renderSVG ("SQl" ++ show n ++ ".svg") dimensions (example2 positions wordList subMap)
  --mainWith $ example2 positions  mapOfWords
  mapM_ putStrLn ( take 25 $ map fst $ sortListBySecondElement (Map.toList map1))


sortListBySecondElement :: (Ord b) => [(a,b)] -> [(a,b)]
sortListBySecondElement xs = reverse $ sortBy (compare `on` snd) xs


filterCommonWords :: [String] -> Set.Set String -> [String]
filterCommonWords xs commonWordsSet = notWord $ containsLetters $ filter (\x -> Set.notMember x commonWordsSet) xs --filter out common words, and only keep words that have letters in them
  where containsLetters ys = filter (\xw -> or $ map(\x -> elem x ['a'..'z']) xw) ys --ensures that there is at least one letter
        notWord ys = filter (\xw -> not ('<' `elem` xw)) ys

listOfQuestions :: [Question] -> [String]
listOfQuestions xs = fmap (body) xs

listOfQuestionsAndAnswers :: [Question] -> [String]
listOfQuestionsAndAnswers xs = map (\x -> (body x) ++ (qAnswers x)) xs


qAnswers :: Question -> [Char]
qAnswers x
  | qanswers x == Nothing = []
  | otherwise = fromJust $ qanswers x
  where qanswers x = (concat <$> map answer_body <$> answers x)

  --case d of
    --Left err -> putStrLn err
    --Right ps -> print ps
