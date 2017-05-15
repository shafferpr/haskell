{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


import Diagrams.Backend.SVG.CmdLine
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
jsonURL a = "https://api.stackexchange.com/2.2/questions?page=" ++ show a ++ "&pagesize=10&order=desc&sort=activity&site=dba&filter=!DER*bZIt1fz(_v-)6.c3jG15.0WMnEJGtH3Tl.9kKgRlWn(TVae"

getJSON :: Int -> IO B.ByteString
getJSON a = simpleHttp $ jsonURL a

getPage :: Int -> IO (Maybe [Question])
getPage a = (fmap items) <$> ((decode <$> getJSON a) :: IO (Maybe APIQuery))

getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile

main :: IO ()
main = do
  commonWords <- getLines "1000.txt"
  let commonWordsSet = Set.fromList $ map (map toLower) commonWords
  xs <- sequence $ map (\x -> fromJust <$> getPage x) [1..20]
  let allQuestions = concat xs
  let ys = listOfQuestions allQuestions
  let zs = listOfQuestionsAndAnswers allQuestions
  let wordsInPosts = map (map (map toLower)) $ map words zs --creates the list of list words in each thread
  let fullList = concat wordsInPosts
  let reducedList = filterCommonWords fullList commonWordsSet
  let zeroMap = createZeroMap $ nub reducedList
  let mapOfWords = createMapOfWords wordsInPosts zeroMap
  let map1 = Map.fromListWith (+) (zip reducedList [0.1,0.1..])
  let positions = allPositions (take 35 $ sortListBySecondElement (Map.toList map1) ) mapOfWords 35
  --mainWith $ example ( take 25 $ sortListBySecondElement (Map.toList map1)) mapOfWords
  mainWith $ example2 positions ( take 35 $ sortListBySecondElement (Map.toList map1)) mapOfWords
  mapM_ putStrLn ( take 25 $ map fst $ sortListBySecondElement (Map.toList map1))
  --c <- (decode <$> getJSON 2) :: IO (Maybe APIQuery)
  --d <- (decode $ getJSON) :: IO (Either String APIQuery)
  --print ys


sortListBySecondElement :: (Ord b) => [(a,b)] -> [(a,b)]
sortListBySecondElement xs = reverse $ sortBy (compare `on` snd) xs


filterCommonWords :: [String] -> Set.Set String -> [String]
filterCommonWords xs commonWordsSet = containsLetters $ filter (\x -> Set.notMember x commonWordsSet) xs --filter out common words, and only keep words that have letters in them
  where containsLetters ys = filter (\xw -> or $ map(\x -> elem x ['a'..'z']) xw) ys

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
