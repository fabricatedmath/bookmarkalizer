{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Lazy as B
import Data.Text as T
import Data.Text.IO as T (writeFile)
import Data.Time.Clock
import Data.Aeson

import System.Environment

import GHC.Generics
{-
data Folder2 =
  Folder2
  { title :: Text
  } deriving (Show, Generic)
-}

data Folder =
  Folder
  { title :: Text
  , id :: Int
  , dateAdded :: Maybe Integer
--  , lastModified :: Integer
  , children :: Maybe [Folder]
  , uri :: Maybe Text
  , annos :: Maybe [Anno]
  } deriving (Show,Generic)

fromAnno :: Maybe [Anno] -> Maybe Text
fromAnno Nothing = Nothing
fromAnno (Just [a]) = Just $ value a

toItems :: Folder -> [Item]
toItems folder =
  let
    is =
      case uri folder of
       Nothing -> []
       Just t -> [Item t (fromAnno (annos folder)) (title folder) (dateAdded folder)]
  in
    is ++ maybe [] (Prelude.concatMap toItems) (children folder)

data Item =
  Item
  { url :: Text
  , desc :: Maybe Text
  , title2 :: Text
  , added :: Maybe Integer
  } deriving Show

data Anno =
  Anno
  { value :: Text
  } deriving (Show, Generic)

instance FromJSON Folder
instance FromJSON Anno
--instance FromJSON Folder2

{-
instance FromJSON Folder where
  parseJSON (Object v) =
    Folder <$> v .: "title"
  parseJSON _ = mzero
-}

main :: IO ()
main =
  do
    [fileNameBase] <- getArgs
    file <- B.readFile $ fileNameBase ++ ".json"
    case eitherDecode file  :: Either String Folder of
      Left err -> putStrLn err
      Right ps ->
        T.writeFile (fileNameBase ++ ".html") $ T.unlines $ header ++ (Prelude.map toLine $ toItems ps) ++ footer

toLine :: Item -> Text
toLine (Item u md t ma) =
  let
    link = [" HREF=\"",u,"\""]
    mAddDate = maybe [" ADD_DATE=\"0\""] (\d -> [" ADD_DATE=\"",pack $ show d,"\""]) ma
    mdesc = maybe [">",t] (\d -> [">",t," ",d]) md
    beginner = ["<DT><A"]
    ender = ["</A>"]
  in
    T.concat $ beginner ++ link ++ mAddDate ++ mdesc ++ ender

header =
  [ "<!DOCTYPE NETSCAPE-Bookmark-file-1>"
  , "<!-- This is an automatically generated file."
  , "\tIt will be read and overwritten."
  , "\tDO NOT EDIT! -->"
  , "<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=UTF-8\">"
  , "<TITLE>Bookmarks</TITLE>"
  , "<H1>Bookmarks</H1>"
  , "<DL><p>"
  ]

footer =
  [ "</DL><p>" ]
