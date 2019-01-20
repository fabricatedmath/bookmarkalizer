{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Control.Monad.State

import Data.Hashable

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as S

import Data.List (isSuffixOf, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.Environment (getArgs)

type Item = (Text,Text,Text)

filterBookmark :: [Text] -> [Text]
filterBookmark = filter (T.isPrefixOf "<DT><A HREF=") . map T.strip

setifyFile :: FilePath -> IO (HashMap Text Item)
setifyFile fp = do
  f <- T.readFile fp
  return . itemFromList . map process . filterBookmark $ T.lines f

itemFromList :: [Item] -> HashMap Text Item
itemFromList = S.fromListWith f . map ((\(l,_,_) -> l) &&& id)
  where f i1@(_,d1,_) i2@(_,d2,_)
          | d1 < d2 = i1
          | otherwise = i2

process :: Text -> Item
process t = (link,date,title)
  where ((link:date:_),title) = (take 2 . T.words *** T.tail) . T.break (== '>') . strip $ t
        strip = fromJust .  (T.stripSuffix "</A>" <=< T.stripPrefix "<DT><A HREF=")

main :: IO ()
main = do
  argList <- getArgs
  case argList of
    [dir,target] -> go dir target
    _ -> putStrLn "needs dir and target as args"

go :: FilePath -> FilePath -> IO ()
go dir target = do
  contents <- getDirectoryContents dir
  final <- foldM f S.empty (filter (".html" `isSuffixOf`) contents)
  T.writeFile target $ toHtml final
    where f set fp = do
            print fp
            next <- setifyFile $ dir ++ fp
            let union = S.union set next
            print $ "found " ++ show (S.size next) ++ " - now " ++ show (S.size union)
            return union

toHtml :: HashMap Text Item -> Text
toHtml set = T.unlines $ header ++ body ++ footer
  where body = map toLine . sortBy (comparing (\(_,x,_) -> x)) . map snd $ S.toList set

toLine :: Item -> Text
toLine (link,date,title) = T.concat ["\t","<DT><A HREF=",link," ",date,">",title,"</A>"]

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
