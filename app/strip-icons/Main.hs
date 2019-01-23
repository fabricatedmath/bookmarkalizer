{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment (getArgs)

main :: IO ()
main =
  do
    [file] <- getArgs
    fileLines <- T.lines <$> T.readFile file
    T.writeFile file $ T.unlines $ map go fileLines

go :: Text -> Text
go line
  | T.isPrefixOf "<DT><A HREF=" $ T.strip line =
      stripField "ICON_URI=" $ stripField "ICON=" line
  | otherwise = line

stripField :: Text -> Text -> Text
stripField field line =
  let (f,s) = T.breakOn field line
  in case T.null s of
       True -> line
       False ->
         let s' = drop 1 $ T.breakOnAll "\"" s
         in case s' of
              [] -> error "quote not found"
              (x:_) -> T.append (T.init f) $ T.tail $ snd x
