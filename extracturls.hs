-- also see https://github.com/jgm/pandoc/blob/master/src/Text/Pandoc/Parsing.hs#L447
-- also https://github.com/jgm/pandoc/blob/master/src/Text/Pandoc/Shared.hs#L841
{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception    as X
import           Control.Monad
import qualified Data.ByteString.Lazy as L (ByteString (..), empty)
import           Data.Maybe
import           Data.Monoid          (mconcat)
import           Data.Text            (unpack)
import           Network.HTTP.Conduit (simpleHttp)
import qualified Network.URL          as U (URL, URLType (Absolute), exportURL,
                                            importURL, url_type)
import           Text.HTML.DOM        (parseLBS)
import           Text.Pandoc
import           Text.Pandoc.Walk
import           Text.XML.Cursor      (attributeIs, content, element,
                                       fromDocument, ($//), (&//), (>=>))

sampleText = "wobble http://google.com wibble http://youtube.com foo http://pandoc.org/scripting-1.11.html woo bar baz woo http://www.dcs.gla.ac.uk/~simonpj/ https://i.imgur.com/svDQJx2.jpg"

extractURL :: Inline -> [String]
extractURL (Link _ (u,_)) = [u]
extractURL (Image _ (u,_)) = [u]
extractURL _ = []

extractURLs :: Pandoc -> [String]
extractURLs = query extractURL

readDoc :: String -> Pandoc
readDoc = readMarkdown def

main :: IO [String]
main = (getTitles . linkify) sampleText

linkify :: String -> String
linkify = unwords . (map angleWrap) . words
  where
    angleWrap :: String -> String
    angleWrap w = '<' : (w ++ ">")

getTitles :: String -> IO [String]
getTitles = mapM getTitle . extractURLs . readDoc

getTitle :: String -> IO String
getTitle url =
  do
    body <- (simpleHttp url) `X.catch` statusExceptionHandler
    case body of
      "" -> return ""
      _  -> case (extractTitle body) of
              "" -> return ""
              t  -> return t


statusExceptionHandler ::  SomeException -> IO L.ByteString
statusExceptionHandler e = (putStrLn "statusExceptionHandler") >> (return L.empty)

extractTitle :: L.ByteString -> String
extractTitle body =
    unpack $ mconcat $ cursor $// element "title" &// content
  where cursor = fromDocument doc
        doc = parseLBS body
