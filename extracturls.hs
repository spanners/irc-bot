{-# LANGUAGE OverloadedStrings #-}
import           Data.ByteString.Lazy.Internal (ByteString (..))
import           Data.Maybe
import           Data.Monoid                   (mconcat)
import           Data.Text                     (unpack)
import           Network.HTTP.Conduit          (simpleHttp)
import qualified Network.URL                   as U (URL, URLType (Absolute),
                                                     exportURL, importURL,
                                                     url_type)
import           Text.HTML.DOM                 (parseLBS)
import           Text.XML.Cursor               (attributeIs, content, element,
                                                fromDocument, ($//), (&//),
                                                (>=>))
-- extracturls.hs
import           Text.Pandoc
import           Text.Pandoc.Walk
import           Control.Monad

extractURL :: Inline -> [String]
extractURL (Link _ (u,_)) = [u]
extractURL (Image _ (u,_)) = [u]
extractURL _ = []

extractURLs :: Pandoc -> [String]
extractURLs = query extractURL

readDoc :: String -> Pandoc
readDoc = readMarkdown def

--main :: IO ()
--main = interact (  unlines
--                 . extractURLs
--                 . readDoc )

main :: IO ()
main = newMain "wobble <http://google.com> wibble <http://youtube.com> foo <http://pandoc.org/scripting-1.11.html> woo bar baz"

newMain :: String -> IO ()
newMain = mapM_ getTitle . extractURLs . readDoc

getTitle :: String -> IO ()
getTitle url =
  do
    body <- simpleHttp url
    print $ extractTitle body

extractTitle :: ByteString -> String
extractTitle body =
    unpack $ mconcat $ cursor $// element "title" &// content
  where cursor = fromDocument doc
        doc = parseLBS body
