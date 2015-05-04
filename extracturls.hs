-- also see https://github.com/jgm/pandoc/blob/master/src/Text/Pandoc/Parsing.hs#L447
-- also https://github.com/jgm/pandoc/blob/master/src/Text/Pandoc/Shared.hs#L841
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as L     (empty, ByteString (..))
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

-- status Exception handling
import Control.Exception as X

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
main = newMain "<wobble> <http://google.com> wibble <http://youtube.com> foo <http://pandoc.org/scripting-1.11.html> woo bar baz woo <http://www.dcs.gla.ac.uk/~simonpj/> <https://i.imgur.com/svDQJx2.jpg>"

newMain :: String -> IO ()
newMain = mapM_ getTitle . extractURLs . readDoc

getTitle :: String -> IO ()
getTitle url =
  do
    body <- (simpleHttp url) `X.catch` statusExceptionHandler
    case body of
      "" -> return ()
      _  -> case (extractTitle body) of
              ""        -> return ()
              something -> print something

statusExceptionHandler ::  SomeException -> IO L.ByteString
statusExceptionHandler e = (putStrLn "statusExceptionHandler") >> (return L.empty)

extractTitle :: L.ByteString -> String
extractTitle body =
    unpack $ mconcat $ cursor $// element "title" &// content
  where cursor = fromDocument doc
        doc = parseLBS body
