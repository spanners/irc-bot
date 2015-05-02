{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid          (mconcat)
import           Network.HTTP.Conduit (simpleHttp)
import           Text.HTML.DOM        (parseLBS)
import           Text.XML.Cursor      (attributeIs, content, element,
                                       fromDocument, ($//), (&//), (>=>))
import           Data.Text            (unpack)
import           Data.ByteString.Lazy.Internal (ByteString(..))

getTitle :: String -> IO ()
getTitle url = 
  do
    body <- simpleHttp url
    print $ extractTitle body

extractTitle ::ByteString -> String
extractTitle body = 
    unpack $ mconcat $ cursor $// element "title" &// content
  where cursor = fromDocument doc
        doc = parseLBS body
  

main = do
  getTitle "http://www.ted.com/talks/francis_collins_we_need_better_drugs_now.html"
