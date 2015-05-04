{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Network
import System.IO
import Data.List
import Data.Char
import System.Random
import qualified Network.URL as U (URL, URLType (Absolute), exportURL, importURL, url_type)
import GetURLTitle


server = "irc.freenode.org"
port = 6667
channel = "##bots_"
nickname = "JustAnIRCBot"

action :: Handle -> IO ()
action h = do
  line <- hGetLine h
  putStrLn line
  case line of
    PING serv              -> pong h serv
    JOIN Bot chan          -> msg h chan $ "Hello, I am " ++ nickname
    JOIN nick chan         -> msg h chan ("Hi, " ++ nick ++ ". Welcome to " ++ chan)
    PM   from m            -> msg h from ("You said \"" ++ m ++ "\" to me?!")
    MSG  from chan (URL u) -> do
      titles <- getTitles u
      mapM_ (msg h chan) (map ("Title: " ++) titles)
    MSG  from chan Cat     -> msg h chan "Mew!"
    MSG  from chan Roll    -> do
      roll :: Int <- randomRIO (1, 6)
      msg h chan (from ++ ": You rolled " ++ show roll)
    _                      -> return ()

pattern Bot = "JustAnIRCBot"
pattern JOIN nick chan
   <- (words -> [getNick -> Just nick, "JOIN", chan])
pattern PING serv <- (words -> ["PING", serv])
pattern Nick n <- ((\a -> (head a /= '#', a)) -> (True, n))
pattern Chan c <- ((\a -> (head a == '#', a)) -> (True, c))
pattern PM from m <- (getPriv -> Just (from, Nick Bot,  m))
pattern MSG from to m <- (getPriv -> Just (from, Chan to, m))
pattern Cat <- (isInfixOf "cat" . map toLower -> True)
pattern Command cmd = '>':' ':cmd
pattern Roll <- Command (map toLower -> "roll")
pattern URL u <- ((\a -> (isURL a, a)) -> (True,  u))

-- Determine if a String is a URL
isURL :: String -> Bool
isURL str = case (U.importURL str) of
              Just url -> isAbsolute url
              _     -> False
  where
    isAbsolute :: U.URL -> Bool
    isAbsolute url =
      case (U.url_type url) of
        U.Absolute _ -> True
        _            -> False

-- Choose a nick
nick :: Handle -> String -> IO ()
nick h name = hPutStrLn h ("NICK " ++ name)

-- Specify username
user :: Handle -> String -> IO ()
user h name = hPutStrLn h ("USER " ++ name ++ " 0 * :" ++ name ++ " " ++ name)

-- Join a channel
joinChan :: Handle -> String -> IO ()
joinChan h chan = do
    putStrLn ("JOIN" ++ chan)
    hPutStrLn h ("JOIN " ++ chan)

-- Respond to "PING" with "PONG"
pong :: Handle -> String -> IO ()
pong h serv = hPutStrLn h ("PONG " ++ serv)

-- Get the nickname of a user (for use with PRIVMSG)
getNick :: String -> Maybe String
getNick (':':prefix) = do
  index <- findIndex (== '!') prefix
  return (take index prefix)
getNick _            = Nothing

-- Send message to channel
msg :: Handle -> String -> String -> IO ()
msg h chan msg = hPutStrLn h ("PRIVMSG " ++ chan ++ " :" ++ msg)

-- Get the message sent over PRIVMSG
getPriv :: String -> Maybe (String, String, String)
getPriv msg = case words msg of
  sender : "PRIVMSG" : target : (':':_) : _ -> do
    nick <- getNick sender
    return (nick, target, clean msg)
  _ -> Nothing
  where
  clean = tail . dropWhile (/=':') . dropWhile (/= ' ') . tail

main = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering   h NoBuffering
  hSetNewlineMode h (NewlineMode CRLF CRLF)

  nick h nickname
  user h nickname

  joinChan h channel

  forever (action h)
