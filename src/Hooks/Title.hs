{-# Language OverloadedStrings #-}
module Hooks.Title where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hooks.Algebra
import Network.IRC
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Text.StringLike (castString)
import Text.HTML.TagSoup
import Data.Maybe (listToMaybe)
import Data.Char (isSpace)

urlTitleHook :: InMsg -> Irc ()
urlTitleHook (PrivMsg _nick target msg) =
  case () of
       () | "http://" `T.isInfixOf` msg ->
         maybe (return ()) (sendMessage . Msg target) =<< (handleWeb . parseUrl "http://" $ msg)
       () | "https://" `T.isInfixOf` msg ->
         maybe (return ()) (sendMessage . Msg target) =<< (handleWeb . parseUrl "https://" $ msg)
       _ -> return ()
urlTitleHook _ = return ()

parseTitle :: ByteString -> Maybe Text
parseTitle body = let
  tags = parseTags body :: [Tag ByteString]
  titleStr = "title" :: ByteString
  titleLst = takeWhile (~/= TagClose titleStr) . dropWhile (~== TagOpen titleStr []) $ tags
  title = T.strip . castString . innerText $ titleLst
  in if T.null title then Nothing else Just title

handleWeb :: Text -> Irc (Maybe Text)
handleWeb url = do
  Payload _ body <- fetch (T.unpack url)
  return $ if LBS.length body < 64000 then parseTitle body else Nothing

parseUrl :: Text -> Text -> Text
parseUrl splitter msg = let
  (_, start) = T.breakOn splitter msg
  url = T.takeWhile (not . isSpace) start
  in url
