{-# Language OverloadedStrings #-}
module Hooks.Title where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hooks.Algebra
import Network.IRC
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Text.StringLike (castString)
import Text.HTML.TagSoup
import Data.Maybe (listToMaybe)
import Data.Char (isSpace)
import Data.List (find)

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
  titleLst = takeWhile (~/= TagClose titleStr) . dropWhile (~/= TagOpen titleStr []) $ tags
  title = listToMaybe . T.lines . T.strip . castString . innerText $ titleLst
  in if maybe False T.null title then Nothing else title

handleWeb :: Text -> Irc (Maybe Text)
handleWeb url = do
  Payload headers body <- fetch (T.unpack url)
  return $ if isOk headers body then parseTitle body else Nothing
  where
    isOk headers body =
      case find (\(k,_) -> k == "content-type") headers of
           Just (_,v) -> "text/html" `BS.isPrefixOf` v
           Nothing -> LBS.length body < 64000


parseUrl :: Text -> Text -> Text
parseUrl splitter msg = let
  (_, start) = T.breakOn splitter msg
  url = T.takeWhile (not . isSpace) start
  in url
