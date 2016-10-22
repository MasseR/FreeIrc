{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
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
import Data.Acid.Url (UrlRecord(..))
import Data.Monoid

urlTitleHook :: InMsg -> Irc ()
urlTitleHook (PrivMsg nick target msg) =
  case () of
       () | "http://" `T.isInfixOf` msg -> handleTitle nick target "http://" msg
       () | "https://" `T.isInfixOf` msg -> handleTitle nick target "https://" msg
       _ -> return ()
urlTitleHook _ = return ()

handleTitle :: Text -> Text -> Text -> Text -> Irc ()
handleTitle nick target prefix msg = maybe (return ()) (respond url nick respondTo) =<< (handleWeb url)
  where
    url = parseUrl prefix msg
    respondTo = respondTarget nick target

respond :: Text -> Text -> Text -> Text -> Irc ()
respond url nick respondTo title = do
  now <- getCurrentTime
  previous <- getUrl url

  let r = UrlRecord url nick now
  addUrl url r

  sendMessage (Msg respondTo (format previous title))

format :: [UrlRecord] -> Text -> Text
format [] title = title
format (UrlRecord{..}:_) title = title <> " -- last seen by " <> _nick <> " on " <> (T.pack (show _time))

respondTarget :: Text -> Text -> Text
respondTarget nick target = if "#" `T.isPrefixOf` target then target else nick

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
