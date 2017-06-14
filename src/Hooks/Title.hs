{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language FlexibleContexts #-}
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
import Data.Acid.Database (UrlRecord(..))
import Data.Monoid
import Types
import Data.Time (getCurrentTime)
import Data.Acid.Database
import Network.Wreq
import Control.Lens
import Control.Monad.Trans (liftIO)

type TitleHandler a = Handler (AcidState IrcState) a

urlTitleHook :: InMsg -> TitleHandler ()
urlTitleHook (PrivMsg nick target msg) =
  case () of
       () | "http://" `T.isInfixOf` msg -> handleTitle nick target "http://" msg
       () | "https://" `T.isInfixOf` msg -> handleTitle nick target "https://" msg
       _ -> return ()
urlTitleHook _ = return ()

handleTitle :: Text -> Text -> Text -> Text -> TitleHandler ()
handleTitle nick target prefix msg = maybe (return ()) (respond url nick respondTo) =<< (handleWeb url)
  where
    url = parseUrl prefix msg
    respondTo = respondTarget nick target

respond :: Text -> Text -> Text -> Text -> TitleHandler ()
respond url nick respondTo title = do
    now <- liftIO getCurrentTime
    previous <- query (GetUrl (respondTo <> url))
    sendMessage (Msg respondTo (format previous title))
    let r = UrlRecord url nick now
    update (AddUrl (respondTo <> url) r)


format :: [UrlRecord] -> Text -> Text
format [] title = title
format (UrlRecord{..}:_) title = title <> " -- last seen by " <> _nick <> " on " <> (T.pack (show _time))


parseTitle :: ByteString -> Maybe Text
parseTitle body = let
  tags = parseTags body :: [Tag ByteString]
  titleStr = "title" :: ByteString
  titleLst = takeWhile (~/= TagClose titleStr) . dropWhile (~/= TagOpen titleStr []) $ tags
  title = listToMaybe . T.lines . T.strip . castString . innerText $ titleLst
  in if maybe False T.null title then Nothing else title

handleWeb :: Text -> TitleHandler (Maybe Text)
handleWeb url = do
    r <- liftIO $ get (T.unpack url)
    let body = r ^. responseBody
    return $ if isOk (r ^. responseHeaders) body then parseTitle body else Nothing
  where
    isOk headers body =
      case find (\(k,_) -> k == "content-type") headers of
           Just (_,v) -> "text/html" `BS.isPrefixOf` v
           Nothing -> LBS.length body < 64000


parseUrl :: Text -> Text -> Text
parseUrl splitter msg = let
  (_, start) = T.breakOn splitter msg
  url = T.takeWhile (not . isSpace) start
  in postprocess url
  where
    postprocess u | "imgur" `T.isInfixOf` u && ".jpg" `T.isSuffixOf` u = stripSuffix u
                  | "imgur" `T.isInfixOf` u && ".png" `T.isSuffixOf` u = stripSuffix u
                  | otherwise = u
    stripSuffix = T.intercalate "." . init . T.splitOn "."
