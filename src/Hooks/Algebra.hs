{-# Language OverloadedStrings #-}
{-# Language GADTs #-}
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
module Hooks.Algebra where

import GHC.TypeLits
import Control.Monad.Freer
import Control.Monad.Reader
import Network.IRC
import Data.ByteString.Lazy (ByteString)
import Control.Lens ((^.))
import Network.Wreq hiding (Payload, Proxy)
import Data.CaseInsensitive (CI)
import qualified Data.ByteString as BS (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Time as Time
import qualified Data.Acid.Database as DB
import Plugin
import Types


type Header = [(CI BS.ByteString, BS.ByteString)]
-- data Payload = Payload Header ByteString deriving Show
-- data WebF a where
--   Fetch :: Options -> String -> WebF Payload

-- data DatabaseF a where
--     AddUrl :: Text -> DatabaseF DB.UrlRecord
--     GetUrl :: Text -> DatabaseF [DB.UrlRecord]
--     GetUrlCurrentTime :: DatabaseF UTCTime
--     PlusOne :: Text -> DatabaseF ()
--     TopOnes :: Int -> DatabaseF [Text]


instance HasApp app (ReadState app) where
    getApp = readStateApp


-- sendMessage :: Member (IrcF OutMsg) xs => OutMsg -> Eff xs ()
-- sendMessage out = send (SendMessage out)

-- fetch :: String -> Irc Payload
-- fetch url = fetchWith defaults url

-- fetchWith :: Options -> String -> Irc Payload
-- fetchWith opts url = Irc $ liftF (A2 (Fetch opts url id))
--
-- addUrl :: Text -> DB.UrlRecord -> Irc ()
-- addUrl url r = Irc $ liftF (A3 (AddUrl url r ()))

-- getUrl :: MonadReader r m, MonadIO m, jText 

-- getUrl :: Text -> Irc [DB.UrlRecord]
-- getUrl url = Irc $ liftF (A3 (GetUrl url id))
--
-- plusOne :: Text -> Irc ()
-- plusOne nick = Irc $ liftF (A3 (PlusOne nick ()))
--
-- topOnes :: Int -> Irc [Text]
-- topOnes n = Irc $ liftF (A3 (TopOnes n id))
--
-- getCurrentTime :: Irc UTCTime
-- getCurrentTime = Irc $ liftF (A3 (GetUrlCurrentTime id))

-- newtype Irc a = Irc { unIrc :: Free (IrcS IrcF WebF DatabaseF) a }
--   deriving (Functor, Applicative, Monad)

-- runIrcF :: IrcF a -> ReaderT ReadState IO a
-- runIrcF (SendMessage msg next) = asks outChannel >>= \c -> liftIO (sendMessage' c msg) >> return next
--
-- runFetchF :: WebF a -> ReaderT ReadState IO a
-- runFetchF (Fetch opts url next) = liftIO (fetchHandler opts url) >>= return . next
--
-- runDatabaseF :: DatabaseF a -> ReaderT ReadState IO a
-- runDatabaseF (AddUrl url record next) = asks acidState >>= \a -> liftIO (DB.update' a (DB.AddUrl url record)) >> return next
-- runDatabaseF (GetUrl url next) = next <$> (asks acidState >>= \a -> liftIO (DB.query' a (DB.GetUrl url)))
-- runDatabaseF (GetUrlCurrentTime next) = next <$> liftIO Time.getCurrentTime
-- runDatabaseF (TopOnes n next) = next <$> (asks acidState >>= \a -> liftIO (DB.query' a (DB.TopOnes n)))
-- runDatabaseF (PlusOne nick next) = (asks acidState >>= \a -> liftIO (DB.update' a (DB.PlusOne nick))) >> pure next
--
-- runIrc :: Eff ('[IrcF OutMsg, ReaderT (ReadState app) IO]) a -> Eff '[ReaderT (ReadState app) IO] a
-- runIrc = runNat irc2m
--     where
sendMessage :: (MonadReader (ReadState app) m, MonadIO m) => OutMsg -> m ()
sendMessage msg = asks readStateOutChannel >>= \c -> liftIO (sendMessage' c msg)

-- runIrc :: Irc a -> ReaderT ReadState IO a
-- runIrc = foldFree f . unIrc
--   where
--     f :: IrcS IrcF WebF DatabaseF a -> ReaderT ReadState IO a
--     f (A1 op) = runIrcF op
--     f (A2 op) = runFetchF op
--     f (A3 op) = runDatabaseF op
--
-- fetchHandler :: Options -> String -> IO Payload
-- fetchHandler opts url = do
--   r <- getWith opts url
--   return $ Payload (r ^. responseHeaders) (r ^. responseBody)


respondTarget :: Text -> Text -> Text
respondTarget nick target = if "#" `T.isPrefixOf` target then target else nick

respondTo :: (MonadReader (ReadState app) m, MonadIO m) => Text -> Text -> Text -> m ()
respondTo nick trg msg = sendMessage (Msg (respondTarget nick trg) msg)

