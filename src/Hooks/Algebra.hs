{-# Language DeriveFunctor #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
module Hooks.Algebra where

import Control.Monad.Free
import Control.Monad.Reader
import Network.IRC
import Data.ByteString.Lazy (ByteString)
import Control.Lens ((^.))
import Network.Wreq hiding (Payload)
import Data.CaseInsensitive (CI)
import qualified Data.ByteString as BS (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Acid.Url

data IrcF a =
  SendMessage OutMsg a
  deriving Functor

type Header = [(CI BS.ByteString, BS.ByteString)]
data Payload = Payload Header ByteString deriving Show
data WebF a =
  Fetch Options String (Payload -> a)
  deriving Functor

data UrlF a =
    Add Text UrlRecord a
  | Get Text ([UrlRecord] -> a)
  | GetCurrentTime (UTCTime -> a)
  deriving Functor

data IrcS a1 a2 a3 a = A1 (a1 a) | A2 (a2 a) | A3 (a3 a) deriving Functor

data ReadState = ReadState {
    outChannel :: OutChannel
  , acidState :: AcidState UrlState
  }

sendMessage :: OutMsg -> Irc ()
sendMessage out = Irc $ liftF (A1 (SendMessage out ()))

fetch :: String -> Irc Payload
fetch url = fetchWith defaults url

fetchWith :: Options -> String -> Irc Payload
fetchWith opts url = Irc $ liftF (A2 (Fetch opts url id))

addUrl :: Text -> UrlRecord -> Irc ()
addUrl url r = Irc $ liftF (A3 (Add url r ()))

getUrl :: Text -> Irc [UrlRecord]
getUrl url = Irc $ liftF (A3 (Get url id))

getCurrentTime :: Irc UTCTime
getCurrentTime = Irc $ liftF (A3 (GetCurrentTime id))

newtype Irc a = Irc { unIrc :: Free (IrcS IrcF WebF UrlF) a }
  deriving (Functor, Applicative, Monad)

runIrcF :: IrcF a -> ReaderT ReadState IO a
runIrcF (SendMessage msg next) = asks outChannel >>= \c -> liftIO (sendMessage' c msg) >> return next

runFetchF :: WebF a -> ReaderT ReadState IO a
runFetchF (Fetch opts url next) = liftIO (fetchHandler opts url) >>= return . next

runUrlF :: UrlF a -> ReaderT ReadState IO a
runUrlF (Add url record next) = asks acidState >>= \a -> liftIO (update' a (AddUrl url record)) >> return next
runUrlF (Get url next) = next <$> (asks acidState >>= \a -> liftIO (query' a (GetUrl url)))
runUrlF (GetCurrentTime next) = next <$> liftIO Time.getCurrentTime

runIrc :: Irc a -> ReaderT ReadState IO a
runIrc = foldFree f . unIrc
  where
    f :: IrcS IrcF WebF UrlF a -> ReaderT ReadState IO a
    f (A1 op) = runIrcF op
    f (A2 op) = runFetchF op
    f (A3 op) = runUrlF op

fetchHandler :: Options -> String -> IO Payload
fetchHandler opts url = do
  r <- getWith opts url
  return $ Payload (r ^. responseHeaders) (r ^. responseBody)
