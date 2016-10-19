{-# Language DeriveFunctor #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
module Hooks.Algebra where

import Control.Monad.Free
import Control.Monad.Reader
import Network.IRC
import Data.ByteString.Lazy (ByteString)
import Control.Lens
import Network.Wreq hiding (Payload)
import Data.CaseInsensitive (CI)
import qualified Data.ByteString as BS (ByteString)

data IrcF a =
  SendMessage OutMsg a
  deriving Functor

type Header = [(CI BS.ByteString, BS.ByteString)]
data Payload = Payload Header ByteString deriving Show
data WebF a =
  Fetch String (Payload -> a)
  deriving Functor

data DatabaseF x a =
    Add ByteString x a
  | Put ByteString x a
  | Get ByteString (Maybe x -> a)
  deriving Functor

data IrcS a1 a2 a = A1 (a1 a) | A2 (a2 a) deriving Functor

sendMessage :: OutMsg -> Irc ()
sendMessage out = Irc $ liftF (A1 (SendMessage out ()))

fetch :: String -> Irc Payload
fetch url = Irc $ liftF (A2 (Fetch url id))

newtype Irc a = Irc { unIrc :: Free (IrcS IrcF WebF) a }
  deriving (Functor, Applicative, Monad)

runIrc :: Irc a -> ReaderT OutChannel IO a
runIrc = foldFree f . unIrc
  where
    f :: IrcS IrcF WebF a -> ReaderT OutChannel IO a
    f (A1 (SendMessage msg next)) = ask >>= \c -> liftIO (sendMessage' c msg) >> return next
    f (A2 (Fetch url next)) = liftIO (fetchHandler url) >>= return . next

fetchHandler :: String -> IO Payload
fetchHandler url = do
  r <- get url
  return $ Payload (r ^. responseHeaders) (r ^. responseBody)
