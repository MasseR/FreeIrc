{-# Language DeriveFunctor #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Hooks.Algebra where

import Control.Monad.Free
import Control.Monad.Reader
import Network.IRC
import Data.ByteString.Lazy (ByteString)

data IrcF a =
  SendMessage OutMsg a
  deriving Functor

type Header = [(String, String)]
data Payload = Payload Header ByteString
data WebF a =
  Fetch String (Payload -> a)
  deriving Functor

data DatabaseF x a =
    Add ByteString x a
  | Put ByteString x a
  | Get ByteString (Maybe x -> a)
  deriving Functor

data IrcS a1 a2 a = A1 (a1 a) | A2 (a2 a) deriving Functor

sendMessage :: OutMsg -> Free (IrcS IrcF WebF) ()
sendMessage out = liftF (A1 (SendMessage out ()))

fetch :: String -> Free (IrcS IrcF WebF) Payload
fetch url = liftF (A2 (Fetch url id))

newtype Irc a = Irc { unIrc :: Free (IrcS IrcF WebF) a }
  deriving (Functor, Applicative, Monad)

runIrc :: Irc a -> ReaderT OutChannel IO a
runIrc = foldFree f . unIrc
  where
    f :: IrcS IrcF WebF a -> ReaderT OutChannel IO a
    f (A1 (SendMessage msg next)) = ask >>= \c -> liftIO (sendMessage' c msg) >> return next
    f (A2 (Fetch url next)) = error "Not implemented"

