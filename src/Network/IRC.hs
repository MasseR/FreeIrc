{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveFunctor #-}
module Network.IRC where

import Control.Monad.Writer
import Control.Monad.Reader
-- import Control.Monad.State
import qualified Data.Text as T
import Data.Text (Text)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Control.Concurrent (ThreadId)

data OutMsg =
    Nick !Text
  | User !Text !Text !Text !Text
  | Pong !Text
  | Join !Text
  deriving Show

data InMsg =
    Ping !Text
  | PrivMsg !Text !Text !Text
    deriving Show

data IrcInfo = IrcInfo {
    hostname :: String
  , port :: Int
  , channels :: [Text]
  , hooks :: HookBuilder ()
  }

type OutChannel = TChan OutMsg
type Hook = (TChan InMsg, TChan OutMsg)
newtype HookBuilder a = HookBuilder {unHook :: WriterT [ThreadId] (ReaderT Hook IO) a}
  deriving (Functor, Applicative, Monad, MonadWriter [ThreadId], MonadReader Hook, MonadIO)



initial :: [OutMsg]
initial = [Nick "Foobot", User "foo" "foo" "foo" "foo"]

sendMessage' :: TChan OutMsg -> OutMsg -> IO ()
sendMessage' out = atomically . writeTChan out


renderMessage :: OutMsg -> Text
renderMessage msg =
  case msg of
       Nick nick -> T.unwords ["NICK", nick]
       User a b c d -> T.unwords ["USER", a, b, c, d]
       Pong response -> T.unwords ["PONG", response]
       Join channel -> T.unwords ["JOIN", channel]

parseLine :: Text -> Either Text InMsg
parseLine line =
  case T.words line of
       ("PING":response) -> Right $ Ping (T.unwords response)
       (source:"PRIVMSG":target:msg) -> Right $ PrivMsg (parseNick source) target (parseMsg . T.unwords $ msg)
       _ -> Left line
  where
    parseNick source = case T.splitOn "!" . T.tail $ source of
                              [nick,_] -> nick
                              _ -> ""
    parseMsg = T.tail

