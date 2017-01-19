{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveFunctor #-}
module Network.IRC where

-- import Control.Monad.State
import qualified Data.Text as T
import Data.Text (Text)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Data.Monoid
import Data.Acid.Database

data OutMsg =
    Nick !Text
  | User !Text !Text !Text !Text
  | Pong !Text
  | Join !Text
  | Msg !Text !Text
  deriving Show

data InMsg =
    Ping !Text
  | PrivMsg !Text !Text !Text
    deriving Show

type OutChannel = TChan OutMsg
type Hook = (TChan InMsg, TChan OutMsg, AcidState IrcState)


sendMessage' :: TChan OutMsg -> OutMsg -> IO ()
sendMessage' out = atomically . writeTChan out


renderMessage :: OutMsg -> Text
renderMessage msg =
  case msg of
       Nick nick -> T.unwords ["NICK", nick]
       User a b c d -> T.unwords ["USER", a, b, c, d]
       Pong response -> T.unwords ["PONG", response]
       Join channel -> T.unwords ["JOIN", channel]
       Msg target message -> T.unwords ["PRIVMSG", target, ":"<>message]

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

