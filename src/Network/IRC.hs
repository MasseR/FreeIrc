{-# Language OverloadedStrings #-}
module Network.IRC where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Data.Monoid
import Data.Acid.Database
import Control.Monad.Trans
import Types


sendMessage' :: MonadIO m => TChan OutMsg -> OutMsg -> m ()
sendMessage' out = liftIO . atomically . writeTChan out


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

