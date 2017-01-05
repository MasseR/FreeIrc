{-# Language OverloadedStrings #-}
module Config where

import Network.IRC.Runner
import Network.IRC
import Hooks.Algebra
import Hooks.Title
import Hooks.Weather
import qualified Data.Text as T


adminHook :: InMsg -> Irc ()
adminHook (PrivMsg _nick _target msg) =
  case T.words msg of
       ["!join", channel] -> sendMessage (Join channel)
       _ -> return ()
adminHook _ = return ()

echoHook :: InMsg -> Irc ()
echoHook (PrivMsg _nick target msg) = sendMessage (Msg target msg)
echoHook _ = return ()



defaultMain :: IrcInfo -> IO ()
defaultMain conf = do
  connectIrc conf


defaultConf :: IrcInfo
defaultConf = IrcInfo "localhost" 6667 "FooBot" ["#oo"] (return ())

