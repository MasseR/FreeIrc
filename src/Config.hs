{-# Language OverloadedStrings #-}
module Config where

import Network.IRC.Runner
import Network.IRC
import Hooks.Algebra
import Hooks.Title
import qualified Data.Text as T

import qualified Config.Dyre as Dyre
import Config.Dyre.Relaunch

adminHook :: InMsg -> Irc ()
adminHook (PrivMsg _nick _target msg) =
  case T.words msg of
       ["!join", channel] -> sendMessage (Join channel)
       _ -> return ()
adminHook _ = return ()

echoHook :: InMsg -> Irc ()
echoHook (PrivMsg _nick target msg) = sendMessage (Msg target msg)
echoHook _ = return ()



realMain :: IrcInfo -> IO ()
realMain conf = do
  connectIrc conf

dyre = Dyre.wrapMain $ Dyre.defaultParams {
    Dyre.projectName = "wtfbot"
  , Dyre.realMain = realMain
  }



defaultConf = IrcInfo "localhost" 6667 "FooBot" ["#oo"] (newHook urlTitleHook >> newHook adminHook)

