{-# Language OverloadedStrings #-}
module Main where

import Network.IRC.Runner
import Network.IRC
import Hooks.Algebra
import Hooks.Title
import qualified Data.Text as T
import System.Remote.Monitoring (forkServer)

adminHook :: InMsg -> Irc ()
adminHook (PrivMsg _nick _target msg) =
  case T.words msg of
       ["!join", channel] -> sendMessage (Join channel)
       _ -> return ()
adminHook _ = return ()

echoHook :: InMsg -> Irc ()
echoHook (PrivMsg _nick target msg) = sendMessage (Msg target msg)
echoHook _ = return ()



main :: IO ()
main = do
  connectIrc (IrcInfo "localhost" 6667 "Foobot" ["#oo"] (newHook urlTitleHook))
