{-# Language OverloadedStrings #-}
module Main where

import Network.IRC.Runner
import Network.IRC
import Hooks.Algebra
import Hooks.Title

echoHook :: InMsg -> Irc ()
echoHook (PrivMsg _nick target msg) = sendMessage (Msg target msg)
echoHook _ = return ()



main :: IO ()
main = do
  connectIrc (IrcInfo "localhost" 6667 ["#oo"] (newHook urlTitleHook))
