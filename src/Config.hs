{-# Language OverloadedStrings #-}
{-# Language DataKinds #-}
module Config where

import Network.IRC.Runner
import Network.IRC
import Plugin

defaultMain :: IrcInfo ps -> IO ()
defaultMain conf = do
  connectIrc conf


defaultConf :: IrcInfo '[]
defaultConf = IrcInfo "localhost" 6667 "FooBot" ["#oo"] PNil

