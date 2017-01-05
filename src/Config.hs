{-# Language OverloadedStrings #-}
module Config where

import Network.IRC.Runner
import Network.IRC

defaultMain :: IrcInfo -> IO ()
defaultMain conf = do
  connectIrc conf


defaultConf :: IrcInfo
defaultConf = IrcInfo "localhost" 6667 "FooBot" ["#oo"] (return ())

