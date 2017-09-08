{-# Language OverloadedStrings #-}
{-# Language DataKinds #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveGeneric #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
module Config where

import Network.IRC.Runner
import Network.IRC
import Plugin
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Lens

data ConnectionConf = ConnectionConf { connectionConfHostname :: Text
                                     , connectionConfPort :: Int
                                     , connectionConfChannels :: [Text]} deriving (Generic, Show)
data HookConf = HookConf { hookConfDarkskyApiKey :: String } deriving (Generic, Show)
data Configuration = Configuration { configurationConnection :: [ConnectionConf]
                                   , configurationHooksConf :: HookConf
                                   } deriving (Generic, Show)
-- XXX: Do a manual aeson FromJSON
-- XXX: Add username to configurable values

makeFields ''ConnectionConf
makeFields ''HookConf
makeFields ''Configuration

defaultMain :: IrcInfo ps -> IO ()
defaultMain conf = do
  connectIrc conf


defaultConf :: IrcInfo '[]
defaultConf = IrcInfo "localhost" 6667 "FooBot" ["#oo"] PNil

