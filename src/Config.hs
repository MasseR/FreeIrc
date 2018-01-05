{-# Language OverloadedStrings #-}
{-# Language DataKinds #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveGeneric #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
{-# Language RecordWildCards #-}
module Config where

import Network.IRC.Runner
import Network.IRC
import Plugin
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Lens
import Data.Aeson

data ConnectionConf = ConnectionConf { connectionConfHostname :: String
                                     , connectionConfPort :: Int
                                     , connectionConfChannels :: [Text]
                                     , connectionConfUsername :: Text } deriving (Generic, Show)

newtype HookConf = HookConf { hookConfDarkskyApiKey :: String } deriving (Generic, Show)

data Configuration = Configuration { configurationConnection :: [ConnectionConf]
                                   , configurationHooksConf :: HookConf
                                   } deriving (Generic, Show)

instance FromJSON HookConf where
    parseJSON = withObject "HookConf" $ \o ->
        HookConf <$> o .: "darksky_key"

instance FromJSON ConnectionConf where
    parseJSON = withObject "ConnectionConf" $ \o ->
        ConnectionConf <$> o .: "hostname"
                       <*> o .: "port"
                       <*> o .: "channels"
                       <*> o .: "username"

instance FromJSON Configuration where
    parseJSON = withObject "Configuration" $ \o ->
        Configuration <$> o .: "connection" <*> o .: "hooks"


makeFields ''ConnectionConf
makeFields ''HookConf
makeFields ''Configuration

defaultMain :: IrcInfo ps -> IO ()
defaultMain = connectIrc

-- XXX: Add helper for automatic parsing of Config -> IrcInfo

defaultConf :: ConnectionConf -> IrcInfo '[]
defaultConf ConnectionConf{..} = IrcInfo connectionConfHostname connectionConfPort connectionConfUsername connectionConfChannels PNil

