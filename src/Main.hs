{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}
{-# Language RecordWildCards #-}
module Main where

import Config
import Data.Yaml.Config
import Data.Yaml
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Network.IRC.Runner (HookBuilder, newHook, IrcInfo(..))
import Hooks.Algebra
import Hooks.Title
import Hooks.Weather
import Network.IRC
import qualified Data.Text as T

data ConnectionConf = ConnectionConf { hostname :: Text
                                     , port :: Int
                                     , channels :: [Text]} deriving (Generic, Show)
data HookConf = HookConf { darkskyApiKey :: String } deriving (Generic, Show)
data Configuration = Configuration { connection :: [ConnectionConf]
                                   , hooksConf :: HookConf
                                   } deriving (Generic, Show)

instance FromJSON ConnectionConf
instance FromJSON HookConf
instance FromJSON Configuration

instance ToJSON ConnectionConf
instance ToJSON HookConf
instance ToJSON Configuration

adminHook :: InMsg -> Irc ()
adminHook (PrivMsg _nick _target msg) =
  case T.words msg of
       ["!join", channel] -> sendMessage (Join channel)
       _ -> return ()
adminHook _ = return ()

echoHook :: InMsg -> Irc ()
echoHook (PrivMsg _nick target msg) = sendMessage (Msg target msg)
echoHook _ = return ()

myHooks :: HookConf -> HookBuilder ()
myHooks HookConf{..} = do
    newHook urlTitleHook
    newHook adminHook
    newHook $ weatherHook $ ApiKey darkskyApiKey

main :: IO ()
main = do
    conf <- loadYamlSettings ["config/irc.yaml"] [] ignoreEnv :: IO Configuration
    defaultMain defaultConf {hooks = myHooks $ hooksConf conf}

