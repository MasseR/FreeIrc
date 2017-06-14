{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}
{-# Language RecordWildCards #-}
{-# Language FlexibleContexts #-}
module Main where

import Control.Exception (bracket)
import Data.Acid.Database
import Config
import Data.Yaml.Config
import Data.Yaml
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Network.IRC.Runner (IrcInfo(..))
import qualified Network.IRC.Runner as IRC
import Plugin
import Hooks.Algebra
import Control.Monad.Reader
import Control.Monad.Trans
import Hooks.Title
import Hooks.Weather
import Hooks.PlusOne
import Network.IRC
import Types
import qualified Network.IRC as IRC
import qualified Data.Text as T
import Data.Time

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

adminHook (PrivMsg _nick _target msg) =
  case T.words msg of
       ["!join", channel] -> sendMessage (Join channel)
       _ -> return ()
adminHook _ = return ()

uptimeHook :: InMsg -> Handler UTCTime ()
uptimeHook (PrivMsg nick target "!uptime") = do
    started <- asks readStateApp
    respondTo nick target $ T.pack $ show started
uptimeHook _ = return ()

base = Plugin () (const $ return ())


myPlugins start acid HookConf{..} = base adminHook
                                 :> Plugin acid (const $ return ()) urlTitleHook
                                 :> Plugin acid (const $ return ()) plusOneHook
                                 :> Plugin (ApiKey darkskyApiKey) (const $ return ()) weatherHook
                                 :> Plugin start (const $ return ()) uptimeHook
                                 :> PNil

-- myHooks :: HookConf -> HookBuilder ()
-- myHooks HookConf{..} = do
--     newHook urlTitleHook
--     newHook adminHook
--     newHook plusOneHook
--     newHook $ weatherHook $ ApiKey darkskyApiKey

withAcid path initial f = bracket (openLocalStateFrom path initial)
                                  (createCheckpointAndClose)
                                  (\acid -> f acid)

main :: IO ()
main = withAcid "state" initialIrcState $ \acid -> do
    now <- getCurrentTime
    conf <- loadYamlSettings ["config/irc.yaml"] [] ignoreEnv :: IO Configuration
    -- XXX: Use control.concurrent.async and list comprehensions to start
    -- multiple connections based on ConnectionConf
    defaultMain defaultConf {hooks = myPlugins now acid $ hooksConf conf}

