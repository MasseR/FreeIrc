{-# Language OverloadedStrings #-}
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
import Control.Monad (forM_)
import Hooks.Title
import Hooks.Weather
import Hooks.PlusOne
import Network.IRC
import Types
import qualified Network.IRC as IRC
import qualified Data.Text as T
import Data.Time
import Control.Lens ((^.), (^..))
import Data.Text.Lens (packed, unpacked)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)



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


myPlugins start acid conf = base adminHook
                         :> Plugin acid (const $ return ()) urlTitleHook
                         :> Plugin acid (const $ return ()) plusOneHook
                         :> Plugin (ApiKey (conf ^. darkskyApiKey)) (const $ return ()) weatherHook
                         :> Plugin start (const $ return ()) uptimeHook
                         :> PNil

withAcid path initial = bracket (openLocalStateFrom path initial) createCheckpointAndClose


main :: IO ()
main = withAcid "state" initialIrcState $ \acid -> do
    now <- getCurrentTime
    conf <- loadYamlSettings ["config/irc.yaml"] [] ignoreEnv :: IO Configuration
    let runConfs = [(defaultConf conn) {hooks = myPlugins now acid $ conf ^. hooksConf} | conn <- conf ^. connection]
    threads <- forM runConfs $ \c ->
        async . defaultMain $ c
    mapM_ wait threads
