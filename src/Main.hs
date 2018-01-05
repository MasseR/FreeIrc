{-# Language OverloadedStrings #-}
{-# Language FlexibleContexts #-}
{-# Language NoImplicitPrelude #-}
{-# Language DataKinds #-}
module Main where

import ClassyPrelude hiding (Handler)
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
import Control.Concurrent.Async (wait)
import Text.Printf (printf)



adminHook (PrivMsg _nick _target msg) =
  case T.words msg of
       ["!join", channel] -> sendMessage (Join channel)
       _ -> return ()
adminHook _ = return ()

uptimeHook :: InMsg -> Handler UTCTime ()
uptimeHook (PrivMsg nick target "!uptime") = do
    started <- asks readStateApp
    now <- liftIO getCurrentTime
    let seconds = floor $ diffUTCTime now started :: Integer
        (days, seconds') = seconds `quotRem` 86400
        (hours, seconds'') = seconds' `quotRem` 3600
        (minutes, seconds''') = seconds'' `quotRem` 60
        uptime = printf "%d days, %d hours, %d minutes, %d seconds" days hours minutes seconds'''
    respondTo nick target $ T.pack uptime
uptimeHook _ = return ()

base = Plugin () (const $ return ())


myPlugins :: HasDarkskyApiKey s String => UTCTime -> AcidState IrcState -> s -> Plugins InMsg '[(), AcidState IrcState, AcidState IrcState, ApiKey, UTCTime]
myPlugins start acid conf = base adminHook
                         :> Plugin acid (const $ return ()) urlTitleHook
                         :> Plugin acid (const $ return ()) plusOneHook
                         :> Plugin (ApiKey (conf ^. darkskyApiKey)) (const $ return ()) weatherHook
                         :> Plugin start (const $ return ()) uptimeHook
                         :> PNil

withAcid :: FilePath -> IrcState -> (AcidState IrcState -> IO c) -> IO c
withAcid path initial = bracket (openLocalStateFrom path initial) createCheckpointAndClose


main :: IO ()
main = withAcid "state" initialIrcState $ \acid -> do
    now <- getCurrentTime
    conf <- loadYamlSettings ["config/irc.yaml"] [] ignoreEnv :: IO Configuration
    let runConfs = [(defaultConf conn) {hooks = myPlugins now acid $ conf ^. hooksConf} | conn <- conf ^. connection]
    threads <- forM runConfs $ \c ->
        async . defaultMain $ c
    mapM_ wait threads
