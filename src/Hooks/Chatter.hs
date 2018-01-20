{-# Language NoImplicitPrelude #-}
{-# Language FlexibleContexts #-}
module Hooks.Chatter where

import Chat
import Hooks.Algebra
import Types
import Hooks.Weather (weather, formatWeather, ApiKey)
import Network.IRC
import ClassyPrelude
import Control.Monad.Trans
import Control.Monad.Logger
import Control.Monad.Trans.Maybe

weatherRule :: (MonadLogger m, MonadIO m, MonadReader (ReadState ApiKey) m) => (Text -> m ()) -> MatchTree m
weatherRule respond = fromText (\ctx -> getWeather ctx >>= response ctx) "what is the weather * in :city:"
    where
        getWeather :: (MonadLogger m, MonadIO m, MonadReader (ReadState ApiKey) m) => Ctx -> m (Maybe Text)
        getWeather ctx = runMaybeT $ do
            city <- MaybeT (return (lookup "city" ctx >>= listToMaybe))
            w <- MaybeT $ weather city
            return (formatWeather w)
        response ctx Nothing = respond ("I have no weather for " <> pack (show ctx))
        response ctx (Just w) = respond ("Weather for " <> pack (show ctx) <> " is " <> w)

chatterHook (PrivMsg nick target msg) = runMatcher msg (weatherRule respond)
    where
        respond = respondTo nick target
