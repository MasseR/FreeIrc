{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language FlexibleContexts #-}
module Hooks.Weather where

import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Reader (asks)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (decode, Value)
import Data.Aeson.Lens
import Hooks.Algebra
import Network.IRC
import Text.Printf
import Network.Wreq
import Types

data Coord = Coord {_latitude :: Double, _longitude :: Double} deriving Show
data Weather = Weather { _temperature :: Double, _feelsLike :: Double, _icon :: Text } deriving Show

nominatim city = do
    let url = "http://nominatim.openstreetmap.org/search"
        params = defaults & param "format" .~ ["json"]
                          & param "city" .~ [city]
    r <- liftIO $ getWith params url
    let coords = Coord <$> datapoint r "lat" <*> datapoint r "lon"
    return coords
    where
        datapoint json k = read . T.unpack <$> json ^? responseBody . key k . _String

fetchWeather (ApiKey apiKey) Coord{..} = do
    let url = printf "https://api.darksky.net/forecast/%s/%f,%f" apiKey _latitude _longitude :: String
    r <- liftIO $ get url
    let weather = Weather <$> fmap fahrenheitToCelcius (datapoint r "temperature")
                          <*> fmap fahrenheitToCelcius (datapoint r "apparentTemperature")
                          <*> r ^? responseBody . key "currently" . key "icon" . _String
    return weather
    where
        datapoint json k = json ^? responseBody . key "currently" . key k . _Number
        fahrenheitToCelcius f = fromRational $ toRational (f - 32) / 1.8

newtype ApiKey = ApiKey String deriving Show

weather city = runMaybeT $ do
    apiKey <-lift $ asks readStateApp
    coords <- MaybeT $ nominatim city
    MaybeT $ fetchWeather apiKey coords

formatWeather = T.pack . show

weatherHook (PrivMsg nick target msg) =
    case T.words msg of
         ["!weather", city] -> weather city >>= respond
         _ -> return ()
    where
        respond Nothing = return ()
        respond (Just w) = respondTo nick target (formatWeather w)
