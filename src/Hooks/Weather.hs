{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell #-}
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
import Control.Monad.Logger

data Coord = Coord {_latitude :: Double, _longitude :: Double} deriving Show
data Cloudy = NoCloud | SomeCloud | Cloudy deriving Show
data Weather = Weather { _temperature :: Double, _feelsLike :: Double, _icon :: Text, _cloudy :: Cloudy } deriving Show

nominatim city = do
    let url = "http://nominatim.openstreetmap.org/search"
        params = defaults & param "format" .~ ["json"]
                          & param "city" .~ [city]
    r <- liftIO $ getWith params url
    let coords = Coord <$> datapoint r "lat" <*> datapoint r "lon"
    return coords
    where
        datapoint json k = read . T.unpack <$> json ^? responseBody . nth 0 . key k . _String

fetchWeather (ApiKey apiKey) Coord{..} = do
    let url = printf "https://api.darksky.net/forecast/%s/%f,%f" apiKey _latitude _longitude :: String
    r <- liftIO $ get url
    let weather = Weather <$> fmap fahrenheitToCelcius (datapoint r "temperature")
                          <*> fmap fahrenheitToCelcius (datapoint r "apparentTemperature")
                          <*> r ^? responseBody . key "currently" . key "icon" . _String
                          <*> (toCloudy <$> r ^? responseBody . key "currently" . key "cloudCover" . _Number)
    return weather
    where
        toCloudy d | d <= 0.25 = NoCloud
                   | d <= 0.75 = SomeCloud
                   | otherwise = Cloudy
        datapoint json k = json ^? responseBody . key "currently" . key k . _Number
        fahrenheitToCelcius f = fromRational $ toRational (f - 32) / 1.8

newtype ApiKey = ApiKey String deriving Show

weather city = runMaybeT $ do
    $logInfo ("Trying to fetch weather for " <> city)
    apiKey <-lift $ asks readStateApp
    coords <- MaybeT $ nominatim city
    MaybeT $ fetchWeather apiKey coords

formatWeather Weather{..} = T.pack $ printf "%0.0f degrees C, %s, %s" _temperature _icon (formatCloud _cloudy)
    where
        formatCloud :: Cloudy -> String
        formatCloud Cloudy = "clouds"
        formatCloud SomeCloud = "some clouds"
        formatCloud _ = "clear skies"

weatherHook (PrivMsg nick target msg) =
    case T.words msg of
         ["!weather", city] -> weather city >>= respond
         _ -> return ()
    where
        respond Nothing = return ()
        respond (Just w) = respondTo nick target (formatWeather w)
