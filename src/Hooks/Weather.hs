{-# Language TemplateHaskell #-}
{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
module Hooks.Weather where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (decode, Value)
import Data.Aeson.Lens
import Hooks.Algebra
import Network.Wreq
import Network.IRC
import Text.Printf

data Coord = Coord {_latitude :: Double, _longitude :: Double} deriving Show
data Weather = Weather { _temperature :: Double, _feelsLike :: Double, _icon :: Text } deriving Show

nominatim :: Text -> Irc (Maybe Coord)
nominatim city = do
    let url = "http://nominatim.openstreetmap.org/search"
        params = defaults & param "format" .~ ["json"]
                          & param "city" .~ [city]
    Payload _ bs <- fetchWith params url
    let json = decode bs :: Maybe Value
        coords = Coord <$> datapoint json "lat" <*> datapoint json "lon"
    return coords
    where
        datapoint json k = read . T.unpack <$> json ^? _Just . values . key k . _String

fetchWeather :: ApiKey -> Coord -> Irc (Maybe Weather)
fetchWeather (ApiKey apiKey) Coord{..} = do
    let url = printf "https://api.darksky.net/forecast/%s/%f,%f" apiKey _latitude _longitude :: String
    Payload _ bs <- fetch url
    let weather = Weather <$> fmap fahrenheitToCelcius (datapoint json "temperature")
                          <*> fmap fahrenheitToCelcius (datapoint json "apparentTemperature")
                          <*> json ^? _Just . key "currently" . key "icon" . _String
        json = decode bs :: Maybe Value
    return weather
    where
        datapoint json k = json ^? _Just . key "currently" . key k . _Number
        fahrenheitToCelcius f = fromRational $ toRational (f - 32) / 1.8

newtype ApiKey = ApiKey String deriving Show

weather :: ApiKey -> Text -> Irc (Maybe Weather)
weather apiKey city = runMaybeT $ do
    coords <- MaybeT $ nominatim city
    MaybeT $ fetchWeather apiKey coords

formatWeather :: Weather -> Text
formatWeather = T.pack . show

weatherHook :: ApiKey -> InMsg -> Irc ()
weatherHook apiKey (PrivMsg nick target msg) =
    case T.words msg of
         ["!weather", city] -> weather apiKey city >>= respond
         _ -> return ()
    where
        respond Nothing = return ()
        respond (Just w) = respondTo nick target (formatWeather w)
