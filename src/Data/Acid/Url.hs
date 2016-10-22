{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TypeFamilies #-}
module Data.Acid.Url
(
    AcidState
  , UrlState
  , createCheckpointAndClose
  , query'
  , update'
  , openLocalStateFrom
  , UrlRecord(..)
  , AddUrl(..)
  , GetUrl(..)
  , initialUrlState
)
where


import Control.Monad.Reader
import Control.Monad.State
import Data.Data

import Data.Acid (AcidState, Query, Update, makeAcidic, openLocalStateFrom)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (Text)
import Data.Time
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import Control.Lens
import Data.Maybe (fromMaybe)

data UrlRecord = UrlRecord {
    _url :: !Text
  , _nick :: !Text
  , _time :: !UTCTime
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

data UrlState = UrlState {
  _urls :: Map Text [UrlRecord]
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

initialUrlState :: UrlState
initialUrlState = UrlState mempty

$(makeLenses ''UrlRecord)
$(makeLenses ''UrlState)
$(deriveSafeCopy 0 'base ''UrlRecord)
$(deriveSafeCopy 0 'base ''UrlState)

addUrl :: Text -> UrlRecord -> Update UrlState ()
addUrl url r = modifying urls (M.insertWith (<>) url [r])

getUrl :: Text -> Query UrlState [UrlRecord]
getUrl url = view (urls . at url . non [])

$(makeAcidic ''UrlState ['addUrl, 'getUrl])
