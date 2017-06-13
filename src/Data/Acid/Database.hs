{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
module Data.Acid.Database
(
    AcidState
  , IrcState
  , createCheckpointAndClose
  , openLocalStateFrom
  , UrlRecord(..)
  , AddUrl(..)
  , GetUrl(..)
  , TopOnes(..)
  , PlusOne(..)
  , initialIrcState
  , update
  , query
)
where


import Control.Monad.Reader
import Control.Monad.State
import Data.Data

import Types
import Plugin

import Data.Acid (AcidState, Query, Update, makeAcidic, openLocalStateFrom)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)
import Data.SafeCopy (base, extension, Migrate(..), deriveSafeCopy)
import Data.Text (Text)
import Data.Time
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Lens
import Data.Maybe (fromMaybe)

data UrlRecord = UrlRecord {
    _url :: !Text
  , _nick :: !Text
  , _time :: !UTCTime
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

data IrcState_0 = IrcState_0 {
  _urls_0 :: Map Text [UrlRecord]
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

type Nick = Text
data IrcState = IrcState { _urls :: Map Text [UrlRecord]
                         , _plusOnes :: Map Nick Int
                         }

instance Migrate IrcState where
  type MigrateFrom IrcState = IrcState_0
  migrate i = IrcState (_urls_0 i) mempty

initialIrcState :: IrcState
initialIrcState = IrcState mempty mempty

$(makeLenses ''UrlRecord)
$(makeLenses ''IrcState)
$(deriveSafeCopy 0 'base ''UrlRecord)
$(deriveSafeCopy 0 'base ''IrcState_0)
$(deriveSafeCopy 1 'extension ''IrcState)

addUrl :: Text -> UrlRecord -> Update IrcState ()
addUrl url r = modifying urls (M.insertWith (<>) url [r])

getUrl :: Text -> Query IrcState [UrlRecord]
getUrl url = view (urls . at url . non [])

plusOne :: Nick -> Update IrcState ()
plusOne nick = modifying plusOnes (M.insertWith (+) nick 1)

topOnes :: Int -> Query IrcState [Nick]
topOnes n = f <$> view plusOnes
  where f = take n . map fst . sortBy (flip (comparing snd)) . M.toList

$(makeAcidic ''IrcState ['addUrl, 'getUrl, 'plusOne, 'topOnes])

query ev = asks readStateApp >>= \st -> query' st ev
update ev = asks readStateApp >>= \st -> update' st ev
