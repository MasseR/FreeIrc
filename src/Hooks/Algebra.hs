{-# Language OverloadedStrings #-}
{-# Language GADTs #-}
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
module Hooks.Algebra where

import GHC.TypeLits
import Control.Monad.Reader
import Network.IRC
import Data.ByteString.Lazy (ByteString)
import Control.Lens ((^.))
import Network.Wreq hiding (Payload, Proxy)
import Data.CaseInsensitive (CI)
import qualified Data.ByteString as BS (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Time as Time
import qualified Data.Acid.Database as DB
import Plugin
import Types


instance HasApp app (ReadState app) where
    getApp = readStateApp

sendMessage :: (MonadReader (ReadState app) m, MonadIO m) => OutMsg -> m ()
sendMessage msg = asks readStateOutChannel >>= \c -> liftIO (sendMessage' c msg)


respondTarget :: Text -> Text -> Text
respondTarget nick target = if "#" `T.isPrefixOf` target then target else nick

respondTo :: (MonadReader (ReadState app) m, MonadIO m) => Text -> Text -> Text -> m ()
respondTo nick trg msg = sendMessage (Msg (respondTarget nick trg) msg)

