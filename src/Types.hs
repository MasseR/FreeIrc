{-# Language NoImplicitPrelude #-}
module Types where

import ClassyPrelude
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad.Logger

data OutMsg =
    Nick !Text
  | User !Text !Text !Text !Text
  | Pong !Text
  | Join !Text
  | Msg !Text !Text
  deriving Show

data InMsg =
    Ping !Text
  | PrivMsg !Text !Text !Text
    deriving Show

type OutChannel = TChan OutMsg
type Hook = (TChan InMsg, TChan OutMsg)
data ReadState app = ReadState { readStateOutChannel :: OutChannel
                               , readStateApp :: app }


type Handler app a = ReaderT (ReadState app) (LoggingT IO) a
