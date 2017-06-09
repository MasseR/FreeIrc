{-# Language GeneralizedNewtypeDeriving #-}
{-# Language GADTs #-}
module Network.IRC.Design where

import Control.Monad.Reader (ReaderT(..), MonadReader)
import Control.Monad.State (StateT(..), MonadState)
import Control.Monad.Trans (MonadIO)

-- Trying to find a proper design for connections

data App

data InnerEnvironment a =
  IRC a => InnerEnvironment { app :: a
                            , server :: Server }

newtype Channel = Channel String

data InnerState = InnerState { channels :: Channel }

class IRC a where

instance IRC App

newtype Handler r a = Handler {unHandler :: StateT InnerState (ReaderT (InnerEnvironment r) IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (InnerEnvironment r), MonadState InnerState)

newtype Duration = Duration Int

data Recovery = Noretry
              | Retry { seconds :: Duration }

data Server = Server { hostname :: String
                     , port :: Int }

data User = User { primaryNick :: String
                 , secondaryNick :: String
                 , realname :: String }

-- Should handlers be pull or push-based?
-- Should we separate user written handlers from server internal handlers?

start :: IRC a => InnerState -> a -> Server -> Recovery -> Handler a () -> IO ()
start oldState app server recovery = undefined

data Connection

withRecovery :: Recovery -> IO Connection -> (Connection -> Handler a ()) -> IO ()
withRecovery rules reconnect act = undefined

connect :: Server -> IO Connection
connect = undefined
