{-# Language DeriveFunctor #-}
{-# Language TypeFamilies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language PartialTypeSignatures #-}
{-# Language PolyKinds #-}
{-# Language MultiParamTypeClasses #-}
module Hooks.Algebra where

import GHC.TypeLits
import Control.Monad.Free
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
import Data.Acid.Url
import Data.Functor.Sum
import Data.Proxy

data IrcF a =
  SendMessage OutMsg a
  deriving Functor

type Header = [(CI BS.ByteString, BS.ByteString)]
data Payload = Payload Header ByteString deriving Show
data WebF a =
  Fetch Options String (Payload -> a)
  deriving Functor

data UrlF a =
    Add Text UrlRecord a
  | Get Text ([UrlRecord] -> a)
  | GetCurrentTime (UTCTime -> a)
  deriving Functor

data IrcS a1 a2 a3 a = A1 (a1 a) | A2 (a2 a) | A3 (a3 a) deriving Functor

data ReadState = ReadState {
    outChannel :: OutChannel
  , acidState :: AcidState UrlState
  }

sendMessage :: OutMsg -> Irc ()
sendMessage out = Irc $ liftF (A1 (SendMessage out ()))

fetch :: String -> Irc Payload
fetch url = fetchWith defaults url

fetchWith :: Options -> String -> Irc Payload
fetchWith opts url = Irc $ liftF (A2 (Fetch opts url id))

addUrl :: Text -> UrlRecord -> Irc ()
addUrl url r = Irc $ liftF (A3 (Add url r ()))

getUrl :: Text -> Irc [UrlRecord]
getUrl url = Irc $ liftF (A3 (Get url id))

getCurrentTime :: Irc UTCTime
getCurrentTime = Irc $ liftF (A3 (GetCurrentTime id))

newtype Irc a = Irc { unIrc :: Free (IrcS IrcF WebF UrlF) a }
  deriving (Functor, Applicative, Monad)

runIrcF :: IrcF a -> ReaderT ReadState IO a
runIrcF (SendMessage msg next) = asks outChannel >>= \c -> liftIO (sendMessage' c msg) >> return next

runFetchF :: WebF a -> ReaderT ReadState IO a
runFetchF (Fetch opts url next) = liftIO (fetchHandler opts url) >>= return . next

runUrlF :: UrlF a -> ReaderT ReadState IO a
runUrlF (Add url record next) = asks acidState >>= \a -> liftIO (update' a (AddUrl url record)) >> return next
runUrlF (Get url next) = next <$> (asks acidState >>= \a -> liftIO (query' a (GetUrl url)))
runUrlF (GetCurrentTime next) = next <$> liftIO Time.getCurrentTime

runIrc :: Irc a -> ReaderT ReadState IO a
runIrc = foldFree f . unIrc
  where
    f :: IrcS IrcF WebF UrlF a -> ReaderT ReadState IO a
    f (A1 op) = runIrcF op
    f (A2 op) = runFetchF op
    f (A3 op) = runUrlF op

fetchHandler :: Options -> String -> IO Payload
fetchHandler opts url = do
  r <- getWith opts url
  return $ Payload (r ^. responseHeaders) (r ^. responseBody)


respondTarget :: Text -> Text -> Text
respondTarget nick target = if "#" `T.isPrefixOf` target then target else nick

respondTo :: Text -> Text -> Text -> Irc ()
respondTo nick trg msg = sendMessage (Msg (respondTarget nick trg) msg)

data CloudApiF a = PutFile String String a
                 | GetFile String (String -> a)
                 deriving (Functor)

data LogF a = Log String a
            deriving (Functor)

class Interpreted (f :: * -> *) where
    interpret :: MonadIO m => f a -> m a

instance Interpreted f => Interpreted (Free f) where
    interpret f = foldFree interpret f

instance (Interpreted f, Interpreted g) => Interpreted (Sum f g) where
    interpret (InL f) = interpret f
    interpret (InR g) = interpret g

instance Interpreted LogF where
    interpret (Log x next) = liftIO (putStrLn x) >> return next

instance Interpreted CloudApiF where
    interpret (PutFile name content next) = liftIO (putStrLn $ "Putting " ++ name) >> return next
    interpret (GetFile name next) = liftIO (putStrLn $ "Getting " ++ name) >> return (next "asd")

instance Interpreted UrlF where
    interpret _ = error "Not implemented"

class SumBuilder (x :: * -> *) (f :: [* -> *]) where
    type Eff f :: * -> *
    autoHoist :: x a -> Proxy f -> Free (Eff f) a

instance SumBuilder (Free f) ('[f] :: [* -> *]) where
    type Eff '[f] = f
    autoHoist f _ = f

instance (Functor f, Functor (Eff (g ': xs))) => SumBuilder (Free f) ((f :: * -> *) ': g ': xs) where
    type Eff (f ': g ': xs) = Sum f (Eff (g ': xs))
    autoHoist f _ = hoistFree InL f

instance (Functor f, Functor (Eff (g ': xs)), SumBuilder (Free g) (g ': xs)) => SumBuilder (Free g) (f ': g ': xs) where
    type Eff (f ': g ': xs) = Sum f (Eff (g ': xs))
    autoHoist g _ = hoistFree InR (autoHoist g p)
        where
            p :: Proxy (g ': xs)
            p = Proxy

-- class Hoister f xs where
--     autoHoist :: Free f a -> Proxy xs -> Free (Eff xs) a
--
-- instance (Functor f, Functor (Eff (f ': xs))) => Hoister f (f ': xs) where
--     autoHoist f _ = hoistFree InL f


logger :: String -> Free LogF ()
logger str = liftF (Log str ())
hoistedLogger :: String -> Free (Sum LogF (Sum UrlF CloudApiF)) ()
hoistedLogger str = hoistFree InL (logger str)
