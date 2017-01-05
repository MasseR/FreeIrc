{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Network.IRC.Runner where

import Hooks.Algebra
import Network.IRC
import Network.Socket hiding (recv, send, connect)
import Network.Simple.TCP
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Text (Text)
import System.IO (IOMode(..), hClose, Handle)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO, ThreadId)
import Data.Acid.Url
import Control.Exception (bracket)

data IrcInfo = IrcInfo {
    hostname :: String
  , port :: !Int
  , nick :: !Text
  , channels :: [Text]
  , hooks :: HookBuilder ()
  }

newtype HookBuilder a = HookBuilder {unHook :: WriterT [ThreadId] (ReaderT Hook IO)  a}
  deriving (Functor, Applicative, Monad, MonadWriter [ThreadId], MonadReader Hook, MonadIO)

connectIrc :: IrcInfo -> IO ()
connectIrc i@IrcInfo{..} = newAcid hostname (connectIrc' i)

connectIrc' :: IrcInfo -> AcidState UrlState -> IO ()
connectIrc' IrcInfo{..} acid = do
    chans@(inChan, outChan, _) <- (,,) <$> atomically newTChan <*> atomically newTChan <*> return acid
    connect hostname (show port) $ \(sock, addr) -> do
        handle <- socketToHandle sock ReadWriteMode
        mapM_ (sendMessage' outChan) (initial nick channels)
        forkIO (ircWriter outChan handle)
        runReaderT (execWriterT . unHook $ hooks) chans
        ircReader outChan inChan handle
        hClose handle

newAcid :: String -> (AcidState UrlState -> IO ()) -> IO ()
newAcid host f = bracket (openLocalStateFrom ("stateFromHost/"<>host) initialUrlState)
                       (createCheckpointAndClose)
                       (\acid -> f acid)

initial :: Text -> [Text] -> [OutMsg]
initial nick channels = [Nick nick, User "foo" "foo" "foo" "foo"] ++ [Join c | c <- channels]

ircWriter :: TChan OutMsg -> Handle -> IO ()
ircWriter out handle = forever $ do
  msg <- atomically $ readTChan out
  let bs = renderMessage msg <> "\r\n"
  T.putStr bs
  T.hPutStr handle bs

ircReader :: TChan OutMsg -> TChan InMsg -> Handle -> IO ()
ircReader outChan inChan h = forever $ do
    line <- T.strip <$> T.hGetLine h
    let msg = parseLine line
    case msg of
         Right (Ping response) -> atomically $ writeTChan outChan (Pong response)
         Right m -> atomically $ writeTChan inChan m
         Left err -> T.putStrLn $ "Unparsed " <> err

newHook :: (InMsg -> Irc ()) -> HookBuilder ()
newHook f = do
  original@(_,_,acid) <- ask
  (inChan, outChan) <- liftIO $ atomically $ duplicate original
  _t <- liftIO $ forkIO $ forever $ do
    msg <- atomically $ readTChan inChan
    void $ forkIO $ runReaderT (runIrc (f msg)) (ReadState outChan acid)
  tell []
  where
    duplicate (a,b,_) = (,) <$> dupTChan a <*> dupTChan b
