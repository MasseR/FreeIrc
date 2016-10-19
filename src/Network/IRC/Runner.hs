{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Network.IRC.Runner where

import Hooks.Algebra
import Network.IRC
import Network.Socket hiding (recv, send)
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Text (Text)
import System.IO (IOMode(..), hClose, Handle)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO, ThreadId)

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
connectIrc IrcInfo{..} = do
  let hints = defaultHints
  addr:_ <- getAddrInfo (Just hints) (Just hostname) (Just (show port))
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  connected <- isConnected sock
  chans@(inChan, outChan) <- atomically $ (,) <$> newTChan <*> newTChan
  when connected $ do
    handle <- socketToHandle sock ReadWriteMode
    mapM_ (sendMessage' outChan) (initial nick channels)
    _writer <- forkIO (ircWriter outChan handle)
    _threads <- runReaderT (execWriterT . unHook $ hooks) chans
    ircReader outChan inChan handle
    hClose handle

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
  original <- ask
  (inChan, outChan) <- liftIO $ atomically $ duplicate original
  t <- liftIO $ forkIO $ forever $ do
    msg <- atomically $ readTChan inChan
    runReaderT (runIrc (f msg)) outChan
  tell []
  where
    duplicate (a,b) = (,) <$> dupTChan a <*> dupTChan b
