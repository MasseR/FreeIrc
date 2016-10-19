{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
module Network.IRC.Runner where

import Network.IRC
import Network.Socket hiding (recv, send)
import Control.Monad.Writer
import Control.Monad.Reader
import System.IO (IOMode(..), hClose, Handle)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)

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
    mapM_ (sendMessage' outChan) (initial ++ [Join c | c <- channels])
    _writer <- forkIO (ircWriter outChan handle)
    _threads <- runReaderT (execWriterT $ unHook hooks) chans
    ircReader outChan inChan handle
    hClose handle

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

newHook :: (InMsg -> ReaderT (TChan OutMsg) IO ()) -> HookBuilder ()
newHook f = do
  original <- ask
  (inChan, outChan) <- liftIO $ atomically $ duplicate original
  t <- liftIO $ forkIO $ forever $ do
    msg <- atomically $ readTChan inChan
    runReaderT (f msg) outChan
  tell [t]
  where
    duplicate (a,b) = (,) <$> dupTChan a <*> dupTChan b