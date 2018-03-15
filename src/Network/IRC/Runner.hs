{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
module Network.IRC.Runner where

import Hooks.Algebra
import Network.IRC
import Network.Socket hiding (connect)
import Network.Simple.TCP
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Text (Text)
import System.IO (IOMode(..), hClose, Handle)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async.Lifted (async, Async)
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Data.Acid.Database
import Control.Exception (SomeException, catch)
import Control.Exception.Lifted (bracket)
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Plugin
import Types

data IrcInfo ps = IrcInfo {
    hostname :: String
  , port :: !Int
  , nick :: !Text
  , channels :: [Text]
  , hooks :: Plugins InMsg ps
  }

connectIrc :: IrcInfo ps -> IO ()
connectIrc info@IrcInfo{..} = run `catch` (\e -> let _ = e :: SomeException in threadDelay 30000000 >> connectIrc info)
  where
    run = runStdoutLoggingT $ do
      chans@(inChan, outChan) <- liftIO ((,) <$> atomically newTChan <*> atomically newTChan)
      connect hostname (show port) $ \(sock, addr) ->
        bracket
          (liftIO $ socketToHandle sock ReadWriteMode)
          (liftIO . hClose) $ \handle -> do
            async $ do
              liftIO $ threadDelay (2 * 10^6)
              mapM_ (sendMessage' outChan) (initial nick channels)
            async (ircWriter outChan handle)
            runPlugins chans hooks
            ircReader outChan inChan handle
            liftIO $ hClose handle

initial :: Text -> [Text] -> [OutMsg]
initial nick channels = [Nick nick, User "foo" "foo" "foo" "foo"] ++ [Join c | c <- channels]

ircWriter :: TChan OutMsg -> Handle -> LoggingT IO ()
ircWriter out handle = forever $ do
  msg <- liftIO $ atomically $ readTChan out
  let bs = renderMessage msg <> "\r\n"
  $logInfo bs
  liftIO $ T.hPutStr handle bs

ircReader :: TChan OutMsg -> TChan InMsg -> Handle -> LoggingT IO ()
ircReader outChan inChan h = forever $ do
    line <- liftIO (T.strip <$> T.hGetLine h)
    let msg = parseLine line
    case msg of
         Right (Ping response) -> liftIO $ atomically $ writeTChan outChan (Pong response)
         Right m -> liftIO $ atomically $ writeTChan inChan m
         Left err -> $logWarn ("Unparsed " <> err)

runPlugins :: Hook -> Plugins InMsg apps -> LoggingT IO [Async ()]
runPlugins _ PNil = return []
runPlugins original (plugin :> ps) = do
    t <- runPlugin original plugin
    ts <- runPlugins original ps
    return $ t : ts

runPlugin :: HasApp app (ReadState app) => Hook -> Plugin InMsg app -> LoggingT IO (Async ())
runPlugin original (Plugin env _ w) = do
    (inChan, outChan) <- liftIO $ atomically $ duplicate original
    async $ forever $ do
        msg <- liftIO $ atomically $ readTChan inChan
        runReaderT (w msg) (ReadState outChan env)
    where
        duplicate (a,b) = (,) <$> dupTChan a <*> dupTChan b
