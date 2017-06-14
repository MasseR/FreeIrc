{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
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
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Data.Acid.Database
import Control.Exception (bracket)
import Control.Monad.Freer
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
connectIrc IrcInfo{..} = do
    chans@(inChan, outChan) <- (,) <$> atomically newTChan <*> atomically newTChan
    connect hostname (show port) $ \(sock, addr) -> do
        handle <- socketToHandle sock ReadWriteMode
        forkIO $ do
          threadDelay (2 * 10^6)
          mapM_ (sendMessage' outChan) (initial nick channels)
        forkIO (ircWriter outChan handle)
        runPlugins chans hooks
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

runPlugins :: Hook -> Plugins InMsg apps -> IO [ThreadId]
runPlugins _ PNil = return []
runPlugins original (plugin :> ps) = do
    t <- runPlugin original plugin
    ts <- runPlugins original ps
    return $ t : ts

runPlugin :: HasApp app (ReadState app) => Hook -> Plugin InMsg app -> IO ThreadId
runPlugin original (Plugin env _ w) = do
    (inChan, outChan) <- atomically $ duplicate original
    forkIO $ forever $ do
        msg <- atomically $ readTChan inChan
        void $ forkIO (runReaderT (w msg) (ReadState outChan env))
    where
        duplicate (a,b) = (,) <$> dupTChan a <*> dupTChan b
