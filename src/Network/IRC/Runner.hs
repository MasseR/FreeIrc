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

data IrcInfo effs ps = IrcInfo {
    hostname :: String
  , port :: !Int
  , nick :: !Text
  , channels :: [Text]
  , hooks :: Plugins effs OutMsg ps
  }


-- newtype HookBuilder app a = HookBuilder {unHook :: WriterT [ThreadId] (ReaderT (Hook app) IO)  a}
--   deriving (Functor, Applicative, Monad, MonadWriter [ThreadId], MonadReader Hook, MonadIO)

-- connectIrc :: IrcInfo -> IO ()
-- connectIrc i@IrcInfo{..} = newAcid hostname (connectIrc' i)

connectIrc :: IrcInfo effs ps -> IO ()
connectIrc IrcInfo{..} = do
    chans@(inChan, outChan) <- (,) <$> atomically newTChan <*> atomically newTChan
    connect hostname (show port) $ \(sock, addr) -> do
        handle <- socketToHandle sock ReadWriteMode
        forkIO $ do
          threadDelay (2 * 10^6)
          mapM_ (sendMessage' outChan) (initial nick channels)
        forkIO (ircWriter outChan handle)
        -- runReaderT (execWriterT . unHook $ hooks) chans
        ircReader outChan inChan handle
        hClose handle

-- newAcid :: String -> (AcidState IrcState -> IO ()) -> IO ()
-- newAcid host f = bracket (openLocalStateFrom ("stateFromHost/"<>host) initialIrcState)
--                        (createCheckpointAndClose)
--                        (\acid -> f acid)

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

-- runPlugin :: Hook -> Plugin '[IrcF OutMsg, ReaderT (ReadState app) IO] InMsg app -> IO ThreadId
runPlugin original plugin = do
    (inChan, outChan) <- atomically $ duplicate original
    forkIO $ forever $ do
        msg <- atomically $ readTChan inChan
        let w = Plugin.work plugin
            env = Plugin.app plugin
        undefined
        -- void $ forkIO (runReaderT (runM . runIrc $ w msg) (ReadState outChan env))
    where
        duplicate (a,b) = (,) <$> dupTChan a <*> dupTChan b

-- newHook :: (InMsg -> Eff '[IrcF OutMsg, ReaderT ReadState IO] ()) -> HookBuilder ()
-- newHook f = do
--   original <- ask
--   (inChan, outChan) <- liftIO $ atomically $ duplicate original
--   t <- liftIO $ forkIO $ forever $ do
--     msg <- atomically $ readTChan inChan
--     forkIO (runReaderT (runM . runIrc $ (f msg)) (ReadState outChan))
--   tell [t]
--   where
--     duplicate (a,b) = (,) <$> dupTChan a <*> dupTChan b
