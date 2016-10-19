{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveFunctor #-}
module Network.IRC where

import Control.Monad.Writer
import Control.Monad.Reader
-- import Control.Monad.State
import Network.Socket hiding (recv, send)
import System.IO (IOMode(..), hClose, Handle)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad (when, forever)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO, ThreadId)

data OutMsg =
    Nick !Text
  | User !Text !Text !Text !Text
  | Pong !Text
  | Join !Text
  deriving Show

data InMsg =
    Ping !Text
  | PrivMsg !Text !Text !Text
    deriving Show

data IrcInfo = IrcInfo {
    hostname :: String
  , port :: Int
  , channels :: [Text]
  , hooks :: HookBuilder ()
  }

type OutChannel = TChan OutMsg
type Hook = (TChan InMsg, TChan OutMsg)
newtype HookBuilder a = HookBuilder {unHook :: WriterT [ThreadId] (ReaderT Hook IO) a}
  deriving (Functor, Applicative, Monad, MonadWriter [ThreadId], MonadReader Hook, MonadIO)

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

initial :: [OutMsg]
initial = [Nick "Foobot", User "foo" "foo" "foo" "foo"]

sendMessage' :: TChan OutMsg -> OutMsg -> IO ()
sendMessage' out = atomically . writeTChan out

ircWriter :: TChan OutMsg -> Handle -> IO ()
ircWriter out handle = forever $ do
  msg <- atomically $ readTChan out
  let bs = renderMessage msg <> "\r\n"
  T.putStr bs
  T.hPutStr handle bs

renderMessage :: OutMsg -> Text
renderMessage msg =
  case msg of
       Nick nick -> T.unwords ["NICK", nick]
       User a b c d -> T.unwords ["USER", a, b, c, d]
       Pong response -> T.unwords ["PONG", response]
       Join channel -> T.unwords ["JOIN", channel]

parseLine :: Text -> Either Text InMsg
parseLine line =
  case T.words line of
       ("PING":response) -> Right $ Ping (T.unwords response)
       (source:"PRIVMSG":target:msg) -> Right $ PrivMsg (parseNick source) target (parseMsg . T.unwords $ msg)
       _ -> Left line
  where
    parseNick source = case T.splitOn "!" . T.tail $ source of
                              [nick,_] -> nick
                              _ -> ""
    parseMsg = T.tail

ircReader :: TChan OutMsg -> TChan InMsg -> Handle -> IO ()
ircReader outChan inChan h = forever $ do
    line <- T.strip <$> T.hGetLine h
    let msg = parseLine line
    print line
    print msg
    case msg of
         Right (Ping response) -> atomically $ writeTChan outChan (Pong response)
         Right m -> atomically $ writeTChan inChan m
         Left err -> T.putStrLn $ "Unparsed " <> err
