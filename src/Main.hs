{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
module Main where

import Network.Socket hiding (recv, send)
import System.IO (IOMode(..), hClose, Handle)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad (when, forever)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)
import Data.Monoid

data OutMsg =
    Nick !Text
  | User !Text !Text !Text !Text
  | Pong !Text

data InMsg =
    Ping !Text

data IrcInfo = IrcInfo {
    hostname :: String
  , port :: Int
  }

connectIrc :: IrcInfo -> IO ()
connectIrc IrcInfo{..} = do
  let hints = defaultHints
  addr:_ <- getAddrInfo (Just hints) (Just hostname) (Just (show port))
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  connected <- isConnected sock
  (inChan, outChan) <- atomically $ (,) <$> newTChan <*> newTChan
  when connected $ do
    handle <- socketToHandle sock ReadWriteMode
    mapM_ (sendMessage' outChan) initial
    _writer <- forkIO (ircWriter outChan handle)
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

parseLine :: Text -> Either Text InMsg
parseLine line =
  case T.words line of
       ("PING":_user:response) -> Right $ Ping (T.unwords response)
       _ -> Left line

ircReader :: TChan OutMsg -> TChan InMsg -> Handle -> IO ()
ircReader outChan inChan h = forever $ do
    msg <- (parseLine . T.strip) <$> T.hGetLine h
    case msg of
         Right (Ping response) -> atomically $ writeTChan outChan (Pong response)
         Right m -> atomically $ writeTChan inChan m
         Left line -> T.putStrLn $ "Unparsed " <> line

main :: IO ()
main = do
  putStrLn "hello world"
