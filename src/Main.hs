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
  | Join !Text
  deriving Show

data InMsg =
    Ping !Text
  | PrivMsg !Text !Text
    deriving Show

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
       Join channel -> T.unwords ["JOIN", channel]

parseLine :: Text -> Either Text InMsg
parseLine line =
  case T.words line of
       ("PING":response) -> Right $ Ping (T.unwords response)
       (from:"PRIVMSG":_target:msg) -> Right $ PrivMsg from (T.unwords msg)
       _ -> Left line

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

main :: IO ()
main = do
  putStrLn "hello world"
