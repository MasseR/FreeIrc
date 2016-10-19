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
  out <- atomically $ newTChan
  when connected $ do
    handle <- socketToHandle sock ReadWriteMode
    mapM_ (sendMessage' out) initial
    _writer <- forkIO (ircWriter out handle)
    ircReader handle
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
       Nick nick -> T.unwords ["Nick", nick]
       User a b c d -> T.unwords ["USER", a, b, c, d]

ircReader :: Handle -> IO ()
ircReader h = forever $ do
    line <- T.hGetLine h
    print line

main :: IO ()
main = do
  putStrLn "hello world"
