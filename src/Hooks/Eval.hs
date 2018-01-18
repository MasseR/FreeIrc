{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language FlexibleContexts #-}
module Hooks.Eval where

import ClassyPrelude
import Data.Conduit.Process
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as Text
import Network.IRC
import Hooks.Algebra
import Types
import Control.Monad.Logger

type Expression = String

mueval :: (MonadThrow m, MonadIO m) => Expression -> m [Text]
mueval expr = snd <$> sourceProcessWithConsumer p consumer
    where
        p = proc "mueval" ["--expression", expr]
        consumer = Text.decodeUtf8 .| Text.lines .| CL.consume

-- XXX: Do a monadlogger instance with mmorph and monadcontrol
djinn :: (MonadThrow m, MonadIO m) => Text -> m [Text]
djinn expr = snd' <$> liftIO (sourceProcessWithStreams p (yield $ encodeUtf8 expr) consumer C.stderr)
    where
        snd' (_,x,_) = x
        p = proc "djinn" ["/dev/stdin"]
        consumer = Text.decodeUtf8 .| Text.lines .| CL.consume



evalHook (PrivMsg nick target msg) =
    case words msg of
         (">" : rest) -> mueval (unpack $ unwords rest) >>= respond
         (">>" : rest) -> djinn ("? " <> unwords rest) >>= respond
    where
        respond [] = return ()
        respond xs = mapM_ (respondTo nick target . take 1024) . take 3 $ xs
