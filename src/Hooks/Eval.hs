{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell #-}
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
djinn :: (MonadThrow m, MonadLogger m, MonadIO m) => Text -> m [Text]
djinn expr = do
    $logInfo ("Evaluating " <> expr)
    (st,x,_) <- liftIO (sourceProcessWithStreams p (yield $ encodeUtf8 (expr <> "\n")) consumer C.stderr)
    $logInfo ("Evaluated with " <> pack (show st))
    return x
    where
        snd' (_,x,_) = x
        p = proc "djinn" ["/dev/stdin"]
        consumer = Text.decodeUtf8 .| Text.lines .| CL.consume


data EvalState = EvalState

evalHook (PrivMsg nick target msg) = do
    $logInfo msg
    case words msg of
         (">" : rest) -> do
             $logInfo ("Trying to eval " <> unwords rest)
             mueval (unpack $ unwords rest) >>= respond
         (">>" : rest) -> do
             $logInfo ("Trying to djinn " <> unwords rest)
             djinn ("? " <> unwords rest) >>= respond
         _ -> return ()
    where
        respond [] = return ()
        respond xs = mapM_ (respondTo nick target . take 1024) . take 3 $ xs
