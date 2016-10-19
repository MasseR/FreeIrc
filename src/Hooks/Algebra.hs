{-# Language DeriveFunctor #-}
module Hooks.Algebra where

import Control.Monad.Free
import Network.IRC

data HookF a =
  SendMessage OutMsg a
