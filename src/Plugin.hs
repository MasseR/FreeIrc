{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language RankNTypes #-}
{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language PolyKinds #-}
{-# Language TypeOperators #-}
{-# Language FunctionalDependencies #-}
module Plugin where

import Control.Monad.Reader
import Types

class HasApp app b | b -> app where
    getApp :: b -> app

data Plugin msg app = Plugin { app :: app
                             , clean :: app -> IO ()
                             , work :: msg -> ReaderT (ReadState app) IO () }

data Plugins msg (a :: [*]) where
    PNil :: Plugins msg '[]
    (:>) :: Plugin msg r -> Plugins msg rs -> Plugins msg (r ': rs)

infixr 2 :>
