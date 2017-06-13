{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language RankNTypes #-}
{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language PolyKinds #-}
{-# Language TypeOperators #-}
module Plugin where

import Hooks.Algebra
import Control.Monad.Freer

data Plugin (xs :: [* -> *]) msg app
    = Plugin { app :: app
             , clean :: app -> IO ()
             , work :: msg -> Eff xs () }

data Plugins xs msg (a :: [*]) where
    PNil :: Plugins xs msg '[]
    (:>) :: Plugin xs msg r -> Plugins xs msg rs -> Plugins xs msg (r ': rs)

infixr 2 :>
