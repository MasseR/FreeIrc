{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
module Hooks.AutoFree where

import Data.Functor.Sum
import Data.Proxy
import Control.Monad.Free
import Control.Monad.Trans (MonadIO)

class Interpreted (f :: * -> *) where
    interpret :: MonadIO m => f a -> m a

instance Interpreted f => Interpreted (Free f) where
    interpret f = foldFree interpret f

instance (Interpreted f, Interpreted g) => Interpreted (Sum f g) where
    interpret (InL f) = interpret f
    interpret (InR g) = interpret g

class AutoHoister (x :: * -> *) (f :: [* -> *]) where
    type Eff f :: * -> *
    autoHoist :: x a -> Proxy f -> Free (Eff f) a

instance AutoHoister (Free f) ('[f] :: [* -> *]) where
    type Eff '[f] = f
    autoHoist f _ = f

instance (Functor f, Functor (Eff (g ': xs))) => AutoHoister (Free f) ((f :: * -> *) ': g ': xs) where
    type Eff (f ': g ': xs) = Sum f (Eff (g ': xs))
    autoHoist f _ = hoistFree InL f

instance (Functor f, Functor (Eff (g ': xs)), AutoHoister (Free g) (g ': xs)) => AutoHoister (Free g) (f ': g ': xs) where
    type Eff (f ': g ': xs) = Sum f (Eff (g ': xs))
    autoHoist g _ = hoistFree InR (autoHoist g p)
        where
            p :: Proxy (g ': xs)
            p = Proxy
