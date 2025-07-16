{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Free.Linear () where

import qualified Control.Functor.Linear as Control
import Data.Functor (Functor)
import qualified Data.Functor.Linear as Data
import Prelude.Linear

data Free f a where
  Pure :: a %1 -> Free f a
  Free :: f (Free f a) %1 -> Free f a

instance (Functor f, Data.Functor f) => Data.Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free m) = Free $ Data.fmap (Data.fmap f) m

instance (Functor f, Control.Functor f) => Control.Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free m) = Free $ Control.fmap (Control.fmap f) m

instance (Functor f, Control.Functor f, Data.Applicative f) => Data.Applicative (Free f) where
  {-# INLINE pure #-}
  pure = Pure
  Pure f <*> Pure x = Pure $ f x
  Pure f <*> Free mx = Free $ Control.fmap (Control.fmap f) mx
  Free mf <*> Pure x = Free $ Control.fmap ($ x) Control.<$> mf
  Free ma <*> Free mb = Free $ Control.fmap (Data.<*>) ma Data.<*> mb

instance (Functor f, Control.Applicative f) => Control.Applicative (Free f) where
  {-# INLINE pure #-}
  pure = Pure
  Pure f <*> Pure x = Pure $ f x
  Pure f <*> Free mx = Free $ Control.fmap (Control.fmap f) mx
  Free mf <*> Pure x = Free $ Control.fmap ($ x) Control.<$> mf
  Free ma <*> Free mb = Free $ Control.fmap (Control.<*>) ma Control.<*> mb

instance (Functor f, Control.Applicative f) => Control.Monad (Free f) where
  Pure x >>= f = f x
  Free m >>= f = Free ((Control.>>= f) Control.<$> m)
