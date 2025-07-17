{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Control.Monad.Free.Linear (
  MonadFreeL (..),
  FreeL (..),
  iter,
  retract,
  hoistFree,
  foldFree,
  unfold,
) where

import qualified Control.Functor.Linear as Control
import Data.Functor (Functor)
import qualified Data.Functor.Linear as Data
import GHC.Generics (Generic)
import Prelude.Linear

class MonadFreeL f m where
  wrap :: f (m a) %1 -> m a

data FreeL f a where
  Pure :: a %1 -> FreeL f a
  Free :: f (FreeL f a) %1 -> FreeL f a
  deriving (Generic)

instance (Functor f, Data.Functor f) => Data.Functor (FreeL f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free m) = Free $ Data.fmap (Data.fmap f) m

instance (Functor f, Control.Functor f) => Control.Functor (FreeL f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free m) = Free $ Control.fmap (Control.fmap f) m

instance (Functor f, Control.Functor f, Data.Applicative f) => Data.Applicative (FreeL f) where
  {-# INLINE pure #-}
  pure = Pure

  Pure f <*> Pure x = Pure $ f x
  Pure f <*> Free mx = Free $ Control.fmap (Control.fmap f) mx
  Free mf <*> Pure x = Free $ Control.fmap ($ x) Control.<$> mf
  Free ma <*> Free mb = Free $ Control.fmap (Data.<*>) ma Data.<*> mb

instance (Functor f, Control.Applicative f) => Control.Applicative (FreeL f) where
  {-# INLINE pure #-}
  pure = Pure

  Pure f <*> Pure x = Pure $ f x
  Pure f <*> Free mx = Free $ Control.fmap (Control.fmap f) mx
  Free mf <*> Pure x = Free $ Control.fmap ($ x) Control.<$> mf
  Free ma <*> Free mb = Free $ Control.fmap (Control.<*>) ma Control.<*> mb

instance (Functor f, Control.Applicative f) => Control.Monad (FreeL f) where
  Pure x >>= f = f x
  Free m >>= f = Free ((Control.>>= f) Control.<$> m)

instance MonadFreeL f (FreeL f) where
  {-# INLINE wrap #-}
  wrap = Free

retract :: Control.Monad f => FreeL f a %1 -> f a
retract (Pure x) = Control.pure x
retract (Free m) = m Control.>>= retract

iter :: Control.Functor f => (f a %1 -> a) -> FreeL f a %1 -> a
iter _ (Pure x) = x
iter f (Free m) = f $ iter f Control.<$> m

hoistFree ::
  Control.Functor g => (forall a. f a %1 -> g a) -> FreeL f b %1 -> FreeL g b
hoistFree _ (Pure x) = Pure x
hoistFree f (Free m) = Free $ hoistFree f Control.<$> f m

foldFree :: Control.Monad m => (forall x. f x %1 -> m x) -> FreeL f a %1 -> m a
foldFree _ (Pure x) = Control.pure x
foldFree f (Free m) = f m Control.>>= foldFree f

unfold :: Control.Functor f => (b %1 -> Either a (f b)) -> b %1 -> FreeL f a
unfold f b = case f b of
  Left x -> Pure x
  Right m -> Free $ unfold f Control.<$> m
