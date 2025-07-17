{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Free.Linear (
  MonadFreeL (..),
  FreeL (..),
  iter,
  retract,
  hoistFree,
  foldFree,
  unfold,
  liftF,
) where

import qualified Control.Functor.Linear as Control
import qualified Data.Functor.Linear as Data
import GHC.Generics (Generic)
import Prelude.Linear

class Control.Monad m => MonadFreeL f m where
  wrap :: f (m a) %1 -> m a

data FreeL f a where
  Pure :: a %1 -> FreeL f a
  Free :: f (FreeL f a) %1 -> FreeL f a
  deriving (Generic)

instance Data.Functor f => Data.Functor (FreeL f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free m) = Free $ Data.fmap (Data.fmap f) m

instance Control.Functor f => Control.Functor (FreeL f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free m) = Free $ Control.fmap (Control.fmap f) m

instance Control.Functor f => Data.Applicative (FreeL f) where
  {-# INLINE pure #-}
  pure = Pure

  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ Control.fmap a Control.<$> mb
  Free ma <*> b = Free $ (Control.<*> b) Control.<$> ma

instance Control.Functor f => Control.Applicative (FreeL f) where
  {-# INLINE pure #-}
  pure = Pure

  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ Control.fmap a Control.<$> mb
  Free ma <*> b = Free $ (Control.<*> b) Control.<$> ma

instance Control.Functor f => Control.Monad (FreeL f) where
  Pure x >>= f = f x
  Free m >>= f = Free ((Control.>>= f) Control.<$> m)

instance Control.Functor f => MonadFreeL f (FreeL f) where
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

liftF :: (Control.Functor f, MonadFreeL f m) => f a %1 -> m a
liftF = wrap . Control.fmap Control.pure
