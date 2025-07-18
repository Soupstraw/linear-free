{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Free.Linear (
  MonadFree (..),
  Free (..),
  iter,
  retract,
  hoistFree,
  foldFree,
  unfold,
  liftF,
) where

import qualified Control.Functor.Linear as Control
import Data.Functor.Classes (Eq1 (..))
import qualified Data.Functor.Linear as Data
import GHC.Generics (Generic, Generic1)
import Prelude.Linear
import qualified Prelude as Base

class Control.Monad m => MonadFree f m where
  wrap :: f (m a) %1 -> m a

data Free f a where
  Pure :: a %1 -> Free f a
  Free :: f (Free f a) %1 -> Free f a
  deriving (Generic, Generic1)

instance Data.Functor f => Data.Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free m) = Free $ Data.fmap (Data.fmap f) m

instance Control.Functor f => Control.Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free m) = Free $ Control.fmap (Control.fmap f) m

instance Control.Functor f => Data.Applicative (Free f) where
  {-# INLINE pure #-}
  pure = Pure

  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ Control.fmap a Control.<$> mb
  Free ma <*> b = Free $ (Control.<*> b) Control.<$> ma

instance Control.Functor f => Control.Applicative (Free f) where
  {-# INLINE pure #-}
  pure = Pure

  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ Control.fmap a Control.<$> mb
  Free ma <*> b = Free $ (Control.<*> b) Control.<$> ma

instance Control.Functor f => Control.Monad (Free f) where
  Pure x >>= f = f x
  Free m >>= f = Free ((Control.>>= f) Control.<$> m)

instance Control.Functor f => MonadFree f (Free f) where
  {-# INLINE wrap #-}
  wrap = Free

instance Data.Traversable f => Data.Traversable (Free f) where
  traverse f (Pure x) = Pure Data.<$> f x
  traverse f (Free m) = Free Data.<$> Data.traverse (Data.traverse f) m

retract :: Control.Monad f => Free f a %1 -> f a
retract (Pure x) = Control.pure x
retract (Free m) = m Control.>>= retract

iter :: Control.Functor f => (f a %1 -> a) -> Free f a %1 -> a
iter _ (Pure x) = x
iter f (Free m) = f $ iter f Control.<$> m

hoistFree ::
  Control.Functor g => (forall a. f a %1 -> g a) -> Free f b %1 -> Free g b
hoistFree _ (Pure x) = Pure x
hoistFree f (Free m) = Free $ hoistFree f Control.<$> f m

foldFree :: Control.Monad m => (forall x. f x %1 -> m x) -> Free f a %1 -> m a
foldFree _ (Pure x) = Control.pure x
foldFree f (Free m) = f m Control.>>= foldFree f

unfold :: Control.Functor f => (b %1 -> Either a (f b)) -> b %1 -> Free f a
unfold f b = case f b of
  Left x -> Pure x
  Right m -> Free $ unfold f Control.<$> m

liftF :: (Control.Functor f, MonadFree f m) => f a %1 -> m a
liftF = wrap . Control.fmap Control.pure
