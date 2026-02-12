{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Linear.Monad.Free.Church (
  F (..),
  runF,
  fromF,
  improve,
) where

import qualified Control.Functor.Linear as Control
import Control.Linear.Monad.Free (Free, MonadFree (..))
import qualified Data.Functor.Linear as Data
import Prelude.Linear
import qualified Prelude.Linear as L

newtype F f a where
  F :: (forall r. (a %1 -> r) %1 -> (f r %1 -> r) -> r) %1 -> F f a

runF :: F f a %1 -> (a %1 -> r) %1 -> (f r %1 -> r) -> r
runF (F m) p b = m p b

instance Data.Functor f => Data.Functor (F f) where
  fmap f (F m) = F (\p b -> m (p . f) b)

instance Data.Functor f => Control.Functor (F f) where
  fmap f (F m) = F (\p b -> m (p . f) b)

instance Data.Functor f => Data.Applicative (F f) where
  {-# INLINE pure #-}
  pure x = F (\p _ -> p x)

  F f <*> F g = F (\p b -> f (\a -> g (p L.. a) b) b)

instance Data.Functor f => Control.Applicative (F f) where
  {-# INLINE pure #-}
  pure x = F (\p _ -> p x)

  F f <*> F g = F (\p b -> f (\a -> g (p L.. a) b) b)

instance Data.Functor f => Control.Monad (F f) where
  F m >>= f = F (\p b -> m (\x -> runF (f x) p b) b)

instance Control.Functor f => MonadFree f (F f) where
  wrap f = F (\p b -> b (Control.fmap (\(F m) -> m p b) f))

instance (Control.Functor f, Data.Traversable f) => Data.Traversable (F f) where
  traverse f m = runF m (Data.fmap Control.return . f) (Data.fmap wrap . Data.sequenceA)

fromF :: forall m f a. MonadFree f m => F f a %1 -> m a
fromF (F m) = m Control.return wrap

improve ::
  forall f a.
  Control.Functor f =>
  (forall m. MonadFree f m => m a) %1 ->
  Free f a
improve f = fromF @_ @f f
