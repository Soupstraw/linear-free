module Control.Linear.Applicative.Free (
  Ap (..),
  runApD,
  runAp,
  runAp_,
  liftAp,
  iterAp,
  hoistAp,
  retractAp,
) where

import qualified Control.Functor.Linear as Control
import qualified Data.Functor.Linear as Data
import Prelude.Linear

data Ap f a where
  Pure :: a %1 -> Ap f a
  Ap :: f a %1 -> Ap f (a %1 -> b) %1 -> Ap f b

instance Data.Functor f => Data.Functor (Ap f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Ap x mf) = Ap x $ (f .) Data.<$> mf

instance Control.Functor f => Control.Functor (Ap f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Ap x mf) = Ap x $ ((.) f) Control.<$> mf

instance Control.Functor f => Data.Applicative (Ap f) where
  pure = Pure
  (Pure f) <*> x = Control.fmap f x
  (Ap y f) <*> x = Ap y ((flip Control.<$> f) Data.<*> x)

instance Control.Functor f => Control.Applicative (Ap f) where
  pure = Pure
  (Pure f) <*> x = Control.fmap f x
  (Ap y f) <*> x = Ap y ((flip Control.<$> f) Data.<*> x)

runApD :: Data.Applicative g => (forall x. f x %1 -> g x) -> Ap f a -> g a
runApD _ (Pure x) = Data.pure x
runApD f (Ap x g) = runApD f g Data.<*> f x

runAp :: Control.Applicative g => (forall x. f x %1 -> g x) -> Ap f a %1 -> g a
runAp _ (Pure x) = Control.pure x
runAp f (Ap x g) = runAp f g Control.<*> f x

runAp_ :: Monoid m => (forall a. f a -> m) -> Ap f b -> m
runAp_ _ (Pure _) = mempty
runAp_ f (Ap x g) = f x <> runAp_ f g

liftAp :: f a %1 -> Ap f a
liftAp x = Ap x $ Pure id

iterAp :: Control.Functor g => (g a %1 -> a) -> Ap g a %1 -> a
iterAp _ (Pure x) = x
iterAp f (Ap x g) = f (iterAp f . ((Control.<*>) g) . Pure Control.<$> x)

hoistAp :: (forall a. f a %1 -> g a) -> Ap f b %1 -> Ap g b
hoistAp _ (Pure x) = Pure x
hoistAp f (Ap x g) = Ap (f x) (hoistAp f g)

retractAp :: Control.Applicative f => Ap f a %1 -> f a
retractAp (Pure x) = Control.pure x
retractAp (Ap x g) = retractAp g Control.<*> x
