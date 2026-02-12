{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import qualified Control.Functor.Linear as Control
import qualified Control.Functor.Linear as Linear
import qualified Control.Linear.Monad.Free as Linear
import qualified Data.Functor.Linear as Data
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude.Linear hiding ((.))
import qualified Prelude.Linear as L
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified Prelude as Base

data ProductType
  = Eggs
  | Milk
  | Flour
  | Dough
  | Pancakes
  deriving (Base.Eq, Base.Ord, Show)

newtype ProductId = ProductId Int

data RecipeF a where
  Buy :: ProductType -> (ProductId %1 -> a) %1 -> RecipeF a
  Combine :: ProductType -> [ProductId] %1 -> (ProductId %1 -> a) %1 -> RecipeF a
  Cook :: ProductType -> ProductId %1 -> (ProductId %1 -> a) %1 -> RecipeF a

instance Data.Functor RecipeF where
  fmap = forget Control.fmap

instance Control.Functor RecipeF where
  fmap f (Buy t c) = Buy t $ f L.. c
  fmap f (Combine t cs c) = Combine t cs $ f L.. c
  fmap f (Cook t p c) = Cook t p $ f L.. c

type Recipe a = Linear.Free RecipeF a

buy :: ProductType -> Recipe ProductId
buy t = Linear.liftF $ Buy t id

combine :: ProductType -> [ProductId] %1 -> Recipe ProductId
combine t ps = Linear.liftF $ Combine t ps id

cook :: ProductType -> ProductId %1 -> Recipe ProductId
cook t p = Linear.liftF $ Cook t p id

pancakeRecipe :: Recipe ProductId
pancakeRecipe = Linear.do
  eggs <- buy Eggs
  milk <- buy Milk
  flour <- buy Flour
  dough <- combine Dough [eggs, milk, flour]
  pancake <- cook Pancakes dough
  Linear.pure pancake

gatherIngredients :: Recipe a -> Map ProductType Int
gatherIngredients (Linear.Pure _) = Map.empty
gatherIngredients (Linear.Free x) = case x of
  Buy t c -> Map.insertWith (Base.+) t 1 $ gatherIngredients (c productId)
  Combine _ _ c -> gatherIngredients $ c productId
  Cook _ _ c -> gatherIngredients $ c productId
  where
    -- Doesn't matter what we put here, it will not be used anyways
    productId = ProductId 0

main :: IO ()
main = hspec $ describe "Linear free monad" $ do
  describe "pancake recipe example" $ do
    it "can gather the ingredients" $ do
      gatherIngredients pancakeRecipe
        `shouldBe` Map.fromList
          [ (Eggs, 1)
          , (Milk, 1)
          , (Flour, 1)
          ]
