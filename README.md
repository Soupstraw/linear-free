# Linear free monads

This package implements free monads on top of `linear-base`. Linear free monads
are useful to implement domain specific languages that for example have to keep 
track of resources.

## Motivating example

Let's define some data types.

```haskell
data ProductType
  = Eggs
  | Milk
  | Flour
  | Dough
  | Pancake
  deriving (Base.Eq, Base.Ord, Show)

newtype ProductId = ProductId Int

data RecipeF a where
  Buy :: ProductType -> (ProductId %1 -> a) %1 -> RecipeF a
  Combine :: ProductType -> [ProductId] %1 -> (ProductId %1 -> a) %1 -> RecipeF a
  Cook :: ProductType -> ProductId %1 -> (ProductId %1 -> a) %1 -> RecipeF a

instance Data.Functor RecipeF where
  fmap f (Buy t c) = Buy t $ f L.. c
  fmap f (Combine t cs c) = Combine t cs $ f L.. c
  fmap f (Cook t p c) = Cook t p $ f L.. c

instance Control.Functor RecipeF where
  fmap f (Buy t c) = Buy t $ f L.. c
  fmap f (Combine t cs c) = Combine t cs $ f L.. c
  fmap f (Cook t p c) = Cook t p $ f L.. c

type Recipe a = FreeL RecipeF a
```

We use `RecipeF` as the functor to implement our DSL. This language lets us 
write down cooking recipes by stating what needs to be bought and how combining
specific products we end up with something new.

Next let's implement some wrapper functions around the constructors to hide the
internal details of the implementation of our DSL.

```haskell
buy :: ProductType -> Recipe ProductId
buy t = liftF $ Buy t id

combine :: ProductType -> [ProductId] %1 -> Recipe ProductId
combine t ps = liftF $ Combine t ps id

cook :: ProductType -> ProductId %1 -> Recipe ProductId
cook t p = liftF $ Cook t p id
```

One way to interpret the AST is to gather all the ingredients that have to be 
bought in order to cook. For this we implement `gatherIngredients`:

```haskell
gatherIngredients :: Recipe a -> Map ProductType Int
gatherIngredients (Pure _) = Map.empty
gatherIngredients (Free x) = case x of
  Buy t c -> Map.insertWith (Base.+) t 1 $ gatherIngredients (c productId)
  Combine _ _ c -> gatherIngredients $ c productId
  Cook _ _ c -> gatherIngredients $ c productId
  where
    -- Doesn't matter what we put here, it will not be used anyways
    productId = ProductId 0
```

Now we can implement a recipe. The type checker won't let us use the eggs, milk 
and flour once we've combined them to make dough. Also, every ingredient that we
purchase has to be used in the recipe in some way.

```haskell
pancakeRecipe :: Recipe ProductId
pancakeRecipe = Linear.do
  eggs <- buy Eggs
  milk <- buy Milk
  flour <- buy Flour
  dough <- combine Dough [eggs, milk, flour]
  -- Eggs, milk and flour are no longer in scope from this point onwards
  pancake <- cook Pancake dough
  Linear.pure pancake
```
