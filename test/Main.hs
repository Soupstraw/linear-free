module Main (main) where

import Test.Hspec (describe, hspec)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec . describe "Linear free monad" $ do
  undefined
