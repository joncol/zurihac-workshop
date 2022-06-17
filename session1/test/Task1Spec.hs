module Task1Spec where

import Data.List
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Task1

spec_prelude :: Spec
spec_prelude = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [23 ..] `shouldBe` (23 :: Int)
