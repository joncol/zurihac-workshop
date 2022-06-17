module Task1Spec where

import Data.List
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Task1

-- spec_enoughEnergy :: Spec
-- spec_enoughEnergy = describe "enoughEnergy" $ do
--   it "returns `True` when cost is zero" $ do
--     enoughEnergy [] ([Card]) `shouldBe` (23 :: Int)

prop_enoughEnergyAlwaysReturnsTrueWhenCostIsZero :: [Card] -> Bool
prop_enoughEnergyAlwaysReturnsTrueWhenCostIsZero cards = enoughEnergy [] cards

prop_enoughEnergyAlwaysReturnsFalseForEmptyHandWhenCostIsNonZero :: [Energy] -> Bool
prop_enoughEnergyAlwaysReturnsFalseForEmptyHandWhenCostIsNonZero cost =
  not $ enoughEnergy cost []
