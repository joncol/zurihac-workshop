module Task1Spec where

import Data.List
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Task1
import Task1 (goomy)

prop_enoughEnergyAlwaysReturnsTrueWhenCostIsZero :: [Card] -> Bool
prop_enoughEnergyAlwaysReturnsTrueWhenCostIsZero = enoughEnergy []

prop_enoughEnergyAlwaysReturnsFalseForEmptyHandWhenCostIsNonZero :: NonEmptyList Energy -> Bool
prop_enoughEnergyAlwaysReturnsFalseForEmptyHandWhenCostIsNonZero (NonEmpty energyCost) =
  not $ enoughEnergy energyCost []

spec_enoughEnergy :: Spec
spec_enoughEnergy = describe "enoughEnergy" $ do
  it "returns `True` when hand has enough energy cards to cover cost" $ do
    let cards =
          [ EnergyCard Grass
          , EnergyCard Fire
          , EnergyCard Water
          , grookey
          , goomy
          , eevee
          ]
    enoughEnergy [Grass, Fire, Water] cards `shouldBe` True

  it "returns `False` when hand does not have enough energy cards to cover cost" $ do
    let cards =
          [ EnergyCard Grass
          , EnergyCard Fire
          , EnergyCard Water
          , grookey
          , goomy
          , eevee
          ]
    enoughEnergy [Grass, Fire, Fire, Water, Water] cards `shouldBe` False

spec_missingEnergy :: Spec
spec_missingEnergy = describe "missingEnergy" $ do
  it "returns `Nothing` when hand has enough energy cards to cover cost" $ do
    let cards =
          [ EnergyCard Grass
          , EnergyCard Fire
          , EnergyCard Water
          , grookey
          , goomy
          , eevee
          ]
    missingEnergy [Grass, Fire, Water] cards `shouldBe` Nothing

  it "returns the missing `Energy` when hand does not have enough energy cards to cover cost" $ do
    let cards =
          [ EnergyCard Grass
          , EnergyCard Fire
          , EnergyCard Water
          , grookey
          , goomy
          , eevee
          ]
    missingEnergy [Grass, Fire, Fire, Water, Water] cards `shouldBe` Just [Fire, Water]
