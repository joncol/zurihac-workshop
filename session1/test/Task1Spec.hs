module Task1Spec where

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Task1

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

  it "allows any energy card to cover a `Colorless` cost" $ do
    let cards =
          [ EnergyCard Grass
          , EnergyCard Fire
          , EnergyCard Water
          , EnergyCard Lightning
          , grookey
          , goomy
          , eevee
          ]
    enoughEnergy [Grass, Fire, Water, Colorless] cards `shouldBe` True

  it "returns `False` when there are not enough cards to cover the `Colorless` cost" $ do
    let cards =
          [ EnergyCard Grass
          , EnergyCard Fire
          , EnergyCard Water
          , EnergyCard Lightning
          , grookey
          , goomy
          , eevee
          ]
    enoughEnergy [Grass, Fire, Water, Colorless, Colorless] cards `shouldBe` False

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

  it "returns missing cards when there are both colored and colorless cards missing" $ do
    let cards =
          [ EnergyCard Grass
          , EnergyCard Fire
          , EnergyCard Water
          ]
    missingEnergy [Grass, Fire, Water, Lightning, Colorless, Colorless, Colorless] cards
      `shouldBe` Just [Lightning, Colorless, Colorless, Colorless]

  it "uses extra colored cards for colorless cost" $ do
    let cards =
          [ EnergyCard Grass
          , EnergyCard Fire
          , EnergyCard Water
          , EnergyCard Psychic -- extra
          ]
    missingEnergy [Grass, Fire, Water, Lightning, Metal, Colorless, Colorless, Colorless] cards
      `shouldBe` Just [Lightning, Metal, Colorless, Colorless]
