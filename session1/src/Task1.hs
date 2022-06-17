{-# LANGUAGE OverloadedStrings #-}

module Task1 where

import qualified Data.MultiSet as MS
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Natural
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()

data Energy
  = Colorless
  | Grass
  | Fire
  | Water
  | Lightning
  | Fighting
  | Psychic
  | Darkness
  | Metal
  | Dragon
  deriving (Generic, Eq, Ord, Show)
  deriving (Arbitrary) via GenericArbitrary Energy

data Card
  = PokemonCard
      { name :: Text
      , typ :: Energy
      , hp :: Natural
      , attacks :: [Attack]
      }
  | EnergyCard {typ :: Energy}
  deriving (Generic, Show)
  deriving (Arbitrary) via GenericArbitrary Card

data Attack = Attack
  { attackName :: Text
  , cost :: [Energy]
  , damage :: Natural
  }
  deriving (Generic, Show)
  deriving (Arbitrary) via GenericArbitrary Attack

-- Define values for the following cards

-- | https://pokemoncard.io/card/?search=swsh1-11
grookey :: Card
grookey =
  PokemonCard
    { name = "Grookey"
    , typ = Grass
    , hp = 70
    , attacks =
        [ Attack
            { attackName = "Scratch"
            , cost = [Colorless]
            , damage = 10
            }
        , Attack
            { attackName = "Beat"
            , cost = [Grass, Colorless]
            , damage = 20
            }
        ]
    }

-- | https://pokemoncard.io/card/?search=swsh8-195
goomy :: Card
goomy =
  PokemonCard
    { name = "Goomy"
    , typ = Dragon
    , hp = 50
    , attacks =
        [ Attack
            { attackName = "Tackle"
            , cost = [Colorless]
            , damage = 10
            }
        , Attack
            { attackName = "Melt"
            , cost = [Water, Psychic]
            , damage = 20
            }
        ]
    }

-- | https://pokemoncard.io/card/?search=swsh4-130
eevee :: Card
eevee =
  PokemonCard
    { name = "Eevee"
    , typ = Colorless
    , hp = 70
    , attacks =
        [ Attack
            { attackName = "Gnaw"
            , cost = [Colorless]
            , damage = 10
            }
        , Attack
            { attackName = "Tail Whap"
            , cost = [Colorless, Colorless]
            , damage = 20
            }
        ]
    }

{- | Check whether some energy cards are enough to
   "pay" for the cost of an attack
-}
enoughEnergy :: [Energy] -> [Card] -> Bool
enoughEnergy energyCost attached = MS.fromList energyCost `MS.isSubsetOf` energies
  where
    energies = MS.fromList $ [c | EnergyCard c <- attached]

-- Then, refine it to return the missing energy
missingEnergy :: [Energy] -> [Card] -> Maybe [Energy]
missingEnergy cost attached = _
