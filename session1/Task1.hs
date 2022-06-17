{-# LANGUAGE OverloadedStrings #-}

module Task1 where

import Data.Text
import GHC.Natural

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

data Card
  = PokemonCard
      { name :: Text
      , typ :: Energy
      , hp :: Natural
      , attacks :: [Attack]
      }
  | EnergyCard {typ :: Energy}

data Attack = Attack
  { attackName :: Text
  , cost :: [Energy]
  , damage :: Natural
  }

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
enoughEnergy cost attached = _

-- Then, refine it to return the missing energy
missingEnergy :: [Energy] -> [Card] -> Maybe [Energy]
missingEnergy cost attached = _
