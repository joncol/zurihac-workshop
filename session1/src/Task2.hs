{-# LANGUAGE DeriveAnyClass #-}

module Task2 where

import GHC.Generics
import GHC.Natural
import System.Random
import System.Random.Stateful

data FlipOutcome
  = Heads
  | Tails
  deriving (Show, Generic, Finite, Uniform)

data Action
  = FlipCoin (FlipOutcome -> Action)
  | Damage Natural

surpriseAttackAction :: Action
surpriseAttackAction =
  FlipCoin $ \case
    Heads -> Damage 30
    Tails -> Damage 0

{- | Define Pikachu's "Iron Tail" attack

   > Flip a coin until you get tails.
   > This attack does 30 damage for each heads.
-}
ironTailAction :: Action
ironTailAction = buildDamage 0
  where
    buildDamage :: Natural -> Action
    buildDamage n =
      FlipCoin $ \case
        Heads -> buildDamage (n + 1)
        Tails -> Damage $ n * 30

-- | Define the randomness interpretation of 'Action'
interpretRandom :: Action -> IO Natural
interpretRandom (FlipCoin f) = do
  putStrLn "\nflipping coin"
  coin <- flipCoin
  putStrLn $ "result: " <> show coin
  interpretRandom (f coin)
  where
    flipCoin :: IO FlipOutcome
    flipCoin = uniformM globalStdGen
interpretRandom (Damage n) = do
  putStrLn $ "Doing damage: " <> show n
  pure n
