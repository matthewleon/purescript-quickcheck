module Test.QuickCheck.LCG
  ( Seed
  , mkSeed
  , runSeed
  , runSeed'
  , lcgM
  , lcgC
  , lcgN
  , lcgNext
  , lcgPerturb
  , randomSeed
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomRange)

import Data.Int (toNumber, floor)

import Math ((%), abs)

-- | The *multiplier*: a magic constant for the linear congruential generator.
lcgM :: Number
lcgM = 25214903917.0

-- | The *increment*: a magic constant for the linear congruential generator.
lcgC :: Number
lcgC = 11.0

-- | The *modulus*: a magic constant for the linear congruential generator.
-- | 2 ** 48, as used in Java and POSIX random number generators.
lcgN :: Number
lcgN = 281474976710656.0

-- | Perturb a seed value
lcgPerturb :: Number -> Seed -> Seed
lcgPerturb d = Seed <<< go <<< runSeed'
  where
  go n = (lcgM * n + d) % lcgN

-- | Step the linear congruential generator
lcgNext :: Seed -> Seed
lcgNext = lcgPerturb lcgC

-- | Create a random seed
randomSeed :: forall e. Eff (random :: RANDOM | e) Seed
randomSeed = mkSeed <<< floor <$> randomRange 0.0 (toNumber top)

-- | A seed for the linear congruential generator.
newtype Seed = Seed Number

mkSeed :: Int -> Seed
mkSeed x = Seed $ abs $ toNumber x

runSeed :: Seed -> Int
--- because our seed is modulo 48, dividing by 2^16 gives us bits 17..48.
runSeed (Seed x) = floor (x / 65536.0 + toNumber bottom)

runSeed' :: Seed -> Number
runSeed' (Seed x) = x

instance showSeed :: Show Seed where
  show (Seed x) = "Seed " <> show x

instance eqSeed :: Eq Seed where
  eq (Seed x) (Seed y) = eq x y
