module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array.Partial (head)
import Data.Foldable (sum)
import Data.Maybe (fromJust)
import Data.NonEmpty (NonEmpty(..))
import Math.Statistics.Test.ChiSquared.Pearson (pearsonTest, Alpha, alpha)
import Math.Statistics.Test.ChiSquared.Pearson.Buckets (discreteUniformBucketize, uniformBucketize', Range, range)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, evalGen, randomSample', uniform, vectorOf)
import Test.QuickCheck.LCG (randomSeed)

main :: Eff (console :: CONSOLE, random :: RANDOM) Unit
main = do
  log "Try with some little Gens first"
  logShow =<< go 10
  logShow =<< go 100
  logShow =<< go 1000
  logShow =<< go 10000

  log "Testing stack safety of Gen"
  logShow =<< go 20000
  logShow =<< go 100000

  log "chi squared test of uniform generator"
  uniformNums' <- uniformNums
  logShow $
    pearsonTest permissiveAlpha $
      uniformBucketize' uniformNums' 100 uniformRange

  log "chi squared test of chooseInt generator"
  uniformInts' <- uniformInts
  logShow $
    pearsonTest permissiveAlpha $ discreteUniformBucketize uniformInts'

  where
  go n = map (sum <<< unsafeHead) $ randomSample' 1 (vectorOf n (arbitrary :: Gen Int))

  unsafeHead :: forall x. Array x -> x
  unsafeHead xs = unsafePartial (head xs)

  permissiveAlpha :: Alpha
  permissiveAlpha = unsafePartial $ fromJust $ alpha 0.001

  uniformRange :: Range
  uniformRange = unsafePartial $ fromJust $ range 0.0 1.0

  uniformNums :: forall e. Eff (random :: RANDOM | e) (NonEmpty Array Number)
  uniformNums =
    (\seed -> evalGen genUniformNums {newSeed: seed, size: 10}) <$> randomSeed

  genUniformNums :: Gen (NonEmpty Array Number)
  genUniformNums = NonEmpty <$> uniform <*> vectorOf 99999 uniform

  uniformInts :: forall e. Eff (random :: RANDOM | e) (NonEmpty Array Int)
  uniformInts =
    (\seed -> evalGen genUniformInts {newSeed: seed, size: 10}) <$> randomSeed

  genUniformInts :: Gen (NonEmpty Array Int)
  genUniformInts = NonEmpty <$> (chooseInt bottom top) <*> vectorOf 9999 (chooseInt bottom top)
