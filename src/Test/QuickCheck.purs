-- | This module is a partial port of the Haskell QuickCheck library.
-- |
-- | QuickCheck provides a way to write _property-based_ tests.
-- |
-- | The `Arbitrary` and `CoArbitrary` type classes allow us to create
-- | random data with which we can run our tests. This module provides
-- | instances of both classes for PureScript's core data structures,
-- | as well as functions for writing new instances.
-- |
-- | Test suites can use the `quickCheck` and `quickCheckPure` functions
-- | to test properties.
-- |
-- | For example:
-- |
-- | ```purescript
-- | main = quickCheck \n -> n + 1 > n
-- | ```
module Test.QuickCheck
  ( QC
  , quickCheck
  , quickCheck'
  , quickCheckWithSeed
  , quickCheckPure
  , Prop(..)
  , Result(..)
  , withHelp
  , (<?>)
  , assertEquals
  , (===)
  , assertNotEquals
  , (/==)
  , module Test.QuickCheck.LCG
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Rec.Class (Step(..), tailRec)

import Data.Foldable (for_)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)

import Test.QuickCheck.Gen (Gen, evalGen, runGen)
import Test.QuickCheck.LCG (Seed, runSeed, randomSeed)

-- | A type synonym which represents the effects used by the `quickCheck` function.
type QC eff a = Eff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | eff) a

-- | Test a property.
-- |
-- | This function generates a new random seed, runs 100 tests and
-- | prints the test results to the console.
quickCheck :: forall eff. Prop -> QC eff Unit
quickCheck prop = quickCheck' 100 prop

-- | A variant of the `quickCheck` function which accepts an extra parameter
-- | representing the number of tests which should be run.
quickCheck' :: forall eff. Int -> Prop -> QC eff Unit
quickCheck' n prop = do
  seed <- randomSeed
  quickCheckWithSeed seed n prop

-- | A variant of the `quickCheck'` function that accepts a specific seed as
-- | well as the number tests that should be run.
quickCheckWithSeed :: forall eff. Seed -> Int -> Prop -> QC eff Unit
quickCheckWithSeed initialSeed n prop = do
  let result = tailRec loop { seed: initialSeed, index: 0, successes: 0, firstFailure: mempty }
  log $ show result.successes <> "/" <> show n <> " test(s) passed."
  for_ result.firstFailure \{ index, message, seed: failureSeed } ->
    throwException $ error
      $ "Test " <> show (index + 1)
      <> " (seed " <> show (runSeed failureSeed) <> ") failed: \n"
      <> message
  where
  loop :: LoopState -> Step LoopState LoopState
  loop state@{ seed, index, successes, firstFailure }
    | index == n = Done state
    | otherwise =
        case runGen (test prop) { newSeed: seed, size: 10 } of
          Tuple Success s ->
            Loop
              { seed: s.newSeed
              , index: index + 1
              , successes: successes + 1
              , firstFailure
              }
          Tuple (Failed message) s ->
            Loop
              { seed: s.newSeed
              , index: index + 1
              , successes
              , firstFailure:
                  firstFailure <> First (Just { index, message, seed })
              }

type LoopState =
  { successes :: Int
  , firstFailure :: First { index :: Int, message :: String, seed :: Seed }
  , seed :: Seed
  , index :: Int
  }

-- | Test a property, returning all test results as an array.
-- |
-- | The first argument is the _random seed_ to be passed to the random generator.
-- | The second argument is the number of tests to run.
quickCheckPure :: Seed -> Int -> Prop -> List Result
quickCheckPure s n prop = evalGen (replicateA n (test prop)) { newSeed: s, size: 10 }

-- | The result of a test: success or failure (with an error message).
data Result = Success | Failed String
instance showResult :: Show Result where
  show Success = "Success"
  show (Failed msg) = "Failed " <> msg

data Prop = PropBoolean Boolean
          | PropResult Result
          | PropGen (Gen Prop)

test :: Prop -> Gen Result
test (PropBoolean b) = pure (if b then Success else Failed "Test returned false")
test (PropResult r) = pure r
test (PropGen g) = g >>= test

withHelp :: Boolean -> String -> Result
withHelp true _ = Success
withHelp false msg = Failed msg

infix 2 withHelp as <?>

-- | Self-documenting equality assertion
assertEquals :: forall a. (Eq a, Show a) => a -> a -> Result
assertEquals a b = a == b <?> show a <> " /= " <> show b

infix 2 assertEquals as ===

-- | Self-documenting inequality assertion
assertNotEquals :: forall a. (Eq a, Show a) => a -> a -> Result
assertNotEquals a b = a /= b <?> show a <> " == " <> show b

infix 2 assertNotEquals as /==
