module Erl.Test.EUnit
  ( TestF(..)
  , Group(..)
  , Test
  , Setup
  , Teardown
  , TestSet
  , TestSetup
  , TestSuite
  , TestTeardown
  , collectTests
  , empty
  , runTests
  , setup
  , setupTeardown
  , suite
  , teardown
  , test
  , timeout
  ) where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.State (State, execState, modify_, runState)
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Atom (atom)
import Erl.Data.List (List, nil, reverse, (:))
import Erl.Data.Tuple (tuple2, tuple3, tuple4)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

foreign import data TestSet :: Type

testSet :: forall a. a -> TestSet
testSet = unsafeCoerce

type Test = Effect Unit

type Setup = Effect Foreign

type Teardown = Foreign -> Effect Unit

type TestSetup a = Effect a
type TestTeardown a = a -> Effect Unit

type TestSuite = Free TestF Unit

data Group = Group String TestSuite

data TestF a
  = TestGroup Group a
  | TestUnit String Test a
  | TestState Setup Teardown (Foreign -> TestSuite) a
  | TestTimeout Int TestSuite a
  | TestEmpty a

instance functorTestF :: Functor TestF where
  map f (TestGroup g a) = TestGroup g (f a)
  map f (TestUnit l t a) = TestUnit l t (f a)
  map f (TestState s t su a) = TestState s t su (f a)
  map f (TestTimeout t su a) = TestTimeout t su (f a)
  map f (TestEmpty a) = TestEmpty (f a)

suite :: String -> TestSuite -> TestSuite
suite label tests = liftF $ TestGroup (Group label tests) unit

test :: String -> Test -> TestSuite
test l t = liftF $ TestUnit l t unit

empty :: TestSuite
empty = liftF $ TestEmpty unit

testDataToForeign :: forall a. a -> Foreign
testDataToForeign = unsafeCoerce

teardownToForeign :: forall a. TestTeardown a -> Teardown
teardownToForeign = unsafeCoerce

testSuiteToForeign :: forall a. (a -> TestSuite) -> (Foreign -> TestSuite)
testSuiteToForeign = unsafeCoerce

setupTeardown :: forall a. TestSetup a -> TestTeardown a -> (a -> TestSuite) -> TestSuite
setupTeardown s t su = liftF $ TestState (map testDataToForeign s) (teardownToForeign t) (testSuiteToForeign su) unit

setup :: forall a. TestSetup a -> (a -> TestSuite) -> TestSuite
setup s su = liftF $ TestState (map testDataToForeign s) (\_ -> pure unit) (testSuiteToForeign su) unit

teardown :: Effect Unit -> TestSuite -> TestSuite
teardown t su = liftF $ TestState (pure $ unsafeCoerce unit) (const t) (const su) unit

-- This should be the only way allowed to build a Timeout node, because eunit only applies timeouts to single tests.
-- This also implies that there seem not to be a way to apply a collective timeout to a list of a tests.
-- We are not even the first to run into this: http://erlang.org/pipermail/erlang-questions/2015-January/082755.html
timeout :: Int -> String -> Test -> TestSuite
timeout time label t = liftF $ TestTimeout time (test label t) unit

collectTests :: TestSuite -> List TestSet
collectTests tst = reverse $ execState (runFreeM go tst) nil
  where

  go :: forall a. TestF (Free TestF a) -> State (List TestSet) (Free TestF a)
  go (TestUnit s t a) = do
    modify_ (testSet (tuple2 (atom "spawn") (tuple2 s t)) : _)
    pure a
  go (TestGroup (Group label tests) a) = do
    let
      grouped = case runState (runFreeM go tests) nil of
        Tuple.Tuple _ g -> reverse g
    modify_ (testSet (tuple2 label grouped) : _)
    pure a
  go (TestState s t tests a) = do
    modify_
      ( testSet
          ( tuple4
              (atom "setup")
              s
              (\testData -> unsafePerformEffect (t testData))
              (\foreign_ -> reverse $ collectTests (tests foreign_))
          ) : _
      )
    pure a
  go (TestTimeout t tests a) = do
    let
      grouped = case runState (runFreeM go tests) nil of
        Tuple.Tuple _ g -> reverse g
    modify_ (testSet (tuple3 (atom "timeout") t grouped) : _)
    pure a
  go (TestEmpty a) = do
    pure a

foreign import runTests_ :: forall a. a -> (Foreign -> a) -> List TestSet -> Effect a

runTests :: TestSuite -> Effect Boolean
runTests testSuite = runTests_ true (const false) $ collectTests testSuite
