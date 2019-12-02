module Erl.Test.EUnit 
( TestF
, TestSet
, suite
, test
, collectTests
, runTests
) where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.State (State, execState, modify_, runState)
import Data.Tuple as Tuple
import Effect (Effect)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (tuple2)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

foreign import data TestSet :: Type

testSet :: forall a. a -> TestSet
testSet = unsafeCoerce

data Group = Group String TestSuite
data TestF a =
    TestGroup Group a
  | TestUnit String Test a

instance functorTestF :: Functor TestF where
  map f (TestGroup g a) = TestGroup g (f a)
  map f (TestUnit l t a) = TestUnit l t (f a)

type TestSuite = Free TestF Unit

type Test = Effect Unit

suite :: String -> TestSuite -> TestSuite
suite label tests = liftF $ TestGroup (Group label tests) unit

test :: String -> Test -> TestSuite
test l t = liftF $ TestUnit l t unit

collectTests :: TestSuite -> List TestSet
collectTests tst = execState (runFreeM go tst) nil
  where

  go :: forall a. TestF (Free TestF a) -> State (List TestSet) (Free TestF a)
  go (TestUnit s t a) = do
    modify_ (testSet (tuple2 s t) : _)
    pure a
  go (TestGroup (Group s tests) a) = do
    let grouped = case runState (runFreeM go tests) nil of
                    Tuple.Tuple _ s -> s
    modify_ (testSet (tuple2 s grouped) : _)
    pure a

foreign import runTests_ :: forall a. a -> (Foreign -> a) -> List TestSet -> Effect a

runTests :: TestSuite -> Effect Boolean
runTests suite = runTests_ true (const false) $ collectTests suite