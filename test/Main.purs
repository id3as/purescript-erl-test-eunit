module Test.Main (main) where

import Prelude

import Control.Monad.Free (liftF)
import Debug (trace)
import Effect (Effect)
import Effect.Console (log)
import Erl.Test.EUnit (TestF(..), TestSuite, runTests, setupTeardown, setupTeardown_, suite, test, timeout)
import Test.Assert (assert, assertEqual)

foreign import sleep :: Int -> Effect Unit

naiveTimeout :: Int -> TestSuite -> TestSuite
naiveTimeout time t = liftF $ TestTimeout time t unit

main :: Effect Unit
main =
  void $ runTests do
    suite "my test suite" do
      test "test 1" do
        assert false
        log "in test 1"
      test "test 2" do
        assertEqual { actual: "foo", expected: "bar" }
        log "in test 1"
      test "test 3" do
        log "in test 1"
    suite "my second test suite" do
      test "test 1.1" do
        log "in test 1.1"
      test "test 1.2" do
        log "in test 1.2"
    test "outside suite" do
      log "in test outside"
    setupTeardown_ (trace "[t] setup" \_ -> log "setup") (trace "[t] teardown" \_ -> log "teardown") do
      test "setup/teardown test" do
        log "test with setupTeardown"
        assert false
    setupTeardown (pure { foo: 42 }) (\{ foo } -> log $ "teardown with " <> show foo) \{ foo } -> do
      test "setup/teardown test with data" do
        log "test with setupTeardown passing data"
        assert (foo == 42)
    -- The timeouts on single tests will stay at 5s even if we want 30
    suite "timeouts don't work on anything else than single tests" do
      naiveTimeout 30 do
        test "slow test 1" do
          log "slow test 1"
          sleep 10000
        test "slow test 2" do
          log "slow test 2"
          sleep 10000
    suite "timeouts work on single tests" do
      naiveTimeout 12 do
        test "slow test 3" do
          log "slow test 3"
          sleep 10000

