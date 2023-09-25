module Test.Main (main) where

import Prelude

import Debug (trace)
import Effect (Effect)
import Effect.Console (log)
import Erl.Test.EUnit (runTests, setupTeardown, suite, test)
import Test.Assert (assert, assertEqual)

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
    setupTeardown (trace "[t] setup" \_ -> log "setup") (const $ trace "[t] teardown" \_ -> log "teardown") \_ -> do
      test "setup/teardown test" do
        log "test with setupTeardown"
        assert false
