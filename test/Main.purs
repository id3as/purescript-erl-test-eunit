module Test.Main (main) where

import Prelude

import Debug.Trace (trace, traceM)
import Effect (Effect)
import Effect.Console (log)
import Erl.Data.List (nil, (:))
import Erl.Test.EUnit (runTests, suite, test)
import Test.Assert (assert, assertEqual)

main :: Effect Unit
main =
  void $ runTests do
    suite "my test suite" do
      test "test 1" do
        assert false
        log "in test 1"
      test "test 2" do
        assertEqual { actual:  "foo", expected: "bar" }
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