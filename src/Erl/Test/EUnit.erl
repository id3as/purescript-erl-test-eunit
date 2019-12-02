-module(erl_test_eUnit@foreign).

-export([runTests_/3]).

runTests_(Success, Fail, Tests) -> fun () ->
  case eunit:test(Tests) of
    ok -> Success;
    {error, Err} -> Fail(Err);
    error -> Fail(undefined)
  end
end.