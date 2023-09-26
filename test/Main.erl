-module(test_main@foreign).

-export([sleep/1]).

sleep(Time) -> fun () ->
  timer:sleep(Time)
end.
