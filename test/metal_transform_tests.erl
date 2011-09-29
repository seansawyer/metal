-module(metal_transform_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

parse_transform_test_() ->
    [{"With no backend, transform metal:LEVEL/N calls to metal:log/4",
      ?_assertMatch(ok, ok)},
     {"With error_logger backend, transform metal:LEVEL/N to error_logger:F/N",
      ?_assertMatch(ok, ok)},
     {"With lager backend, transform metal:LEVEL/N to lager:F/N",
      ?_assertMatch(ok, ok)},
     {"With log4erl backend, transform metal:LEVEL/N to log4erl:F/N",
      ?_assertMatch(not_supported, not_supported)}].
