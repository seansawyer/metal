-module(metal_log4erl_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit.hrl").
-endif.

call_transform_test_() ->
    Mappings = [{alert, error},
                {critical, error},
                {debug, debug},
                {emergency, fatal},
                {error, error},
                {fatal, fatal},
                {info, info},
                {notice, warn},
                {warning, warn}],
    call_transform_gen([], Mappings).

call_transform_gen(Tests, []) ->
    Tests;
call_transform_gen(Tests, [{Level, Function}|T]) ->
    Desc = test_desc("Maps ~p to {log4erl, ~p}", [Level, Function]),
    Test = {Desc, 
            ?_assertMatch({log4erl, Function},
                          metal_log4erl:call_transform(Level))},
    call_transform_gen([Test|Tests], T).

