-module(metal_lager_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit.hrl").
-endif.

call_transform_test_() ->
    Mappings = [{alert, alert},
                {critical, critical},
                {debug, debug},
                {emergency, emergency},
                {error, error},
                {fatal, emergency},
                {info, info},
                {notice, notice},
                {warning, warning}],
    call_transform_gen([], Mappings).

call_transform_gen(Tests, []) ->
    Tests;
call_transform_gen(Tests, [{Level, Function}|T]) ->
    Desc = test_desc("Maps ~p to {lager, ~p}", [Level, Function]),
    Test = {Desc, 
            ?_assertMatch({lager, Function},
                          metal_lager:call_transform(Level))},
    call_transform_gen([Test|Tests], T).
