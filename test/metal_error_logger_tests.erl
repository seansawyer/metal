-module(metal_error_logger_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit.hrl").
-endif.

call_transform_test_() ->
    Mappings = [{alert, error_msg},
                {critical, error_msg},
                {debug, info_msg},
                {emergency, error_msg},
                {error, error_msg},
                {fatal, error_msg},
                {info, info_msg},
                {notice, info_msg},
                {warning, warning_msg}],
    call_transform_gen([], Mappings).

call_transform_gen(Tests, []) ->
    Tests;
call_transform_gen(Tests, [{Level, Function}|T]) ->
    Args = ["~p", foo],
    Desc = test_desc("Maps [~p, Args] to {error_logger, ~p, Args}",
                     [Level, Function]),
    Test = {Desc, 
            ?_assertMatch({error_logger, Function, Args},
                          metal_error_logger:call_transform(Level, Args))},
    call_transform_gen([Test|Tests], T).
