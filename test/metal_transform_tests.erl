-module(metal_transform_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

parse_transform_test_() ->
    [{"With no backend, transform metal:LEVEL/N calls to metal:log/N+5",
      fun() ->
          Expected = forms_no_backend(),
          ets:new(metal, [public, named_table]),
          {ok, _, _} = compile:forms(forms_in(),
                                     [{parse_transform, metal_transform}]),
          [{ast_out, Actual}] = ets:lookup(metal, ast_out),
          ets:delete(metal),
          ?assertMatch(Expected, Actual)
      end},
     {"With lager backend, transform metal:LEVEL/N to lager:F/N",
      fun() ->
          Expected = forms_lager(),
          ets:new(metal, [public, named_table]),
          {ok, _, _} = compile:forms(forms_in(),
                                     [{d, log_backend, metal_lager},
                                      {parse_transform, metal_transform}]),
          [{ast_out, Actual}] = ets:lookup(metal, ast_out),
          ets:delete(metal),
          ?assertMatch(Expected, Actual)
      end},
     {"With error_logger backend, transform metal:LEVEL/N to error_logger:F/N",
      fun() ->
          Expected = forms_error_logger(),
          ets:new(metal, [public, named_table]),
          {ok, _, _} = compile:forms(forms_in(),
                                     [{d, log_backend, metal_error_logger},
                                      {parse_transform, metal_transform}]),
          [{ast_out, Actual}] = ets:lookup(metal, ast_out),
          ets:delete(metal),
          ?assertMatch(Expected, Actual)
      end},
     {"With log4erl backend, transform metal:LEVEL/N to log4erl:F/N",
      fun() ->
          Expected = forms_log4erl(),
          ets:new(metal, [public, named_table]),
          {ok, _, _} = compile:forms(forms_in(),
                                     [{d, log_backend, metal_log4erl},
                                      {parse_transform, metal_transform}]),
          [{ast_out, Actual}] = ets:lookup(metal, ast_out),
          ets:delete(metal),
          ?assertMatch(Expected, Actual)
      end}].

forms(Clause) ->
    [{attribute,1,module,transform_test},
     {attribute,2,export,[{test,0}]},
     {function, 4, test, 0, [
       {clause, 4, [], [], Clause}
      ]}].

forms_in() ->
    Clause = [{call, 5, {remote, 5, {atom,5,metal}, {atom,5,debug}},
                        [{string,5,"Hello"}]},
              {call, 6, {remote, 6, {atom,6,metal}, {atom,6,debug}},
                        [{string,6,"Hello ~p"},
                         {cons, 6, {string,6,"world!"}, {nil,1}}]}],
    forms(Clause).

forms_no_backend() ->
    Clause = [{call, 5, {remote, 5, {atom,5,metal}, {atom,5,log}},
                        [{atom, 5, debug},
                         {atom, 5, transform_test},
                         {atom, 5, test},
                         {integer, 5, 5},
                         {call, 5, {atom, 5,self}, []},
                         {string,5,"Hello"}]},
              {call, 6, {remote, 6, {atom,6,metal}, {atom,6,log}},
                        [{atom, 6, debug},
                         {atom, 6, transform_test},
                         {atom, 6, test},
                         {integer, 6, 6},
                         {call, 6, {atom, 6,self}, []},
                         {string,6,"Hello ~p"},
                         {cons, 6, {string,6,"world!"}, {nil,1}}]}],
    forms(Clause).

forms_error_logger() ->
    Clause = [{call, 5, {remote, 5, {atom,5,error_logger}, {atom,5,info_msg}},
                        [{string,5,"Hello"}]},
              {call, 6, {remote, 6, {atom,6,error_logger}, {atom,6,info_msg}},
                        [{string,6,"Hello ~p"},
                         {cons, 6, {string,6,"world!"}, {nil,1}}]}],
    forms(Clause).

forms_lager() ->
    Clause = [{call, 5, {remote, 5, {atom,5,lager}, {atom,5,debug}},
                        [{string,5,"Hello"}]},
              {call, 6, {remote, 6, {atom,6,lager}, {atom,6,debug}},
                        [{string,6,"Hello ~p"},
                         {cons, 6, {string,6,"world!"}, {nil,1}}]}],
    forms(Clause).

forms_log4erl() ->
    Clause = [{call, 5, {remote, 5, {atom,5,log4erl}, {atom,5,debug}},
                        [{string,5,"Hello"}]},
              {call, 6, {remote, 6, {atom,6,log4erl}, {atom,6,debug}},
                        [{string,6,"Hello ~p"},
                         {cons, 6, {string,6,"world!"}, {nil,1}}]}],
    forms(Clause).
