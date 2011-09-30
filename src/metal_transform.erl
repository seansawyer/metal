-module(metal_transform).

-export([parse_transform/2]).

-ifdef(TEST).
-define(STORE_AST, true).
-else.
-define(STORE_AST, false).
-endif.

parse_transform(AST, Options) ->
    log_backend(Options),
    ?STORE_AST andalso ets:insert(metal, {ast_in, AST}),
    walk_ast([], AST).

walk_ast(Acc, []) ->
    Result = lists:reverse(Acc),
    ?STORE_AST andalso ets:insert(metal, {ast_out, Result}),
    Result;
walk_ast(Acc, [{attribute, _, module, {Module, _PmodArgs}}=H|T]) ->
    put(module, Module),
    walk_ast([H|Acc], T);
walk_ast(Acc, [{attribute, _, module, Module}=H|T]) ->
    put(module, Module),
    walk_ast([H|Acc], T);
walk_ast(Acc, [{function, Line, Name, Arity, Clauses}|T]) ->
    put(function, Name),
    Forms = {function, Line, Name, Arity, walk_clauses([], Clauses)},
    walk_ast([Forms|Acc], T);
walk_ast(Acc, [H|T]) ->
    walk_ast([H|Acc], T).

walk_clauses(Acc, []) ->
    lists:reverse(Acc);
walk_clauses(Acc, [{clause, Line, Arguments, Guards, Body}|T]) ->
    Forms = {clause, Line, Arguments, Guards, walk_body([], Body)},
    walk_clauses([Forms|Acc], T).

walk_body(Acc, []) ->
    lists:reverse(Acc);
walk_body(Acc, [H|T]) ->
    walk_body([statement(H)|Acc], T).

statement({call, Line,
           {remote, Line1,
            {atom, Line2, metal},
            {atom, Line3, Level}},
           Args}) ->
    case get(log_backend) of
        undefined ->
            call_metal_log(Level, Args, Line, Line1, Line2, Line3);
        Module ->
            call_backend_log(Module, Level, Args, Line, Line1, Line2, Line3)
    end.
            
call_backend_log(Module, Level, Args, Line, Line1, Line2, Line3) ->
    {M,F} = apply(Module, call_transform, [Level]),
    {call, Line, {remote, Line1, {atom, Line2, M}, {atom, Line3, F}}, Args}. 

call_metal_log(Level, Args, Line, Line1, Line2, Line3) ->
    {call, Line,
     {remote, Line1, {atom, Line2, metal}, {atom, Line3, log}},
     [{atom, Line3, Level},
      {atom, Line3, get(module)},
      {atom, Line3, get(function)},
      {integer, Line3, Line},
      {call, Line3, {atom, Line3 ,self}, []} | Args]}.

log_backend([]) ->
    false;
log_backend([{d, log_backend, Module}|_]) ->
    put(log_backend, Module),
    Module;
log_backend([_|T]) ->
    log_backend(T).
