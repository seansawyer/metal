-module(metal_transform).

-export([parse_transform/2]).

parse_transform(AST, Options) ->
    case log_transform(Options) of
        {ok, Transform} when is_atom(Transform) ->
            put(log_transform, Transform),
            walk_ast([], AST);
        _ ->
            AST
    end.

walk_ast(Acc, []) ->
    lists:reverse(Acc);
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
    case get(log_transform) of
        undefined ->
            call_metal_log(Level, Args, Line, Line1, Line2, Line3);
        Module ->
            call_backend_log(Module, Level, Args, Line, Line1, Line2, Line3)
    end.
            
call_backend_log(Module, Level, Args, Line, Line1, Line2, Line3) ->
    {M,F,A} = apply(Module, log_transform, [Level, Args]),
    {call, Line, {remote, Line1, {atom, Line2, M}, {atom, Line3, F}}, A}. 

call_metal_log(Level, Args, Line, Line1, Line2, Line3) ->
    {call, Line,
     {remote, Line1, {atom, Line2, metal}, {atom, Line3, log}},
     [{atom, Line3, Level},
      {atom, Line3, get(module)},
      {atom, Line3, get(function)},
      {integer, Line3, Line},
      {call, Line3, {atom, Line3 ,self}, []} | Args]}.

log_transform([]) ->
    undefined;
log_transform([{d, log_transform, LogTransform}|_]) ->
    {ok, LogTransform};
log_transform([_|T]) ->
    log_transform(T).
