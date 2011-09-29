-module(metal_transform).

-export([parse_transform/2]).

parse_transform(AST, Options) ->
    case log_transforms(Options) of
        [] ->
            AST;
        Transforms when is_list(Transforms) ->
            put(log_transforms, Transforms),
            walk_ast([], AST)
    end.

walk_ast(Acc, []) ->
    lists:reverse(Acc);
walk_ast(Acc, [{function, Line, Name, Arity, Clauses}|T]) ->
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
    LogTransform = hd(get(log_transforms)),
    {M,F,A} = apply(LogTransform, [Level, Args]),
    {call, Line, {remote, Line1, {atom, Line2, M}, {atom, Line3, F}}, A}. 

log_transforms([]) ->
    [];
log_transforms([{d, log_transforms, LogTransforms}|_]) ->
    LogTransforms;
log_transforms([_|T]) ->
    log_transforms(T).
