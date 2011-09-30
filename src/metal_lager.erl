-module(metal_lager).

-export([call_transform/1,
         log/4,
         log/7]).

log(Level, Pid, Format, Args) ->
    lager:log(level_function(Level), Pid, Format, Args).

log(Level, Module, Function, Line, Pid, Format, Args) ->
    Level1 = level_function(Level),
    Time = lager_util:maybe_utc(lager_util:localtime_ms()),
    lager:log(Level1, Module, Function, Line, Pid, Time, Format, Args).

call_transform(Level) ->
    {lager, level_function(Level)}.

level_function(Level) ->
    case Level of
        debug     -> debug;
        info      -> info;
        notice    -> notice;
        warning   -> warning;
        error     -> error;
        fatal     -> emergency;
        critical  -> critical;
        alert     -> alert;
        emergency -> emergency;
        _ -> info
    end.
