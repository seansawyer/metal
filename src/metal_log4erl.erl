-module(metal_log4erl).

-export([call_transform/1,
         log/4,
         log/7]).

log(Level, _Pid, Format, Args) ->
    log4erl:log(level_function(Level), Format, Args).

log(Level, _Module, _Function, _Line, _Pid, Format, Args) ->
    log4erl:log(level_function(Level), Format, Args).

call_transform(Level) ->
    {log4erl, level_function(Level)}.

%% metal levels are:
%% debug, info, notice, warning, error, critical, alert, emergency
%% log4erl levels are:
%% debug, error, fatal, info, warn
level_function(Level) ->
    case Level of
        debug     -> debug;
        info      -> info;
        notice    -> warn;
        warning   -> warn;
        error     -> error;
        critical  -> error;
        alert     -> error;
        emergency -> fatal;
        fatal     -> fatal;
        _ -> info
    end.

