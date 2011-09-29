-module(metal_error_logger).

-export([log/4,
         log/7,
         log_transform/2]).

log(Level, Pid, Format, Args) ->
    log(Level, undefined, undefined, 0, Pid, Format, Args).

log(Level, _Module, _Function, _Line, _Pid, Format, Args) ->
    apply(error_logger, level_function(Level), [Format, Args]). 

log_transform(Level, Args) ->
    {error_logger, level_function(Level), Args}.

level_function(Level) ->
    case Level of
        alert     -> error_msg;
        critical  -> error_msg;
        emergency -> error_msg;
        error     -> error_msg;
        fatal     -> error_msg;
        warning   -> warning_msg;
        _         -> info_msg
    end.
