-module(metal_error_logger).

-export([log_transform/2]).

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
