-module(metal).

-compile({no_auto_import,[error/2]}).

-export([alert/1, alert/2,
         critical/1, critical/2,
         debug/1, debug/2,
         emergency/1, emergency/2,
         error/1, error/2,
         fatal/1, fatal/2,
         info/1, info/2,
         log/6, log/7,
         notice/1, notice/2,
         warning/1, warning/2]).

alert(Message) ->
    alert(Message, []).

alert(Format, Args) ->
    log(alert, Format, Args).

critical(Message) ->
    critical(Message, []).

critical(Format, Args) ->
    log(debug, Format, Args).

debug(Message) ->
    debug(Message, []).

debug(Format, Args) ->
    log(debug, Format, Args).

emergency(Message) ->
    emergency(Message, []).

emergency(Format, Args) ->
    log(emergency, Format, Args).

error(Message) ->
    error(Message, []).

error(Format, Args) ->
    log(error, Format, Args).

fatal(Message) ->
    fatal(Message, []).

fatal(Format, Args) ->
    log(fatal, Format, Args).

info(Message) ->
    info(Message, []).

info(Format, Args) ->
    log(info, Format, Args).

notice(Message) ->
    notice(Message, []).

notice(Format, Args) ->
    log(notice, Format, Args).

warning(Message) ->
    warning(Message, []).

warning(Format, Args) ->
    log(warning, Format, Args).

%% @doc Called by lines resulting from application of the parse_transform
%% when no log_backend macro is defined during compilation.
log(Level, Module, Function, Line, Pid, Message) ->
    log_backend([Level, Module, Function, Line, Pid, Message, []]).

%% @doc Called by lines resulting from application of the parse_transform
%% when no log_backend macro is defined during compilation.
log(Level, Module, Function, Line, Pid, Format, Args) ->
    log_backend([Level, Module, Function, Line, Pid, Format, Args]).

%% ===================================================================
%% Support functions
%% ===================================================================

%% @private
%% @doc Used when the parse_transform is not applied by calls to functions
%% exported from this module aren't transformed out.
log(Level, Format, Args) ->
    log_backend([Level, self(), Format, Args]).

%% @private
%% @doc Delegate to the module supplied as the value of the `log_backend'
%% application environment variable if it exists, otherwise do nothing.
log_backend(Args) ->
    case application:get_env(metal, log_backend) of
        {ok, Module} ->
            apply(Module, log, Args);
        _ ->
            ok
    end.
