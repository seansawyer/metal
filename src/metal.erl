-module(metal).

-export([alert/2,
         critical/2,
         debug/2,
         emergency/2,
         error/2,
         fatal/2,
         info/2,
         notice/2,
         warning/2]).

alert(_,_) ->
    ok.

critical(_,_) ->
    ok.

debug(_,_) ->
    ok.

emergency(_,_) ->
    ok.

error(_,_) ->
    ok.

fatal(_,_) ->
    ok.

info(_,_) ->
    ok.

notice(_,_) ->
    ok.

warning(_,_) ->
    ok.
