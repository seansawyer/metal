
-spec test_desc (string(), list(any())) -> binary().
%% @doc Format a test description and convert it to a binary to keep EUnit
%% from whining.
test_desc(Format, Args) ->
    iolist_to_binary(io_lib:format(Format, Args)).
