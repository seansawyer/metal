# Metal, an Erlang metalogger #

Metal lets you defer the decision of how your application does logging. It supports both the parse_transform approach used by something like Lager, as well as runtime logging. Metal supplies its own parse_transform to collect module/function/line information in order to supply it to your log backend later. This was  is implemented as a Erlang parse_transform. Out-of-the-box support for the following logging libraries is or will shortly be included:

* [error_logger](http://www.erlang.org/doc/man/error_logger.html) - meh
* [lager](https://github.com/basho/lager) - my personal favorite, as it includes module/function/line information
* [log4erl](https://github.com/ahmednawras/log4erl) - also gets the job done

However, Metal does not introduce any dependencies on logging frameworks or other libraries.

## Notes for those using Lager ##

* When adding parse transforms, be sure to add `metal_transform` __before__ `lager_transform`.
* Don't skip `metal_transform`. If you do, Lager's parse transform won't see any calls to Lager, and you'll lose its nice short-circuiting when the requested level is not enabled.

## Usage in library applications ##

Just enable the Metal parse_transform during compilation by doing one of the following:

* Add the parse_transform to your compiler flags: `{parse_transform, metal_transform}`
* Add a compiler attribute to your module: `-compile([{parse_transform, metal_transform}]).`

Do not define the log_backend macro or application environment variable. This will be left to developers who use your application as a dependency.

## Usage in top-level applications ##

First, define an application environment variable to tell Metal which module provides the runtime interface to your logging library, perhaps by adding the following to your `app.config` or `sys.config` file like so:

    {metal, [{log_backend, metal_error_logger}]}

Or by setting it with `application:set_env/3` if you prefer:

    application:set_env(metal, log_backend, metal_error_logger).

You can stop here if you are feeling lazy, but you should really continue by configuring the parse transform. There are downsides to being lazy, though, and you will lose module/line/function info (as well as short-circuiting in Lager - see above). Anyway, since you're not lazy, add the parse transform by either:

* adding the parse_transform to your compiler flags: `{parse_transform, metal_transform}`
* or adding a compiler attribute to your modules: `-compile([{parse_transform, metal_transform}]).`

Finally, tell Metal which module contains the implementation of `log_transform/2` to use by adding a macro definition to your compiler flags:

    {d, log_backend, metal_error_logger}

This tells Metal which module to use when transforming calls to metal:LEVEL/N into calls to your logging library of choice. The following are included with Metal:

* [metal_error_logger](https://github.com/seansawyer/metal/blob/master/src/metal_error_logger.erl) - interface to error_logger
* [metal_lager](https://github.com/seansawyer/metal/blob/master/src/metal_lager.erl) - interface to lager
* [metal_log4erl](https://github.com/seansawyer/metal/blob/master/src/metal_log4erl.erl) - interfact to log4erl

## Writing your own log_backend ##

More info on this soon, but you can probably figure it out by looking at the modules listed above.

## Forcing a log backend in a library application ##

It is not recommended to do so, but it should be possible to force a log backend when writing a library application. You would need to bundle the dependency with your application to prevent errors. Then simply follow the directions for a top-level application above. This is currently untested, but like I said it's not recommended. Why would you be using Metal in that case anyway?
