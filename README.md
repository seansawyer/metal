# Metal, an Erlang metalogger #

Metal lets you defer the decision of when and how your application does logging. It supplies a parse_transform to collect module/function/line information in order to supply it to your log backend later, and falls back to runtime logging when that information is unavailable. Out-of-the-box support for the following logging libraries is included:

* [error_logger](http://www.erlang.org/doc/man/error_logger.html) - meh
* [lager](https://github.com/basho/lager) - my personal favorite, as it includes module/function/line information
* [log4erl](https://github.com/ahmednawras/log4erl) - also gets the job done

However, Metal does not introduce any dependencies on logging frameworks or other libraries.

To get started, add Metal to the `deps` list in your `rebar.config`:

    {'metal', ".*", {git, "git://github.com/seansawyer/metal.git", "master"}}

## Usage in library applications ##

Just enable the Metal parse_transform during compilation by doing one of the following:

* Adding the parse_transform to your compiler flags: `{parse_transform, metal_transform}`. If you have some method of forcing this on users of your library, like a Rebar config file, this is a good option. 
* Adding a compiler attribute to your modules: `-compile([{parse_transform, metal_transform}]).` There are various reasons you might choose this; one is discussed below.

Do not define the log_backend macro or application environment variable. This will be left to developers who use your application as a dependency.

## Usage in top-level applications ##

First, define an application environment variable to tell Metal which module provides the runtime interface to your logging library, perhaps by adding the following to your `app.config` or `sys.config` file like so:

    {metal, [{log_backend, metal_lager}]}

Or by setting it with `application:set_env/3` if you prefer:

    application:set_env(metal, log_backend, metal_lager).

If you don't plan to use your application as a library embedded in another application, you are done. Use your logging library of choice as usual, and you should automagically start seeing log output from any dependencies that play nice with Metal.

If you do plan to use your application as a library elsewhere, you might consider doing your logging through Metal and configuring the parse transform to call through to your logging framework.

* Either add the parse_transform to your compiler flags: `{parse_transform, metal_transform}`
* Adding a compiler attribute to your modules: `-compile([{parse_transform, metal_transform}]).` I think you'll have to go this route if you are using lager

Now tell Metal which log_backend module contains the implementation of `log_transform/2` you with to use by adding a macro definition to your compiler flags:

    {d, log_backend, metal_lager} %% assuming you are using lager - see below for more

This module/function supplies the transformation of calls to metal:LEVEL/N into calls to your logging library of choice. The following log_backends are included with Metal:

* [metal_error_logger](https://github.com/seansawyer/metal/blob/master/src/metal_error_logger.erl) - interface to error_logger
* [metal_lager](https://github.com/seansawyer/metal/blob/master/src/metal_lager.erl) - interface to lager
* [metal_log4erl](https://github.com/seansawyer/metal/blob/master/src/metal_log4erl.erl) - interfact to log4erl

Finally, start metal (e.g., `application:start(metal).`) along with any applications associated with your logging library, and you are good to go.

## Notes for those using both Lager and Metal parse transforms ##

* You must use the per-file `-compile` attributes rather than the compiler option attribute for now. Sorry!
* When adding parse transforms, be sure to add `metal_transform` __before__ `lager_transform`.
* Don't skip `metal_transform`. If you do, Lager's parse transform won't see any calls to Lager, and you'll lose its nice short-circuiting when the requested level is not enabled.

## Writing your own log_backend ##

More info on this soon, but you can probably figure it out by looking at the modules listed above.

## Forcing a log backend in a library application ##

It is not recommended to do so, but it should be possible to force a log backend when writing a library application. You would need to bundle the dependency with your application to prevent errors. Then simply follow the directions for a top-level application above. This is currently untested, but like I said it's not recommended. Why would you be using Metal in that case anyway?

## Thanks! ##

Many thanks to the authors of [Lager](https://github.com/basho/lager), whose parse transform approach inspired this.
