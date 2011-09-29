# Metal, an Erlang metalogger #

Metal lets you defer the decision of how your application does logging. It is
implemented as a Erlang parse_transform. Out-of-the-box support for the
following logging libraries is or will shortly be included:

* [error_logger](http://www.erlang.org/doc/man/error_logger.html) - meh
* [lager](https://github.com/basho/lager) - my personal favorite, as it includes module/function/line information
* [log4erl](https://github.com/ahmednawras/log4erl) - also gets the job done

However, metal does not introduce a dependency on any of these.

## A note for those using Lager ##

When adding parse_transforms, be sure to add metal_transform __before__ lager_transform.

## Usage in library applications ##

Just enable the Metal parse_transform during compilation by doing one of the following:

* Add the parse_transform to your compiler flags: `{parse_transform, metal_transform}`
* Add a compiler attribute to your module: `-compile([{parse_transform, metal_transform}]).`

Do not define the log_backend macro or application environment variable. This will be left to developers who use your application as a dependency.

## Usage in top-level applications ##

Enable the Metal parse_transform in your application as described above.

Next, tell Metal which module contains the implementation of `log_transform/2` to use by adding a macro definition to your compiler flags :

    {d, log_backend, metal_error_logger}

Finally, define an application environment variable to tell Metal which module provides the runtime interface to your log backend, perhaps by adding the following to your `app.config` or `sys.config` file:

    {metal, [{log_backend, metal_error_logger}]}

You can skip the parse_transform altogether if you really want, but Lager in particular relies on its parse_transform to introduce short-circuiting when the requested level is not enabled, so it's best to configure it.

## Disadvantages? ##

If you are using Lager, you lose the short-circuiting mentioned above in library applications. I hope to fix this in the near future.

## Forcing a log backend in a library application ##

It is not recommended to do so, but it is possible to force a log backend when writing a library application. For this to work, however, you must bundle the dependency with your application to prevent errors. Then simply follow the directions for a top-level application above.
