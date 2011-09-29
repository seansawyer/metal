## Metal, an Erlang metalogger ##

Metal lets you defer the decision of how your application does logging. It is
implemented as a Erlang parse_transform. Out-of-the-box support for the
following logging libraries is or will shortly be included:

* [error_logger](http://www.erlang.org/doc/man/error_logger.html) - meh
* [lager](https://github.com/basho/lager) - my personal favorite, as it includes module/function/line information
* [log4erl](https://github.com/ahmednawras/log4erl) - also gets the job done

However, metal does not introduce a dependency on any of these.

## Usage ##

Tell Metal which log_transform module to use by adding a macro definition to your compiler flags :

    {d, log_transform, metal_error_logger}

Next enable the Metal parse_transform in your application by doing one of the following:

* Add the parse_transform to your compiler flags: `{parse_transform, metal_transform}`
* Add a compiler attribute to your module: `-compile([{parse_transform, metal_transform}]).`

__If you are using Lager__, be sure to add the metal_transform __before__ lager_transform, whichever method you choose.
