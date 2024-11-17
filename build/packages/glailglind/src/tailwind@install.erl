-module(tailwind@install).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec main() -> {ok, nil} | {error, binary()}.
main() ->
    tailwind:install().
