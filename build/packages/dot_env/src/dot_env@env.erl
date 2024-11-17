-module(dot_env@env).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([set/2, get/1, get_string/1, get_or/2, get_string_or/2, get_then/2, get_int/1, get_int_or/2, get_bool/1, get_bool_or/2]).

-file("/home/runner/work/dotenv/dotenv/src/dot_env/env.gleam", 21).
-spec set(binary(), binary()) -> {ok, nil} | {error, binary()}.
set(Key, Value) ->
    dot_env_ffi:set_env(Key, Value).

-file("/home/runner/work/dotenv/dotenv/src/dot_env/env.gleam", 38).
-spec get(binary()) -> {ok, binary()} | {error, binary()}.
get(Key) ->
    dot_env_ffi:get_env(Key).

-file("/home/runner/work/dotenv/dotenv/src/dot_env/env.gleam", 58).
-spec get_string(binary()) -> {ok, binary()} | {error, binary()}.
get_string(Key) ->
    dot_env_ffi:get_env(Key).

-file("/home/runner/work/dotenv/dotenv/src/dot_env/env.gleam", 62).
-spec get_or(binary(), binary()) -> binary().
get_or(Key, Default) ->
    _pipe = dot_env_ffi:get_env(Key),
    gleam@result:unwrap(_pipe, Default).

-file("/home/runner/work/dotenv/dotenv/src/dot_env/env.gleam", 81).
-spec get_string_or(binary(), binary()) -> binary().
get_string_or(Key, Default) ->
    _pipe = dot_env_ffi:get_env(Key),
    gleam@result:unwrap(_pipe, Default).

-file("/home/runner/work/dotenv/dotenv/src/dot_env/env.gleam", 100).
-spec get_then(binary(), fun((binary()) -> {ok, GJN} | {error, binary()})) -> {ok,
        GJN} |
    {error, binary()}.
get_then(Key, Next) ->
    case dot_env_ffi:get_env(Key) of
        {ok, Value} ->
            Next(Value);

        {error, Err} ->
            {error, Err}
    end.

-file("/home/runner/work/dotenv/dotenv/src/dot_env/env.gleam", 126).
-spec get_int(binary()) -> {ok, integer()} | {error, binary()}.
get_int(Key) ->
    get_then(Key, fun(Raw_value) -> _pipe = gleam@int:parse(Raw_value),
            gleam@result:map_error(
                _pipe,
                fun(_) ->
                    <<<<"Failed to parse environment variable for `"/utf8,
                            Key/binary>>/binary,
                        "` as integer"/utf8>>
                end
            ) end).

-file("/home/runner/work/dotenv/dotenv/src/dot_env/env.gleam", 149).
-spec get_int_or(binary(), integer()) -> integer().
get_int_or(Key, Default) ->
    _pipe = get_int(Key),
    gleam@result:unwrap(_pipe, Default).

-file("/home/runner/work/dotenv/dotenv/src/dot_env/env.gleam", 170).
-spec get_bool(binary()) -> {ok, boolean()} | {error, binary()}.
get_bool(Key) ->
    get_then(Key, fun(Raw_value) -> case gleam@string:lowercase(Raw_value) of
                <<"true"/utf8>> ->
                    {ok, true};

                <<"1"/utf8>> ->
                    {ok, true};

                <<"false"/utf8>> ->
                    {ok, false};

                <<"0"/utf8>> ->
                    {ok, false};

                _ ->
                    {error,
                        <<<<"Invalid boolean value for environment variable `"/utf8,
                                Key/binary>>/binary,
                            "`. Expected one of `true`, `false`, `1`, or `0`."/utf8>>}
            end end).

-file("/home/runner/work/dotenv/dotenv/src/dot_env/env.gleam", 199).
-spec get_bool_or(binary(), boolean()) -> boolean().
get_bool_or(Key, Default) ->
    _pipe = get_bool(Key),
    gleam@result:unwrap(_pipe, Default).
