-module(tailwind@run).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec main() -> nil.
main() ->
    Output@1 = begin
        _pipe = tailwind:get_args(),
        _pipe@1 = gleam@result:'try'(_pipe, fun(Args) -> tailwind:run(Args) end),
        gleam@result:'try'(
            _pipe@1,
            fun(Output) ->
                gleam@io:println(Output),
                {ok, nil}
            end
        )
    end,
    case Output@1 of
        {error, Err} ->
            gleam@io:println(Err);

        _ ->
            nil
    end.
