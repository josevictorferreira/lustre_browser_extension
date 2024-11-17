-module(globlin).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([match_pattern/2, new_pattern_with/3, new_pattern/1]).
-export_type([pattern/0, pattern_options/0, pattern_error/0]).

-opaque pattern() :: {pattern, gleam@regex:regex(), pattern_options()}.

-type pattern_options() :: {pattern_options, boolean(), boolean()}.

-type pattern_error() :: absolute_pattern_from_dir_error |
    invalid_glob_star_error |
    missing_closing_bracket_error.

-spec match_pattern(pattern(), binary()) -> boolean().
match_pattern(Pattern, Path) ->
    gleam@regex:check(erlang:element(2, Pattern), Path).

-spec escape_meta_char(binary()) -> binary().
escape_meta_char(Char) ->
    case Char of
        <<"\\"/utf8>> ->
            <<"\\\\"/utf8>>;

        <<"^"/utf8>> ->
            <<"\\^"/utf8>>;

        <<"$"/utf8>> ->
            <<"\\$"/utf8>>;

        <<"."/utf8>> ->
            <<"\\."/utf8>>;

        <<"["/utf8>> ->
            <<"\\["/utf8>>;

        <<"|"/utf8>> ->
            <<"\\|"/utf8>>;

        <<"("/utf8>> ->
            <<"\\("/utf8>>;

        <<")"/utf8>> ->
            <<"\\)"/utf8>>;

        <<"?"/utf8>> ->
            <<"\\?"/utf8>>;

        <<"*"/utf8>> ->
            <<"\\*"/utf8>>;

        <<"+"/utf8>> ->
            <<"\\+"/utf8>>;

        <<"{"/utf8>> ->
            <<"\\{"/utf8>>;

        <<"]"/utf8>> ->
            <<"\\]"/utf8>>;

        <<"}"/utf8>> ->
            <<"\\}"/utf8>>;

        _ ->
            Char
    end.

-spec parse_path_chars(binary()) -> list(binary()).
parse_path_chars(Prefix) ->
    _pipe = Prefix,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun escape_meta_char/1),
    _pipe@3 = lists:reverse(_pipe@2),
    (fun(Path_chars) -> case Path_chars of
            [] ->
                Path_chars;

            [<<"/"/utf8>> | _] ->
                Path_chars;

            _ ->
                [<<"/"/utf8>> | Path_chars]
        end end)(_pipe@3).

-spec start_of_directory(list(binary())) -> boolean().
start_of_directory(Path_chars) ->
    case Path_chars of
        [] ->
            true;

        [<<""/utf8>>] ->
            true;

        [Previous | _] ->
            gleam@string:ends_with(Previous, <<"/"/utf8>>)
    end.

-spec ignore_dotfiles(list(binary()), pattern_options()) -> boolean().
ignore_dotfiles(Path_chars, Options) ->
    not erlang:element(3, Options) andalso start_of_directory(Path_chars).

-spec do_convert_pattern(
    list(binary()),
    list(binary()),
    boolean(),
    pattern_options()
) -> {ok, binary()} | {error, pattern_error()}.
do_convert_pattern(Graphemes, Path_chars, In_range, Options) ->
    case In_range of
        true ->
            case Graphemes of
                [] ->
                    {error, missing_closing_bracket_error};

                [<<"]"/utf8>> | Rest] ->
                    do_convert_pattern(
                        Rest,
                        [<<"]"/utf8>> | Path_chars],
                        false,
                        Options
                    );

                [<<"\\"/utf8>>, Second | Rest@1] ->
                    _pipe = [escape_meta_char(Second) | Path_chars],
                    do_convert_pattern(Rest@1, _pipe, true, Options);

                [First | Rest@2] ->
                    _pipe@1 = [escape_meta_char(First) | Path_chars],
                    do_convert_pattern(Rest@2, _pipe@1, true, Options)
            end;

        false ->
            case Graphemes of
                [] ->
                    _pipe@2 = Path_chars,
                    _pipe@3 = lists:reverse(_pipe@2),
                    _pipe@4 = gleam@string:concat(_pipe@3),
                    {ok, _pipe@4};

                [<<"["/utf8>>, <<"]"/utf8>> | Rest@3] ->
                    do_convert_pattern(
                        Rest@3,
                        [<<"\\[\\]"/utf8>> | Path_chars],
                        false,
                        Options
                    );

                [<<"["/utf8>>, <<"!"/utf8>> | Rest@4] ->
                    do_convert_pattern(
                        Rest@4,
                        [<<"[^"/utf8>> | Path_chars],
                        true,
                        Options
                    );

                [<<"["/utf8>>, <<"^"/utf8>> | Rest@5] ->
                    do_convert_pattern(
                        Rest@5,
                        [<<"[\\^"/utf8>> | Path_chars],
                        true,
                        Options
                    );

                [<<"["/utf8>> | Rest@6] ->
                    do_convert_pattern(
                        Rest@6,
                        [<<"["/utf8>> | Path_chars],
                        true,
                        Options
                    );

                [<<"\\"/utf8>>, Second@1 | Rest@7] ->
                    _pipe@5 = [escape_meta_char(Second@1) | Path_chars],
                    do_convert_pattern(Rest@7, _pipe@5, false, Options);

                [<<"?"/utf8>> | Rest@8] ->
                    Wildcard = case ignore_dotfiles(Path_chars, Options) of
                        true ->
                            <<"[^/.]"/utf8>>;

                        false ->
                            <<"[^/]"/utf8>>
                    end,
                    do_convert_pattern(
                        Rest@8,
                        [Wildcard | Path_chars],
                        false,
                        Options
                    );

                [<<"*"/utf8>>, <<"*"/utf8>> | Rest@9] ->
                    case {Path_chars, Rest@9} of
                        {[], []} ->
                            Wildcard@1 = case erlang:element(3, Options) of
                                true ->
                                    <<".*"/utf8>>;

                                false ->
                                    <<"([^.][^/]*(/[^.][^/]*)*)?"/utf8>>
                            end,
                            Path_chars@1 = [Wildcard@1 | Path_chars],
                            do_convert_pattern(
                                Rest@9,
                                Path_chars@1,
                                false,
                                Options
                            );

                        {[<<"/"/utf8>> | Path_chars@2], []} ->
                            Wildcard@2 = case erlang:element(3, Options) of
                                true ->
                                    <<"(/.*)?"/utf8>>;

                                false ->
                                    <<"(/[^.][^/]*)*"/utf8>>
                            end,
                            Path_chars@3 = [Wildcard@2 | Path_chars@2],
                            do_convert_pattern(
                                Rest@9,
                                Path_chars@3,
                                false,
                                Options
                            );

                        {[], [<<"/"/utf8>> | Rest@10]} ->
                            Wildcard@3 = case erlang:element(3, Options) of
                                true ->
                                    <<"(.*/)?"/utf8>>;

                                false ->
                                    <<"([^.][^/]*/)*"/utf8>>
                            end,
                            Path_chars@4 = [Wildcard@3 | Path_chars],
                            do_convert_pattern(
                                Rest@10,
                                Path_chars@4,
                                false,
                                Options
                            );

                        {[<<"/"/utf8>> | _], [<<"/"/utf8>> | Rest@10]} ->
                            Wildcard@3 = case erlang:element(3, Options) of
                                true ->
                                    <<"(.*/)?"/utf8>>;

                                false ->
                                    <<"([^.][^/]*/)*"/utf8>>
                            end,
                            Path_chars@4 = [Wildcard@3 | Path_chars],
                            do_convert_pattern(
                                Rest@10,
                                Path_chars@4,
                                false,
                                Options
                            );

                        {_, _} ->
                            {error, invalid_glob_star_error}
                    end;

                [<<"*"/utf8>> | Rest@11] ->
                    Wildcard@4 = case ignore_dotfiles(Path_chars, Options) of
                        true ->
                            <<"([^.][^/]*)?"/utf8>>;

                        false ->
                            <<"[^/]*"/utf8>>
                    end,
                    do_convert_pattern(
                        Rest@11,
                        [Wildcard@4 | Path_chars],
                        false,
                        Options
                    );

                [First@1 | Rest@12] ->
                    _pipe@6 = [escape_meta_char(First@1) | Path_chars],
                    do_convert_pattern(Rest@12, _pipe@6, false, Options)
            end
    end.

-spec convert_pattern(binary(), binary(), pattern_options()) -> {ok, binary()} |
    {error, pattern_error()}.
convert_pattern(Prefix, Pattern, Options) ->
    Graphemes = gleam@string:to_graphemes(Pattern),
    Path_chars = parse_path_chars(Prefix),
    case {Graphemes, Path_chars} of
        {[<<"/"/utf8>> | _], [_ | _]} ->
            {error, absolute_pattern_from_dir_error};

        {_, _} ->
            case do_convert_pattern(Graphemes, Path_chars, false, Options) of
                {ok, Regex_pattern} ->
                    {ok,
                        <<<<"^"/utf8, Regex_pattern/binary>>/binary, "$"/utf8>>};

                {error, Err} ->
                    {error, Err}
            end
    end.

-spec new_pattern_with(binary(), binary(), pattern_options()) -> {ok, pattern()} |
    {error, pattern_error()}.
new_pattern_with(Pattern, Directory, Options) ->
    case convert_pattern(Directory, Pattern, Options) of
        {ok, Pattern@1} ->
            Regex_options = {options, erlang:element(2, Options), false},
            case gleam@regex:compile(Pattern@1, Regex_options) of
                {ok, Regex} ->
                    {ok, {pattern, Regex, Options}};

                {error, Err} ->
                    Error_message = <<<<<<<<<<<<"Globlin Regex Compile Bug: "/utf8,
                                            "with directory '"/utf8>>/binary,
                                        Directory/binary>>/binary,
                                    "' and pattern '"/utf8>>/binary,
                                Pattern@1/binary>>/binary,
                            "': "/utf8>>/binary,
                        (erlang:element(2, Err))/binary>>,
                    erlang:error(#{gleam_error => panic,
                            message => Error_message,
                            module => <<"globlin"/utf8>>,
                            function => <<"new_pattern_with"/utf8>>,
                            line => 57})
            end;

        {error, Err@1} ->
            {error, Err@1}
    end.

-spec new_pattern(binary()) -> {ok, pattern()} | {error, pattern_error()}.
new_pattern(Pattern) ->
    new_pattern_with(Pattern, <<""/utf8>>, {pattern_options, false, false}).
