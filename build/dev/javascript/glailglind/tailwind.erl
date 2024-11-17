-module(tailwind).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get_args/0, run/1, install/0, install_and_run/1]).

-spec target() -> binary().
target() ->
    case {tailwind_erl:os_platform(), tailwind_erl:os_arch()} of
        {<<"win32"/utf8>>, <<"x86_64"/utf8>>} ->
            <<"windows-x64.exe"/utf8>>;

        {<<"win32"/utf8>>, <<"x64"/utf8>>} ->
            <<"windows-x64.exe"/utf8>>;

        {<<"win32"/utf8>>, <<"arm"/utf8, _/binary>>} ->
            <<"windows-arm64.exe"/utf8>>;

        {<<"darwin"/utf8>>, <<"aarch64"/utf8>>} ->
            <<"macos-arm64"/utf8>>;

        {<<"darwin"/utf8>>, <<"arm"/utf8, _/binary>>} ->
            <<"macos-arm64"/utf8>>;

        {<<"darwin"/utf8>>, <<"x86_64"/utf8>>} ->
            <<"macos-x64"/utf8>>;

        {<<"darwin"/utf8>>, <<"x64"/utf8>>} ->
            <<"macos-x64"/utf8>>;

        {<<"linux"/utf8>>, <<"aarch64"/utf8>>} ->
            <<"linux-arm64"/utf8>>;

        {<<"linux"/utf8>>, <<"arm64"/utf8>>} ->
            <<"linux-arm64"/utf8>>;

        {<<"linux"/utf8>>, <<"armv7"/utf8, _/binary>>} ->
            <<"linux-armv7"/utf8>>;

        {<<"linux"/utf8>>, <<"x86_64"/utf8>>} ->
            <<"linux-x64"/utf8>>;

        {<<"linux"/utf8>>, <<"x64"/utf8>>} ->
            <<"linux-x64"/utf8>>;

        {<<"linux"/utf8>>, <<"amd64"/utf8>>} ->
            <<"linux-x64"/utf8>>;

        {Os, Arch} ->
            erlang:error(#{gleam_error => panic,
                    message => gleam@string:join(
                        [<<"Error: TailwindCSS CLI is not available for"/utf8>>,
                            Os,
                            Arch],
                        <<" "/utf8>>
                    ),
                    module => <<"tailwind"/utf8>>,
                    function => <<"target"/utf8>>,
                    line => 183})
    end.

-spec generate_config() -> {ok, nil} | {error, binary()}.
generate_config() ->
    case simplifile_erl:is_file(<<"./tailwind.config.js"/utf8>>) of
        {ok, true} ->
            gleam@io:println(<<"TailwindCSS config already exists."/utf8>>),
            {ok, nil};

        _ ->
            _pipe = <<"
// See the Tailwind configuration guide for advanced usage
// https://tailwindcss.com/docs/configuration

let plugin = require('tailwindcss/plugin')

module.exports = {
  content: ['./src/**/*.{html,gleam}'],
  theme: {
    extend: {},
  },
  plugins: [require('@tailwindcss/forms')],
}
"/utf8>>,
            _pipe@1 = simplifile:write(<<"./tailwind.config.js"/utf8>>, _pipe),
            _pipe@2 = gleam@result:map_error(
                _pipe@1,
                fun(Err) ->
                    <<"Error: Couldn't create tailwind config. Reason: "/utf8,
                        (gleam@string:inspect(Err))/binary>>
                end
            ),
            gleam@result:map(
                _pipe@2,
                fun(_) ->
                    gleam@io:println(<<"TailwindCSS config created."/utf8>>),
                    nil
                end
            )
    end.

-spec download_bin(binary()) -> {ok, nil} | {error, binary()}.
download_bin(Path) ->
    _pipe = gleam@http@request:new(),
    _pipe@1 = gleam@http@request:set_method(_pipe, get),
    _pipe@2 = gleam@http@request:set_host(_pipe@1, <<"github.com"/utf8>>),
    _pipe@3 = gleam@http@request:set_path(_pipe@2, Path),
    _pipe@4 = gleam@http@request:map(_pipe@3, fun gleam_stdlib:identity/1),
    _pipe@5 = gleam@httpc:send_bits(_pipe@4),
    _pipe@6 = gleam@result:map_error(
        _pipe@5,
        fun(Err) ->
            <<"Error: Couldn't download tailwind. Reason: "/utf8,
                (gleam@string:inspect(Err))/binary>>
        end
    ),
    gleam@result:'try'(
        _pipe@6,
        fun(Resp) ->
            _pipe@7 = simplifile_erl:write_bits(
                <<"./build/bin/tailwindcss-cli"/utf8>>,
                erlang:element(4, Resp)
            ),
            gleam@result:map_error(
                _pipe@7,
                fun(Err@1) ->
                    <<"Error: Couldn't write tailwind binary. Reason: "/utf8,
                        (gleam@string:inspect(Err@1))/binary>>
                end
            )
        end
    ).

-spec download_tailwind(binary(), binary()) -> {ok, nil} | {error, binary()}.
download_tailwind(Version, Target) ->
    case simplifile_erl:is_file(<<"./build/bin/tailwindcss-cli"/utf8>>) of
        {ok, true} ->
            gleam@io:println(<<"TailwindCSS CLI already exists."/utf8>>),
            {ok, nil};

        _ ->
            Url_path = gleam@string:concat(
                [<<"/tailwindlabs/tailwindcss/releases/download/v"/utf8>>,
                    Version,
                    <<"/tailwindcss-"/utf8>>,
                    Target]
            ),
            _assert_subject = simplifile:create_directory_all(
                <<"./build/bin/"/utf8>>
            ),
            {ok, nil} = case _assert_subject of
                {ok, nil} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"tailwind"/utf8>>,
                                function => <<"download_tailwind"/utf8>>,
                                line => 206})
            end,
            gleam@io:println(
                <<<<"Downloading TailwindCSS "/utf8, Target/binary>>/binary,
                    "..."/utf8>>
            ),
            _pipe = download_bin(Url_path),
            gleam@result:'try'(
                _pipe,
                fun(_) ->
                    _pipe@1 = simplifile_erl:set_permissions_octal(
                        <<"./build/bin/tailwindcss-cli"/utf8>>,
                        8#755
                    ),
                    gleam@result:map_error(
                        _pipe@1,
                        fun(Err) ->
                            <<"Error: Can't change tailwindcli permissions. Reason: "/utf8,
                                (gleam@string:inspect(Err))/binary>>
                        end
                    )
                end
            )
    end.

-spec get_config() -> {ok, gleam@dict:dict(binary(), tom:toml())} |
    {error, binary()}.
get_config() ->
    _pipe = simplifile:read(<<"./gleam.toml"/utf8>>),
    _pipe@1 = gleam@result:map_error(
        _pipe,
        fun(Err) ->
            <<"Error: Couldn't read config. Reason: "/utf8,
                (gleam@string:inspect(Err))/binary>>
        end
    ),
    gleam@result:'try'(_pipe@1, fun(Config) -> _pipe@2 = tom:parse(Config),
            gleam@result:replace_error(
                _pipe@2,
                <<"Error: Couldn't parse config."/utf8>>
            ) end).

-spec get_config_string(binary()) -> {ok, binary()} | {error, binary()}.
get_config_string(Key) ->
    _pipe = get_config(),
    gleam@result:'try'(
        _pipe,
        fun(Parsed) ->
            _pipe@1 = tom:get_string(Parsed, [<<"tailwind"/utf8>>, Key]),
            gleam@result:replace_error(
                _pipe@1,
                <<<<"Error: Config key \""/utf8, Key/binary>>/binary,
                    "\" not found."/utf8>>
            )
        end
    ).

-spec get_args() -> {ok, list(binary())} | {error, binary()}.
get_args() ->
    _pipe = get_config(),
    gleam@result:'try'(
        _pipe,
        fun(Parsed) ->
            _pipe@1 = tom:get_array(
                Parsed,
                [<<"tailwind"/utf8>>, <<"args"/utf8>>]
            ),
            _pipe@2 = gleam@result:replace_error(
                _pipe@1,
                <<"Error: Config arguments not found. Is the \"args\" key set in the \"gleam.toml\"?"/utf8>>
            ),
            gleam@result:map(
                _pipe@2,
                fun(Args) -> gleam@list:map(Args, fun(Arg) -> case Arg of
                                {string, A} ->
                                    A;

                                _ ->
                                    <<""/utf8>>
                            end end) end
            )
        end
    ).

-spec get_cli_path() -> binary().
get_cli_path() ->
    _pipe = get_config_string(<<"path"/utf8>>),
    gleam@result:unwrap(_pipe, <<"./build/bin/tailwindcss-cli"/utf8>>).

-spec run(list(binary())) -> {ok, binary()} | {error, binary()}.
run(Args) ->
    Cli = get_cli_path(),
    case simplifile_erl:is_file(Cli) of
        {ok, true} ->
            _pipe = shellout:command(Cli, Args, <<"."/utf8>>, []),
            gleam@result:map_error(
                _pipe,
                fun(Err) -> gleam@pair:second(Err) end
            );

        _ ->
            {error, <<"Error: TailwindCSS CLI isn't installed."/utf8>>}
    end.

-spec get_tailwind_version() -> binary().
get_tailwind_version() ->
    _pipe = get_config_string(<<"version"/utf8>>),
    gleam@result:unwrap(_pipe, <<"3.4.1"/utf8>>).

-spec install() -> {ok, nil} | {error, binary()}.
install() ->
    gleam@io:println(<<"Installing TailwindCSS..."/utf8>>),
    Output = begin
        _pipe = generate_config(),
        gleam@result:'try'(
            _pipe,
            fun(_) ->
                Version = get_tailwind_version(),
                download_tailwind(Version, target())
            end
        )
    end,
    case Output of
        {ok, _} ->
            gleam@io:println(<<"TailwindCSS installed!"/utf8>>),
            {ok, nil};

        {error, Err} ->
            gleam@io:println(Err),
            {error, Err}
    end.

-spec install_and_run(list(binary())) -> {ok, binary()} | {error, binary()}.
install_and_run(Args) ->
    _pipe = install(),
    gleam@result:'try'(_pipe, fun(_) -> run(Args) end).
