-module(lustre_dev_tools@cli).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([run/2, return/1, throw/1, from_result/1, do/2, in/1, map/2, 'try'/2, log/2, success/2, notify/2, mute/0, unmute/0, template/2, get_config/0, get_name/0, get_flags/0, get_config_value/5, get_int/4, get_string/4, get_bool/4]).
-export_type([cli/1, env/0, spinner_status/0]).

-opaque cli(XVX) :: {cli,
        fun((env()) -> {env(),
            {ok, XVX} | {error, lustre_dev_tools@error:error()}})}.

-type env() :: {env,
        boolean(),
        spinner_status(),
        glint:flags(),
        lustre_dev_tools@project:config()}.

-type spinner_status() :: {running, spinner:spinner(), binary()} | paused.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 40).
-spec run(cli(XVY), glint:flags()) -> {ok, XVY} |
    {error, lustre_dev_tools@error:error()}.
run(Step, Flags) ->
    gleam@result:'try'(
        lustre_dev_tools@project:config(),
        fun(Config) ->
            Env = {env, false, paused, Flags, Config},
            {Env@1, Result} = (erlang:element(2, Step))(Env),
            case erlang:element(3, Env@1) of
                {running, Spinner, _} ->
                    spinner:stop(Spinner);

                paused ->
                    nil
            end,
            case {Result, erlang:element(3, Env@1)} of
                {{error, _}, {running, _, Message}} ->
                    gleam@io:println(
                        <<"❌ "/utf8,
                            (gleam_community@ansi:red(Message))/binary>>
                    );

                {{error, _}, _} ->
                    nil;

                {{ok, _}, _} ->
                    nil
            end,
            Result
        end
    ).

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 64).
-spec return(XWC) -> cli(XWC).
return(Value) ->
    {cli, fun(Env) -> {Env, {ok, Value}} end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 72).
-spec throw(lustre_dev_tools@error:error()) -> cli(any()).
throw(Error) ->
    {cli, fun(Env) -> {Env, {error, Error}} end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 78).
-spec from_result({ok, XWG} | {error, lustre_dev_tools@error:error()}) -> cli(XWG).
from_result(Result) ->
    {cli, fun(Env) -> {Env, Result} end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 88).
-spec do(cli(XWK), fun((XWK) -> cli(XWM))) -> cli(XWM).
do(Step, Next) ->
    {cli,
        fun(Env) ->
            {Env@1, Result} = (erlang:element(2, Step))(Env),
            case Result of
                {ok, Value} ->
                    (erlang:element(2, Next(Value)))(Env@1);

                {error, Error} ->
                    case erlang:element(3, Env@1) of
                        {running, Spinner, _} ->
                            spinner:stop(Spinner);

                        paused ->
                            nil
                    end,
                    {Env@1, {error, Error}}
            end
        end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 105).
-spec in(fun(() -> XWP)) -> cli(XWP).
in(Value) ->
    {cli, fun(Env) -> {Env, {ok, Value()}} end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 111).
-spec map(cli(XWR), fun((XWR) -> XWT)) -> cli(XWT).
map(Step, Next) ->
    {cli,
        fun(Env) ->
            {Env@1, Result} = (erlang:element(2, Step))(Env),
            Result@1 = gleam@result:map(Result, Next),
            {Env@1, Result@1}
        end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 121).
-spec 'try'(
    {ok, XWV} | {error, lustre_dev_tools@error:error()},
    fun((XWV) -> cli(XWY))
) -> cli(XWY).
'try'(Result, Next) ->
    {cli, fun(Env) -> case Result of
                {ok, A} ->
                    (erlang:element(2, Next(A)))(Env);

                {error, Error} ->
                    case erlang:element(3, Env) of
                        {running, Spinner, _} ->
                            spinner:stop(Spinner);

                        paused ->
                            nil
                    end,
                    {Env, {error, Error}}
            end end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 141).
-spec log(binary(), fun(() -> cli(XXB))) -> cli(XXB).
log(Message, Next) ->
    {cli,
        fun(Env) ->
            Env@1 = case erlang:element(2, Env) of
                true ->
                    Env;

                false ->
                    erlang:setelement(3, Env, case erlang:element(3, Env) of
                            paused ->
                                {running,
                                    begin
                                        _pipe = spinner:new(Message),
                                        _pipe@1 = spinner:with_colour(
                                            _pipe,
                                            fun gleam_community@ansi:magenta/1
                                        ),
                                        _pipe@2 = spinner:with_frames(
                                            _pipe@1,
                                            [<<"⠋"/utf8>>,
                                                <<"⠙"/utf8>>,
                                                <<"⠹"/utf8>>,
                                                <<"⠸"/utf8>>,
                                                <<"⠼"/utf8>>,
                                                <<"⠴"/utf8>>,
                                                <<"⠦"/utf8>>,
                                                <<"⠧"/utf8>>,
                                                <<"⠇"/utf8>>,
                                                <<"⠏"/utf8>>]
                                        ),
                                        spinner:start(_pipe@2)
                                    end,
                                    Message};

                            {running, Spinner, _} ->
                                spinner:set_text(Spinner, Message),
                                {running, Spinner, Message}
                        end)
            end,
            (erlang:element(2, Next()))(Env@1)
        end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 169).
-spec success(binary(), fun(() -> cli(XXE))) -> cli(XXE).
success(Message, Next) ->
    {cli,
        fun(Env) ->
            Env@1 = erlang:setelement(3, Env, case erlang:element(3, Env) of
                    paused ->
                        paused;

                    {running, Spinner, _} ->
                        spinner:stop(Spinner),
                        paused
                end),
            case erlang:element(2, Env@1) of
                true ->
                    nil;

                false ->
                    gleam@io:println(
                        <<"✅ "/utf8,
                            (gleam_community@ansi:green(Message))/binary>>
                    )
            end,
            (erlang:element(2, Next()))(Env@1)
        end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 191).
-spec notify(binary(), fun(() -> cli(XXH))) -> cli(XXH).
notify(Message, Next) ->
    {cli,
        fun(Env) ->
            Env@1 = erlang:setelement(3, Env, case erlang:element(3, Env) of
                    paused ->
                        paused;

                    {running, Spinner, _} ->
                        spinner:stop(Spinner),
                        paused
                end),
            case erlang:element(2, Env@1) of
                true ->
                    nil;

                false ->
                    gleam@io:println(gleam_community@ansi:bright_cyan(Message))
            end,
            (erlang:element(2, Next()))(Env@1)
        end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 213).
-spec mute() -> cli(nil).
mute() ->
    {cli, fun(Env) -> {erlang:setelement(2, Env, true), {ok, nil}} end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 219).
-spec unmute() -> cli(nil).
unmute() ->
    {cli, fun(Env) -> {erlang:setelement(2, Env, false), {ok, nil}} end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 227).
-spec template(binary(), fun((binary()) -> cli(XXM))) -> cli(XXM).
template(Name, Next) ->
    {cli,
        fun(Env) ->
            _assert_subject = gleam_erlang_ffi:priv_directory(
                <<"lustre_dev_tools"/utf8>>
            ),
            {ok, Priv} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"lustre_dev_tools/cli"/utf8>>,
                                function => <<"template"/utf8>>,
                                line => 229})
            end,
            case simplifile:read(
                <<<<Priv/binary, "/template/"/utf8>>/binary, Name/binary>>
            ) of
                {ok, Template} ->
                    (erlang:element(2, Next(Template)))(Env);

                {error, Error} ->
                    {Env, {error, {template_missing, Name, Error}}}
            end
        end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 239).
-spec get_config() -> cli(lustre_dev_tools@project:config()).
get_config() ->
    {cli, fun(Env) -> {Env, {ok, erlang:element(5, Env)}} end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 245).
-spec get_name() -> cli(binary()).
get_name() ->
    {cli,
        fun(Env) -> {Env, {ok, erlang:element(2, erlang:element(5, Env))}} end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 253).
-spec get_flags() -> cli(glint:flags()).
get_flags() ->
    {cli, fun(Env) -> {Env, {ok, erlang:element(4, Env)}} end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 259).
-spec get_config_value(
    binary(),
    XXS,
    list(binary()),
    fun((gleam@dict:dict(binary(), tom:toml()), list(binary())) -> {ok, XXS} |
        {error, any()}),
    fun((glint:flags()) -> {ok, XXS} | {error, any()})
) -> cli(XXS).
get_config_value(Name, Fallback, Namespace, Toml, Flag) ->
    {cli,
        fun(Env) ->
            Toml_path = gleam@list:concat(
                [[<<"lustre-dev"/utf8>>], Namespace, [Name]]
            ),
            Value = begin
                _pipe = gleam@result:'or'(
                    gleam@result:nil_error(Flag(erlang:element(4, Env))),
                    gleam@result:nil_error(
                        Toml(
                            erlang:element(4, erlang:element(5, Env)),
                            Toml_path
                        )
                    )
                ),
                gleam@result:unwrap(_pipe, Fallback)
            end,
            {Env, {ok, Value}}
        end}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 278).
-spec get_int(
    binary(),
    integer(),
    list(binary()),
    fun((glint:flags()) -> {ok, integer()} | {error, any()})
) -> cli(integer()).
get_int(Name, Fallback, Namespace, Flag) ->
    get_config_value(Name, Fallback, Namespace, fun tom:get_int/2, Flag).

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 287).
-spec get_string(
    binary(),
    binary(),
    list(binary()),
    fun((glint:flags()) -> {ok, binary()} | {error, any()})
) -> cli(binary()).
get_string(Name, Fallback, Namespace, Flag) ->
    get_config_value(Name, Fallback, Namespace, fun tom:get_string/2, Flag).

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/cli.gleam", 296).
-spec get_bool(
    binary(),
    boolean(),
    list(binary()),
    fun((glint:flags()) -> {ok, boolean()} | {error, any()})
) -> cli(boolean()).
get_bool(Name, Fallback, Namespace, Flag) ->
    get_config_value(Name, Fallback, Namespace, fun tom:get_bool/2, Flag).
