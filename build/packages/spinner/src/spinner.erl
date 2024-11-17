-module(spinner).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([with_frames/2, with_colour/2, set_text/2, set_colour/2, stop/1, start/1, new/1]).
-export_type([spinner/0, state/0, builder/0]).

-opaque spinner() :: {spinner,
        repeatedly:repeater(state()),
        glearray:array(binary())}.

-type state() :: {state, binary(), fun((binary()) -> binary())}.

-opaque builder() :: {builder,
        list(binary()),
        binary(),
        fun((binary()) -> binary())}.

-file("/Users/louis/src/gleam/spinner/src/spinner.gleam", 52).
-spec with_frames(builder(), list(binary())) -> builder().
with_frames(Builder, Frames) ->
    erlang:setelement(2, Builder, Frames).

-file("/Users/louis/src/gleam/spinner/src/spinner.gleam", 56).
-spec with_colour(builder(), fun((binary()) -> binary())) -> builder().
with_colour(Builder, Colour) ->
    erlang:setelement(4, Builder, Colour).

-file("/Users/louis/src/gleam/spinner/src/spinner.gleam", 70).
-spec set_text(spinner(), binary()) -> nil.
set_text(Spinner, Text) ->
    repeatedly_ffi:update_state(
        erlang:element(2, Spinner),
        fun(State) -> erlang:setelement(2, State, Text) end
    ).

-file("/Users/louis/src/gleam/spinner/src/spinner.gleam", 76).
-spec set_colour(spinner(), fun((binary()) -> binary())) -> nil.
set_colour(Spinner, Colour) ->
    repeatedly_ffi:update_state(
        erlang:element(2, Spinner),
        fun(State) -> erlang:setelement(3, State, Colour) end
    ).

-file("/Users/louis/src/gleam/spinner/src/spinner.gleam", 106).
-spec frame(glearray:array(binary()), integer()) -> binary().
frame(Frames, Index) ->
    _assert_subject = glearray:get(Frames, case erlang:tuple_size(Frames) of
            0 -> 0;
            Gleam@denominator -> Index rem Gleam@denominator
        end),
    {ok, Frame} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"spinner"/utf8>>,
                        function => <<"frame"/utf8>>,
                        line => 107})
    end,
    Frame.

-file("/Users/louis/src/gleam/spinner/src/spinner.gleam", 88).
-spec stop(spinner()) -> nil.
stop(Spinner) ->
    repeatedly_ffi:stop(erlang:element(2, Spinner)),
    Show_cursor = <<"\x{001b}[?25h"/utf8>>,
    gleam@io:print(
        <<<<"\x{001b}[2K"/utf8, "\r"/utf8>>/binary, Show_cursor/binary>>
    ).

-file("/Users/louis/src/gleam/spinner/src/spinner.gleam", 94).
-spec print(glearray:array(binary()), state(), integer()) -> nil.
print(Frames, State, Index) ->
    Hide_cursor = <<"\x{001b}[?25l"/utf8>>,
    gleam@io:print(
        <<<<<<<<<<Hide_cursor/binary, "\x{001b}[2K"/utf8>>/binary, "\r"/utf8>>/binary,
                    ((erlang:element(3, State))(frame(Frames, Index)))/binary>>/binary,
                " "/utf8>>/binary,
            (erlang:element(2, State))/binary>>
    ).

-file("/Users/louis/src/gleam/spinner/src/spinner.gleam", 60).
-spec start(builder()) -> spinner().
start(Builder) ->
    Frames = erlang:list_to_tuple(erlang:element(2, Builder)),
    Repeater = repeatedly_ffi:call(
        80,
        {state, erlang:element(3, Builder), erlang:element(4, Builder)},
        fun(State, I) ->
            print(Frames, State, I),
            State
        end
    ),
    {spinner, Repeater, Frames}.

-file("/Users/louis/src/gleam/spinner/src/spinner.gleam", 48).
-spec new(binary()) -> builder().
new(Text) ->
    {builder,
        [<<"⠋"/utf8>>,
            <<"⠙"/utf8>>,
            <<"⠹"/utf8>>,
            <<"⠸"/utf8>>,
            <<"⠼"/utf8>>,
            <<"⠴"/utf8>>,
            <<"⠦"/utf8>>,
            <<"⠧"/utf8>>,
            <<"⠇"/utf8>>,
            <<"⠏"/utf8>>],
        Text,
        fun gleam_community@ansi:magenta/1}.
