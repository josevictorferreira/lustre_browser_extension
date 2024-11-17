-module(examples@error_messages).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([errors_to_doc/2, example_errors/0, main/0]).
-export_type([span/0, error/0]).

-type span() :: {span, integer(), integer(), integer()}.

-type error() :: {error, binary(), binary(), binary(), span()}.

-spec header_doc(binary(), binary()) -> glam@doc:document().
header_doc(Code, Name) ->
    _pipe = [<<"["/utf8>>, Code, <<"]:"/utf8>>, Name],
    _pipe@1 = gleam@string:join(_pipe, <<" "/utf8>>),
    glam@doc:from_string(_pipe@1).

-spec line_prefix(integer()) -> binary().
line_prefix(Line_number) ->
    <<(gleam@int:to_string(Line_number + 1))/binary, " | "/utf8>>.

-spec line_doc(gleam@dict:dict(integer(), binary()), integer()) -> {glam@doc:document(),
    integer()}.
line_doc(Source_code, Line_number) ->
    _assert_subject = gleam@dict:get(Source_code, Line_number),
    {ok, Line} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"examples/error_messages"/utf8>>,
                        function => <<"line_doc"/utf8>>,
                        line => 92})
    end,
    Prefix = line_prefix(Line_number),
    Prefix_size = gleam@string:length(Prefix),
    {glam@doc:from_string(<<Prefix/binary, Line/binary>>), Prefix_size}.

-spec underlined_pointer(integer()) -> glam@doc:document().
underlined_pointer(Length) ->
    _pipe = [glam@doc:from_string(
            <<"┬"/utf8,
                (gleam@string:repeat(<<"─"/utf8>>, Length - 1))/binary>>
        ),
        {line, 1},
        glam@doc:from_string(<<"╰─ "/utf8>>)],
    glam@doc:concat(_pipe).

-spec flexible_text(binary()) -> glam@doc:document().
flexible_text(Text) ->
    To_flexible_line = fun(Line) ->
        _pipe = gleam@string:split(Line, <<" "/utf8>>),
        _pipe@1 = gleam@list:map(_pipe, fun glam@doc:from_string/1),
        _pipe@2 = glam@doc:join(
            _pipe@1,
            {flex_break, <<" "/utf8>>, <<""/utf8>>}
        ),
        glam@doc:group(_pipe@2)
    end,
    _pipe@3 = gleam@string:split(Text, <<"\n"/utf8>>),
    _pipe@4 = gleam@list:map(_pipe@3, To_flexible_line),
    _pipe@5 = glam@doc:join(_pipe@4, {line, 1}),
    glam@doc:group(_pipe@5).

-spec message_doc(binary(), integer()) -> glam@doc:document().
message_doc(Message, Length) ->
    _pipe@1 = [underlined_pointer(Length),
        begin
            _pipe = flexible_text(Message),
            glam@doc:nest(_pipe, 3)
        end],
    glam@doc:concat(_pipe@1).

-spec error_to_doc(gleam@dict:dict(integer(), binary()), error()) -> glam@doc:document().
error_to_doc(Source_code, Error) ->
    Underline_size = (erlang:element(4, erlang:element(5, Error)) - erlang:element(
        3,
        erlang:element(5, Error)
    ))
    + 1,
    {Line_doc, Prefix_size} = line_doc(
        Source_code,
        erlang:element(2, erlang:element(5, Error))
    ),
    _pipe@2 = [header_doc(erlang:element(2, Error), erlang:element(3, Error)),
        {line, 1},
        Line_doc,
        begin
            _pipe = [{line, 1},
                message_doc(erlang:element(4, Error), Underline_size)],
            _pipe@1 = glam@doc:concat(_pipe),
            glam@doc:nest(
                _pipe@1,
                erlang:element(3, erlang:element(5, Error)) + Prefix_size
            )
        end],
    glam@doc:concat(_pipe@2).

-spec errors_to_doc(binary(), list(error())) -> glam@doc:document().
errors_to_doc(Source_code, Errors) ->
    Source_code@1 = begin
        _pipe = gleam@string:split(Source_code, <<"\n"/utf8>>),
        _pipe@1 = gleam@list:index_map(_pipe, fun(Line, I) -> {I, Line} end),
        maps:from_list(_pipe@1)
    end,
    _pipe@2 = gleam@list:map(
        Errors,
        fun(_capture) -> error_to_doc(Source_code@1, _capture) end
    ),
    glam@doc:join(_pipe@2, glam@doc:lines(2)).

-spec example_errors() -> list(error()).
example_errors() ->
    [{error,
            <<"E001"/utf8>>,
            <<"Unused function"/utf8>>,
            <<"This function is unused!\nYou can safely remove it or make it public with the `pub` keyword."/utf8>>,
            {span, 0, 3, 7}},
        {error,
            <<"E011"/utf8>>,
            <<"Unknown variable"/utf8>>,
            <<"The name `println` is not in scope here.\nDid you mean to use `io.println`?"/utf8>>,
            {span, 1, 2, 8}}].

-spec main() -> nil.
main() ->
    _pipe = errors_to_doc(
        <<"fn greet(message: String) -> Nil {\n  println(\"Hello\" <> message)\n}"/utf8>>,
        example_errors()
    ),
    _pipe@1 = glam@doc:to_string(_pipe, 30),
    gleam@io:println(_pipe@1).
