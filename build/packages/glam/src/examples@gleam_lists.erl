-module(examples@gleam_lists).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([pretty_list/1, main/0]).

-spec pretty_list(list(binary())) -> glam@doc:document().
pretty_list(List) ->
    List_item_to_document = fun(Item) ->
        glam@doc:from_string(<<<<"\""/utf8, Item/binary>>/binary, "\""/utf8>>)
    end,
    Comma = glam@doc:concat(
        [glam@doc:from_string(<<","/utf8>>), {break, <<" "/utf8>>, <<""/utf8>>}]
    ),
    Open_square = glam@doc:concat(
        [glam@doc:from_string(<<"["/utf8>>), {break, <<""/utf8>>, <<""/utf8>>}]
    ),
    Trailing_comma = glam@doc:break(<<""/utf8>>, <<","/utf8>>),
    Close_square = glam@doc:concat(
        [Trailing_comma, glam@doc:from_string(<<"]"/utf8>>)]
    ),
    _pipe = gleam@list:map(List, List_item_to_document),
    _pipe@1 = glam@doc:join(_pipe, Comma),
    _pipe@2 = glam@doc:prepend(_pipe@1, Open_square),
    _pipe@3 = glam@doc:nest(_pipe@2, 2),
    _pipe@4 = glam@doc:append(_pipe@3, Close_square),
    glam@doc:group(_pipe@4).

-spec main() -> nil.
main() ->
    gleam@list:each(
        [10, 20, 30],
        fun(Max_width) ->
            gleam@io:println(
                <<"Max width: "/utf8, (gleam@int:to_string(Max_width))/binary>>
            ),
            gleam@io:println(gleam@string:repeat(<<"-"/utf8>>, Max_width)),
            _pipe = [<<"Gleam"/utf8>>, <<"is"/utf8>>, <<"fun!"/utf8>>],
            _pipe@1 = pretty_list(_pipe),
            _pipe@2 = glam@doc:to_string(_pipe@1, Max_width),
            gleam@io:println(_pipe@2),
            gleam@io:println(<<""/utf8>>)
        end
    ).
