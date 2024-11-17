-module(examples@json).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([example_json/0, json_to_doc/1, main/0]).
-export_type([json/0]).

-type json() :: {string, binary()} |
    {number, float()} |
    {bool, boolean()} |
    null |
    {array, list(json())} |
    {object, list({binary(), json()})}.

-spec colon() -> glam@doc:document().
colon() ->
    glam@doc:from_string(<<":"/utf8>>).

-spec comma() -> glam@doc:document().
comma() ->
    glam@doc:from_string(<<","/utf8>>).

-spec bool_to_doc(boolean()) -> glam@doc:document().
bool_to_doc(Bool) ->
    _pipe = gleam@bool:to_string(Bool),
    _pipe@1 = gleam@string:lowercase(_pipe),
    glam@doc:from_string(_pipe@1).

-spec parenthesise(glam@doc:document(), binary(), binary()) -> glam@doc:document().
parenthesise(Doc, Open, Close) ->
    _pipe = Doc,
    _pipe@1 = glam@doc:prepend_docs(
        _pipe,
        [glam@doc:from_string(Open), {break, <<" "/utf8>>, <<""/utf8>>}]
    ),
    _pipe@2 = glam@doc:nest(_pipe@1, 2),
    _pipe@3 = glam@doc:append_docs(
        _pipe@2,
        [{break, <<" "/utf8>>, <<""/utf8>>}, glam@doc:from_string(Close)]
    ),
    glam@doc:group(_pipe@3).

-spec example_json() -> json().
example_json() ->
    {object,
        [{<<"title"/utf8>>, {string, <<"The sundial"/utf8>>}},
            {<<"author"/utf8>>, {string, <<"Shirley Jackson"/utf8>>}},
            {<<"publication_year"/utf8>>, {number, 1958.0}},
            {<<"read"/utf8>>, {bool, true}},
            {<<"characters"/utf8>>,
                {array,
                    [{string, <<"Mrs. Halloran"/utf8>>},
                        {string, <<"Essex"/utf8>>},
                        {string, <<"Captain Scarabombardon"/utf8>>}]}},
            {<<"average_rating"/utf8>>, {number, 5.0}},
            {<<"ratings"/utf8>>,
                {array,
                    [{object,
                            [{<<"from"/utf8>>, {string, <<"Ben"/utf8>>}},
                                {<<"value"/utf8>>, {number, 5.0}}]},
                        {object,
                            [{<<"from"/utf8>>, {string, <<"Giacomo"/utf8>>}},
                                {<<"value"/utf8>>, {number, 5.0}}]}]}}]}.

-spec array_to_doc(list(json())) -> glam@doc:document().
array_to_doc(Objects) ->
    _pipe = gleam@list:map(Objects, fun json_to_doc/1),
    _pipe@1 = glam@doc:concat_join(
        _pipe,
        [comma(), {break, <<" "/utf8>>, <<""/utf8>>}]
    ),
    parenthesise(_pipe@1, <<"["/utf8>>, <<"]"/utf8>>).

-spec json_to_doc(json()) -> glam@doc:document().
json_to_doc(Json) ->
    case Json of
        {string, String} ->
            glam@doc:from_string(
                <<<<"\""/utf8, String/binary>>/binary, "\""/utf8>>
            );

        {number, Number} ->
            glam@doc:from_string(gleam@float:to_string(Number));

        {bool, Bool} ->
            bool_to_doc(Bool);

        null ->
            glam@doc:from_string(<<"null"/utf8>>);

        {array, Objects} ->
            array_to_doc(Objects);

        {object, Fields} ->
            object_to_doc(Fields)
    end.

-spec field_to_doc({binary(), json()}) -> glam@doc:document().
field_to_doc(Field) ->
    {Name, Value} = Field,
    Name_doc = glam@doc:from_string(Name),
    Value_doc = json_to_doc(Value),
    _pipe = [Name_doc, colon(), glam@doc:from_string(<<" "/utf8>>), Value_doc],
    glam@doc:concat(_pipe).

-spec object_to_doc(list({binary(), json()})) -> glam@doc:document().
object_to_doc(Fields) ->
    _pipe = gleam@list:map(Fields, fun field_to_doc/1),
    _pipe@1 = glam@doc:concat_join(
        _pipe,
        [comma(), {break, <<" "/utf8>>, <<""/utf8>>}]
    ),
    parenthesise(_pipe@1, <<"{"/utf8>>, <<"}"/utf8>>).

-spec main() -> nil.
main() ->
    gleam@list:each(
        [30, 50, 80],
        fun(Max_width) ->
            gleam@io:println(
                <<"Max width: "/utf8, (gleam@int:to_string(Max_width))/binary>>
            ),
            gleam@io:println(gleam@string:repeat(<<"-"/utf8>>, Max_width)),
            _pipe = json_to_doc(example_json()),
            _pipe@1 = glam@doc:to_string(_pipe, Max_width),
            gleam@io:println(_pipe@1),
            gleam@io:println(<<""/utf8>>)
        end
    ).
