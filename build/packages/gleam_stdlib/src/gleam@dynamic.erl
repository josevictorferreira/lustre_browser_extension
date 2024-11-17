-module(gleam@dynamic).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([from/1, dynamic/1, bit_array/1, classify/1, int/1, float/1, bool/1, shallow_list/1, optional/1, any/1, decode1/2, result/2, list/1, string/1, field/2, optional_field/2, element/2, tuple2/2, tuple3/3, tuple4/4, tuple5/5, tuple6/6, dict/2, decode2/3, decode3/4, decode4/5, decode5/6, decode6/7, decode7/8, decode8/9, decode9/10]).
-export_type([dynamic_/0, decode_error/0, unknown_tuple/0]).

-type dynamic_() :: any().

-type decode_error() :: {decode_error, binary(), binary(), list(binary())}.

-type unknown_tuple() :: any().

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 31).
-spec from(any()) -> dynamic_().
from(A) ->
    gleam_stdlib:identity(A).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 39).
-spec dynamic(dynamic_()) -> {ok, dynamic_()} | {error, list(decode_error())}.
dynamic(Value) ->
    {ok, Value}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 60).
-spec bit_array(dynamic_()) -> {ok, bitstring()} | {error, list(decode_error())}.
bit_array(Data) ->
    gleam_stdlib:decode_bit_array(Data).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 107).
-spec put_expected(decode_error(), binary()) -> decode_error().
put_expected(Error, Expected) ->
    erlang:setelement(2, Error, Expected).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 118).
-spec classify(dynamic_()) -> binary().
classify(Data) ->
    gleam_stdlib:classify_dynamic(Data).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 141).
-spec int(dynamic_()) -> {ok, integer()} | {error, list(decode_error())}.
int(Data) ->
    gleam_stdlib:decode_int(Data).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 164).
-spec float(dynamic_()) -> {ok, float()} | {error, list(decode_error())}.
float(Data) ->
    gleam_stdlib:decode_float(Data).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 187).
-spec bool(dynamic_()) -> {ok, boolean()} | {error, list(decode_error())}.
bool(Data) ->
    gleam_stdlib:decode_bool(Data).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 213).
-spec shallow_list(dynamic_()) -> {ok, list(dynamic_())} |
    {error, list(decode_error())}.
shallow_list(Value) ->
    gleam_stdlib:decode_list(Value).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 356).
-spec optional(fun((dynamic_()) -> {ok, CLK} | {error, list(decode_error())})) -> fun((dynamic_()) -> {ok,
        gleam@option:option(CLK)} |
    {error, list(decode_error())}).
optional(Decode) ->
    fun(Value) -> gleam_stdlib:decode_option(Value, Decode) end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 491).
-spec at_least_decode_tuple_error(integer(), dynamic_()) -> {ok, any()} |
    {error, list(decode_error())}.
at_least_decode_tuple_error(Size, Data) ->
    S = case Size of
        1 ->
            <<""/utf8>>;

        _ ->
            <<"s"/utf8>>
    end,
    Error = begin
        _pipe = [<<"Tuple of at least "/utf8>>,
            gleam@int:to_string(Size),
            <<" element"/utf8>>,
            S],
        _pipe@1 = gleam@string_tree:from_strings(_pipe),
        _pipe@2 = gleam@string_tree:to_string(_pipe@1),
        {decode_error, _pipe@2, classify(Data), []}
    end,
    {error, [Error]}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 1021).
-spec any(list(fun((dynamic_()) -> {ok, CPK} | {error, list(decode_error())}))) -> fun((dynamic_()) -> {ok,
        CPK} |
    {error, list(decode_error())}).
any(Decoders) ->
    fun(Data) -> case Decoders of
            [] ->
                {error,
                    [{decode_error, <<"another type"/utf8>>, classify(Data), []}]};

            [Decoder | Decoders@1] ->
                case Decoder(Data) of
                    {ok, Decoded} ->
                        {ok, Decoded};

                    {error, _} ->
                        (any(Decoders@1))(Data)
                end
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 1517).
-spec all_errors({ok, any()} | {error, list(decode_error())}) -> list(decode_error()).
all_errors(Result) ->
    case Result of
        {ok, _} ->
            [];

        {error, Errors} ->
            Errors
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 1054).
-spec decode1(
    fun((CPO) -> CPP),
    fun((dynamic_()) -> {ok, CPO} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, CPP} | {error, list(decode_error())}).
decode1(Constructor, T1) ->
    fun(Value) -> case T1(Value) of
            {ok, A} ->
                {ok, Constructor(A)};

            A@1 ->
                {error, all_errors(A@1)}
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 563).
-spec push_path(decode_error(), any()) -> decode_error().
push_path(Error, Name) ->
    Name@1 = gleam_stdlib:identity(Name),
    Decoder = any(
        [fun string/1,
            fun(X) -> gleam@result:map(int(X), fun gleam@int:to_string/1) end]
    ),
    Name@3 = case Decoder(Name@1) of
        {ok, Name@2} ->
            Name@2;

        {error, _} ->
            _pipe = [<<"<"/utf8>>, classify(Name@1), <<">"/utf8>>],
            _pipe@1 = gleam@string_tree:from_strings(_pipe),
            gleam@string_tree:to_string(_pipe@1)
    end,
    erlang:setelement(4, Error, [Name@3 | erlang:element(4, Error)]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 244).
-spec result(
    fun((dynamic_()) -> {ok, CKS} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CKU} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {ok, CKS} | {error, CKU}} |
    {error, list(decode_error())}).
result(Decode_ok, Decode_error) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_result(Value),
            fun(Inner_result) -> case Inner_result of
                    {ok, Raw} ->
                        gleam@result:'try'(
                            begin
                                _pipe = Decode_ok(Raw),
                                map_errors(
                                    _pipe,
                                    fun(_capture) ->
                                        push_path(_capture, <<"ok"/utf8>>)
                                    end
                                )
                            end,
                            fun(Value@1) -> {ok, {ok, Value@1}} end
                        );

                    {error, Raw@1} ->
                        gleam@result:'try'(
                            begin
                                _pipe@1 = Decode_error(Raw@1),
                                map_errors(
                                    _pipe@1,
                                    fun(_capture@1) ->
                                        push_path(_capture@1, <<"error"/utf8>>)
                                    end
                                )
                            end,
                            fun(Value@2) -> {ok, {error, Value@2}} end
                        )
                end end
        )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 301).
-spec list(fun((dynamic_()) -> {ok, CLF} | {error, list(decode_error())})) -> fun((dynamic_()) -> {ok,
        list(CLF)} |
    {error, list(decode_error())}).
list(Decoder_type) ->
    fun(Dynamic) ->
        gleam@result:'try'(shallow_list(Dynamic), fun(List) -> _pipe = List,
                _pipe@1 = gleam@list:try_map(_pipe, Decoder_type),
                map_errors(
                    _pipe@1,
                    fun(_capture) -> push_path(_capture, <<"*"/utf8>>) end
                ) end)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 87).
-spec map_errors(
    {ok, CJT} | {error, list(decode_error())},
    fun((decode_error()) -> decode_error())
) -> {ok, CJT} | {error, list(decode_error())}.
map_errors(Result, F) ->
    gleam@result:map_error(
        Result,
        fun(_capture) -> gleam@list:map(_capture, F) end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 95).
-spec decode_string(dynamic_()) -> {ok, binary()} |
    {error, list(decode_error())}.
decode_string(Data) ->
    _pipe = bit_array(Data),
    _pipe@1 = map_errors(
        _pipe,
        fun(_capture) -> put_expected(_capture, <<"String"/utf8>>) end
    ),
    gleam@result:'try'(
        _pipe@1,
        fun(Raw) -> case gleam@bit_array:to_string(Raw) of
                {ok, String} ->
                    {ok, String};

                {error, nil} ->
                    {error,
                        [{decode_error,
                                <<"String"/utf8>>,
                                <<"BitArray"/utf8>>,
                                []}]}
            end end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 83).
-spec string(dynamic_()) -> {ok, binary()} | {error, list(decode_error())}.
string(Data) ->
    decode_string(Data).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 385).
-spec field(
    any(),
    fun((dynamic_()) -> {ok, CLU} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, CLU} | {error, list(decode_error())}).
field(Name, Inner_type) ->
    fun(Value) ->
        Missing_field_error = {decode_error,
            <<"field"/utf8>>,
            <<"nothing"/utf8>>,
            []},
        gleam@result:'try'(
            gleam_stdlib:decode_field(Value, Name),
            fun(Maybe_inner) -> _pipe = Maybe_inner,
                _pipe@1 = gleam@option:to_result(_pipe, [Missing_field_error]),
                _pipe@2 = gleam@result:'try'(_pipe@1, Inner_type),
                map_errors(
                    _pipe@2,
                    fun(_capture) -> push_path(_capture, Name) end
                ) end
        )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 427).
-spec optional_field(
    any(),
    fun((dynamic_()) -> {ok, CLY} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, gleam@option:option(CLY)} |
    {error, list(decode_error())}).
optional_field(Name, Inner_type) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_field(Value, Name),
            fun(Maybe_inner) -> case Maybe_inner of
                    none ->
                        {ok, none};

                    {some, Dynamic_inner} ->
                        _pipe = Inner_type(Dynamic_inner),
                        _pipe@1 = gleam@result:map(
                            _pipe,
                            fun(Field@0) -> {some, Field@0} end
                        ),
                        map_errors(
                            _pipe@1,
                            fun(_capture) -> push_path(_capture, Name) end
                        )
                end end
        )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 470).
-spec element(
    integer(),
    fun((dynamic_()) -> {ok, CMG} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, CMG} | {error, list(decode_error())}).
element(Index, Inner_type) ->
    fun(Data) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple(Data),
            fun(Tuple) ->
                Size = gleam_stdlib:size_of_tuple(Tuple),
                gleam@result:'try'(case Index >= 0 of
                        true ->
                            case Index < Size of
                                true ->
                                    gleam_stdlib:tuple_get(Tuple, Index);

                                false ->
                                    at_least_decode_tuple_error(Index + 1, Data)
                            end;

                        false ->
                            case gleam@int:absolute_value(Index) =< Size of
                                true ->
                                    gleam_stdlib:tuple_get(Tuple, Size + Index);

                                false ->
                                    at_least_decode_tuple_error(
                                        gleam@int:absolute_value(Index),
                                        Data
                                    )
                            end
                    end, fun(Data@1) -> _pipe = Inner_type(Data@1),
                        map_errors(
                            _pipe,
                            fun(_capture) -> push_path(_capture, Index) end
                        ) end)
            end
        )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 553).
-spec tuple_errors({ok, any()} | {error, list(decode_error())}, binary()) -> list(decode_error()).
tuple_errors(Result, Name) ->
    case Result of
        {ok, _} ->
            [];

        {error, Errors} ->
            gleam@list:map(
                Errors,
                fun(_capture) -> push_path(_capture, Name) end
            )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 629).
-spec tuple2(
    fun((dynamic_()) -> {ok, CNG} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CNI} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {CNG, CNI}} | {error, list(decode_error())}).
tuple2(Decode1, Decode2) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple2(Value),
            fun(_use0) ->
                {A, B} = _use0,
                case {Decode1(A), Decode2(B)} of
                    {{ok, A@1}, {ok, B@1}} ->
                        {ok, {A@1, B@1}};

                    {A@2, B@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = lists:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        {error, _pipe@1}
                end
            end
        )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 698).
-spec tuple3(
    fun((dynamic_()) -> {ok, CNL} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CNN} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CNP} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {CNL, CNN, CNP}} | {error, list(decode_error())}).
tuple3(Decode1, Decode2, Decode3) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple3(Value),
            fun(_use0) ->
                {A, B, C} = _use0,
                case {Decode1(A), Decode2(B), Decode3(C)} of
                    {{ok, A@1}, {ok, B@1}, {ok, C@1}} ->
                        {ok, {A@1, B@1, C@1}};

                    {A@2, B@2, C@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = lists:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = lists:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        {error, _pipe@2}
                end
            end
        )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 769).
-spec tuple4(
    fun((dynamic_()) -> {ok, CNS} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CNU} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CNW} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CNY} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {CNS, CNU, CNW, CNY}} |
    {error, list(decode_error())}).
tuple4(Decode1, Decode2, Decode3, Decode4) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple4(Value),
            fun(_use0) ->
                {A, B, C, D} = _use0,
                case {Decode1(A), Decode2(B), Decode3(C), Decode4(D)} of
                    {{ok, A@1}, {ok, B@1}, {ok, C@1}, {ok, D@1}} ->
                        {ok, {A@1, B@1, C@1, D@1}};

                    {A@2, B@2, C@2, D@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = lists:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = lists:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        _pipe@3 = lists:append(
                            _pipe@2,
                            tuple_errors(D@2, <<"3"/utf8>>)
                        ),
                        {error, _pipe@3}
                end
            end
        )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 842).
-spec tuple5(
    fun((dynamic_()) -> {ok, COB} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, COD} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, COF} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, COH} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, COJ} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {COB, COD, COF, COH, COJ}} |
    {error, list(decode_error())}).
tuple5(Decode1, Decode2, Decode3, Decode4, Decode5) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple5(Value),
            fun(_use0) ->
                {A, B, C, D, E} = _use0,
                case {Decode1(A),
                    Decode2(B),
                    Decode3(C),
                    Decode4(D),
                    Decode5(E)} of
                    {{ok, A@1}, {ok, B@1}, {ok, C@1}, {ok, D@1}, {ok, E@1}} ->
                        {ok, {A@1, B@1, C@1, D@1, E@1}};

                    {A@2, B@2, C@2, D@2, E@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = lists:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = lists:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        _pipe@3 = lists:append(
                            _pipe@2,
                            tuple_errors(D@2, <<"3"/utf8>>)
                        ),
                        _pipe@4 = lists:append(
                            _pipe@3,
                            tuple_errors(E@2, <<"4"/utf8>>)
                        ),
                        {error, _pipe@4}
                end
            end
        )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 917).
-spec tuple6(
    fun((dynamic_()) -> {ok, COM} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, COO} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, COQ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, COS} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, COU} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, COW} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, {COM, COO, COQ, COS, COU, COW}} |
    {error, list(decode_error())}).
tuple6(Decode1, Decode2, Decode3, Decode4, Decode5, Decode6) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_tuple6(Value),
            fun(_use0) ->
                {A, B, C, D, E, F} = _use0,
                case {Decode1(A),
                    Decode2(B),
                    Decode3(C),
                    Decode4(D),
                    Decode5(E),
                    Decode6(F)} of
                    {{ok, A@1},
                        {ok, B@1},
                        {ok, C@1},
                        {ok, D@1},
                        {ok, E@1},
                        {ok, F@1}} ->
                        {ok, {A@1, B@1, C@1, D@1, E@1, F@1}};

                    {A@2, B@2, C@2, D@2, E@2, F@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = lists:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = lists:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        _pipe@3 = lists:append(
                            _pipe@2,
                            tuple_errors(D@2, <<"3"/utf8>>)
                        ),
                        _pipe@4 = lists:append(
                            _pipe@3,
                            tuple_errors(E@2, <<"4"/utf8>>)
                        ),
                        _pipe@5 = lists:append(
                            _pipe@4,
                            tuple_errors(F@2, <<"5"/utf8>>)
                        ),
                        {error, _pipe@5}
                end
            end
        )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 968).
-spec dict(
    fun((dynamic_()) -> {ok, COZ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CPB} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, gleam@dict:dict(COZ, CPB)} |
    {error, list(decode_error())}).
dict(Key_type, Value_type) ->
    fun(Value) ->
        gleam@result:'try'(
            gleam_stdlib:decode_map(Value),
            fun(Dict) ->
                gleam@result:'try'(
                    begin
                        _pipe = Dict,
                        _pipe@1 = maps:to_list(_pipe),
                        gleam@list:try_map(
                            _pipe@1,
                            fun(Pair) ->
                                {K, V} = Pair,
                                gleam@result:'try'(
                                    begin
                                        _pipe@2 = Key_type(K),
                                        map_errors(
                                            _pipe@2,
                                            fun(_capture) ->
                                                push_path(
                                                    _capture,
                                                    <<"keys"/utf8>>
                                                )
                                            end
                                        )
                                    end,
                                    fun(K@1) ->
                                        gleam@result:'try'(
                                            begin
                                                _pipe@3 = Value_type(V),
                                                map_errors(
                                                    _pipe@3,
                                                    fun(_capture@1) ->
                                                        push_path(
                                                            _capture@1,
                                                            <<"values"/utf8>>
                                                        )
                                                    end
                                                )
                                            end,
                                            fun(V@1) -> {ok, {K@1, V@1}} end
                                        )
                                    end
                                )
                            end
                        )
                    end,
                    fun(Pairs) -> {ok, maps:from_list(Pairs)} end
                )
            end
        )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 1082).
-spec decode2(
    fun((CPS, CPT) -> CPU),
    fun((dynamic_()) -> {ok, CPS} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CPT} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, CPU} | {error, list(decode_error())}).
decode2(Constructor, T1, T2) ->
    fun(Value) -> case {T1(Value), T2(Value)} of
            {{ok, A}, {ok, B}} ->
                {ok, Constructor(A, B)};

            {A@1, B@1} ->
                {error, gleam@list:flatten([all_errors(A@1), all_errors(B@1)])}
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 1114).
-spec decode3(
    fun((CPY, CPZ, CQA) -> CQB),
    fun((dynamic_()) -> {ok, CPY} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CPZ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CQA} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, CQB} | {error, list(decode_error())}).
decode3(Constructor, T1, T2, T3) ->
    fun(Value) -> case {T1(Value), T2(Value), T3(Value)} of
            {{ok, A}, {ok, B}, {ok, C}} ->
                {ok, Constructor(A, B, C)};

            {A@1, B@1, C@1} ->
                {error,
                    gleam@list:flatten(
                        [all_errors(A@1), all_errors(B@1), all_errors(C@1)]
                    )}
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 1160).
-spec decode4(
    fun((CQG, CQH, CQI, CQJ) -> CQK),
    fun((dynamic_()) -> {ok, CQG} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CQH} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CQI} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CQJ} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, CQK} | {error, list(decode_error())}).
decode4(Constructor, T1, T2, T3, T4) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}} ->
                {ok, Constructor(A, B, C, D)};

            {A@1, B@1, C@1, D@1} ->
                {error,
                    gleam@list:flatten(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1)]
                    )}
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 1216).
-spec decode5(
    fun((CQQ, CQR, CQS, CQT, CQU) -> CQV),
    fun((dynamic_()) -> {ok, CQQ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CQR} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CQS} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CQT} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CQU} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, CQV} | {error, list(decode_error())}).
decode5(Constructor, T1, T2, T3, T4, T5) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}, {ok, E}} ->
                {ok, Constructor(A, B, C, D, E)};

            {A@1, B@1, C@1, D@1, E@1} ->
                {error,
                    gleam@list:flatten(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1)]
                    )}
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 1276).
-spec decode6(
    fun((CRC, CRD, CRE, CRF, CRG, CRH) -> CRI),
    fun((dynamic_()) -> {ok, CRC} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CRD} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CRE} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CRF} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CRG} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CRH} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, CRI} | {error, list(decode_error())}).
decode6(Constructor, T1, T2, T3, T4, T5, T6) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}, {ok, E}, {ok, F}} ->
                {ok, Constructor(A, B, C, D, E, F)};

            {A@1, B@1, C@1, D@1, E@1, F@1} ->
                {error,
                    gleam@list:flatten(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1),
                            all_errors(F@1)]
                    )}
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 1341).
-spec decode7(
    fun((CRQ, CRR, CRS, CRT, CRU, CRV, CRW) -> CRX),
    fun((dynamic_()) -> {ok, CRQ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CRR} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CRS} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CRT} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CRU} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CRV} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CRW} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, CRX} | {error, list(decode_error())}).
decode7(Constructor, T1, T2, T3, T4, T5, T6, T7) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X), T7(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}, {ok, E}, {ok, F}, {ok, G}} ->
                {ok, Constructor(A, B, C, D, E, F, G)};

            {A@1, B@1, C@1, D@1, E@1, F@1, G@1} ->
                {error,
                    gleam@list:flatten(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1),
                            all_errors(F@1),
                            all_errors(G@1)]
                    )}
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 1410).
-spec decode8(
    fun((CSG, CSH, CSI, CSJ, CSK, CSL, CSM, CSN) -> CSO),
    fun((dynamic_()) -> {ok, CSG} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CSH} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CSI} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CSJ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CSK} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CSL} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CSM} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CSN} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, CSO} | {error, list(decode_error())}).
decode8(Constructor, T1, T2, T3, T4, T5, T6, T7, T8) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X), T7(X), T8(X)} of
            {{ok, A},
                {ok, B},
                {ok, C},
                {ok, D},
                {ok, E},
                {ok, F},
                {ok, G},
                {ok, H}} ->
                {ok, Constructor(A, B, C, D, E, F, G, H)};

            {A@1, B@1, C@1, D@1, E@1, F@1, G@1, H@1} ->
                {error,
                    gleam@list:flatten(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1),
                            all_errors(F@1),
                            all_errors(G@1),
                            all_errors(H@1)]
                    )}
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dynamic.gleam", 1483).
-spec decode9(
    fun((CSY, CSZ, CTA, CTB, CTC, CTD, CTE, CTF, CTG) -> CTH),
    fun((dynamic_()) -> {ok, CSY} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CSZ} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CTA} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CTB} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CTC} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CTD} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CTE} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CTF} | {error, list(decode_error())}),
    fun((dynamic_()) -> {ok, CTG} | {error, list(decode_error())})
) -> fun((dynamic_()) -> {ok, CTH} | {error, list(decode_error())}).
decode9(Constructor, T1, T2, T3, T4, T5, T6, T7, T8, T9) ->
    fun(X) ->
        case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X), T7(X), T8(X), T9(X)} of
            {{ok, A},
                {ok, B},
                {ok, C},
                {ok, D},
                {ok, E},
                {ok, F},
                {ok, G},
                {ok, H},
                {ok, I}} ->
                {ok, Constructor(A, B, C, D, E, F, G, H, I)};

            {A@1, B@1, C@1, D@1, E@1, F@1, G@1, H@1, I@1} ->
                {error,
                    gleam@list:flatten(
                        [all_errors(A@1),
                            all_errors(B@1),
                            all_errors(C@1),
                            all_errors(D@1),
                            all_errors(E@1),
                            all_errors(F@1),
                            all_errors(G@1),
                            all_errors(H@1),
                            all_errors(I@1)]
                    )}
        end
    end.
