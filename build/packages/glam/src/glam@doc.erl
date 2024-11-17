-module(glam@doc).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([append/2, break/2, concat/1, append_docs/2, flex_break/2, force_break/1, from_string/1, zero_width_string/1, group/1, join/2, concat_join/2, lines/1, nest/2, nest_docs/2, prepend/2, prepend_docs/2, to_string/2, debug/1]).
-export_type([document/0, mode/0]).

-opaque document() :: {line, integer()} |
    {concat, list(document())} |
    {text, binary(), integer()} |
    {nest, document(), integer()} |
    {force_break, document()} |
    {break, binary(), binary()} |
    {flex_break, binary(), binary()} |
    {group, document()}.

-type mode() :: broken | force_broken | unbroken.

-spec append(document(), document()) -> document().
append(First, Second) ->
    case First of
        {concat, Docs} ->
            {concat, lists:append(Docs, [Second])};

        _ ->
            {concat, [First, Second]}
    end.

-spec break(binary(), binary()) -> document().
break(Unbroken, Broken) ->
    {break, Unbroken, Broken}.

-spec concat(list(document())) -> document().
concat(Docs) ->
    {concat, Docs}.

-spec append_docs(document(), list(document())) -> document().
append_docs(First, Docs) ->
    append(First, concat(Docs)).

-spec flex_break(binary(), binary()) -> document().
flex_break(Unbroken, Broken) ->
    {flex_break, Unbroken, Broken}.

-spec force_break(document()) -> document().
force_break(Doc) ->
    {force_break, Doc}.

-spec from_string(binary()) -> document().
from_string(String) ->
    {text, String, gleam@string:length(String)}.

-spec zero_width_string(binary()) -> document().
zero_width_string(String) ->
    {text, String, 0}.

-spec group(document()) -> document().
group(Doc) ->
    {group, Doc}.

-spec join(list(document()), document()) -> document().
join(Docs, Separator) ->
    concat(gleam@list:intersperse(Docs, Separator)).

-spec concat_join(list(document()), list(document())) -> document().
concat_join(Docs, Separators) ->
    join(Docs, concat(Separators)).

-spec lines(integer()) -> document().
lines(Size) ->
    {line, Size}.

-spec nest(document(), integer()) -> document().
nest(Doc, Indentation) ->
    {nest, Doc, Indentation}.

-spec nest_docs(list(document()), integer()) -> document().
nest_docs(Docs, Indentation) ->
    {nest, concat(Docs), Indentation}.

-spec prepend(document(), document()) -> document().
prepend(First, Second) ->
    case First of
        {concat, Docs} ->
            {concat, [Second | Docs]};

        _ ->
            {concat, [Second, First]}
    end.

-spec prepend_docs(document(), list(document())) -> document().
prepend_docs(First, Docs) ->
    prepend(First, concat(Docs)).

-spec fits(list({integer(), mode(), document()}), integer(), integer()) -> boolean().
fits(Docs, Max_width, Current_width) ->
    case Docs of
        _ when Current_width > Max_width ->
            false;

        [] ->
            true;

        [{Indent, Mode, Doc} | Rest] ->
            case Doc of
                {line, _} ->
                    true;

                {force_break, _} ->
                    false;

                {text, _, Length} ->
                    fits(Rest, Max_width, Current_width + Length);

                {nest, Doc@1, I} ->
                    _pipe = [{Indent + I, Mode, Doc@1} | Rest],
                    fits(_pipe, Max_width, Current_width);

                {break, Unbroken, _} ->
                    case Mode of
                        broken ->
                            true;

                        force_broken ->
                            true;

                        unbroken ->
                            fits(
                                Rest,
                                Max_width,
                                Current_width + gleam@string:length(Unbroken)
                            )
                    end;

                {flex_break, Unbroken, _} ->
                    case Mode of
                        broken ->
                            true;

                        force_broken ->
                            true;

                        unbroken ->
                            fits(
                                Rest,
                                Max_width,
                                Current_width + gleam@string:length(Unbroken)
                            )
                    end;

                {group, Doc@2} ->
                    fits(
                        [{Indent, Mode, Doc@2} | Rest],
                        Max_width,
                        Current_width
                    );

                {concat, Docs@1} ->
                    _pipe@1 = gleam@list:map(
                        Docs@1,
                        fun(Doc@3) -> {Indent, Mode, Doc@3} end
                    ),
                    _pipe@2 = lists:append(_pipe@1, Rest),
                    fits(_pipe@2, Max_width, Current_width)
            end
    end.

-spec do_flatten(list(document()), list(document())) -> list(document()).
do_flatten(Docs, Acc) ->
    case Docs of
        [] ->
            lists:reverse(Acc);

        [One] ->
            lists:reverse([One | Acc]);

        [{concat, One@1}, {concat, Two} | Rest] ->
            do_flatten([{concat, lists:append(One@1, Two)} | Rest], Acc);

        [{text, One@2, Len_one}, {text, Two@1, Len_two} | Rest@1] ->
            do_flatten(
                [{text, <<One@2/binary, Two@1/binary>>, Len_one + Len_two} |
                    Rest@1],
                Acc
            );

        [One@3, Two@2 | Rest@2] ->
            do_flatten([Two@2 | Rest@2], [One@3 | Acc])
    end.

-spec flatten(list(document())) -> list(document()).
flatten(Docs) ->
    do_flatten(Docs, []).

-spec do_split_groups(
    list(document()),
    list(document()),
    list(list(document()))
) -> list(list(document())).
do_split_groups(Docs, Current_group, Acc) ->
    case Docs of
        [] ->
            case Current_group of
                [] ->
                    lists:reverse(Acc);

                _ ->
                    lists:reverse([lists:reverse(Current_group) | Acc])
            end;

        [{group, _} = Doc | Rest] ->
            case Current_group of
                [] ->
                    do_split_groups(Rest, [], [[Doc] | Acc]);

                _ ->
                    do_split_groups(
                        Rest,
                        [],
                        [[Doc], lists:reverse(Current_group) | Acc]
                    )
            end;

        [Doc@1 | Rest@1] ->
            do_split_groups(Rest@1, [Doc@1 | Current_group], Acc)
    end.

-spec split_groups(list(document())) -> list(list(document())).
split_groups(Docs) ->
    do_split_groups(Docs, [], []).

-spec superscript_number(integer()) -> binary().
superscript_number(Number) ->
    _assert_subject = gleam@int:digits(Number, 10),
    {ok, Digits} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"glam/doc"/utf8>>,
                        function => <<"superscript_number"/utf8>>,
                        line => 752})
    end,
    gleam@list:fold(
        Digits,
        <<""/utf8>>,
        fun(Acc, Digit) ->
            Digit@1 = case Digit of
                0 ->
                    <<"⁰"/utf8>>;

                1 ->
                    <<"¹"/utf8>>;

                2 ->
                    <<"²"/utf8>>;

                3 ->
                    <<"³"/utf8>>;

                4 ->
                    <<"⁴"/utf8>>;

                5 ->
                    <<"⁵"/utf8>>;

                6 ->
                    <<"⁶"/utf8>>;

                7 ->
                    <<"⁷"/utf8>>;

                8 ->
                    <<"⁸"/utf8>>;

                9 ->
                    <<"⁹"/utf8>>;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"not a digit"/utf8>>,
                            module => <<"glam/doc"/utf8>>,
                            function => <<"superscript_number"/utf8>>,
                            line => 765})
            end,
            <<Acc/binary, Digit@1/binary>>
        end
    ).

-spec indentation(integer()) -> binary().
indentation(Size) ->
    gleam@string:repeat(<<" "/utf8>>, Size).

-spec do_to_string(
    binary(),
    integer(),
    integer(),
    list({integer(), mode(), document()})
) -> binary().
do_to_string(Acc, Max_width, Current_width, Docs) ->
    case Docs of
        [] ->
            Acc;

        [{Indent, Mode, Doc} | Rest] ->
            case Doc of
                {line, Size} ->
                    _pipe = (<<<<Acc/binary,
                            (gleam@string:repeat(<<"\n"/utf8>>, Size))/binary>>/binary,
                        (indentation(Indent))/binary>>),
                    do_to_string(_pipe, Max_width, Indent, Rest);

                {flex_break, Unbroken, Broken} ->
                    New_unbroken_width = Current_width + gleam@string:length(
                        Unbroken
                    ),
                    case fits(Rest, Max_width, New_unbroken_width) of
                        true ->
                            _pipe@1 = (<<Acc/binary, Unbroken/binary>>),
                            do_to_string(
                                _pipe@1,
                                Max_width,
                                New_unbroken_width,
                                Rest
                            );

                        false ->
                            _pipe@2 = (<<<<<<Acc/binary, Broken/binary>>/binary,
                                    "\n"/utf8>>/binary,
                                (indentation(Indent))/binary>>),
                            do_to_string(_pipe@2, Max_width, Indent, Rest)
                    end;

                {break, Unbroken@1, Broken@1} ->
                    case Mode of
                        unbroken ->
                            New_width = Current_width + gleam@string:length(
                                Unbroken@1
                            ),
                            do_to_string(
                                <<Acc/binary, Unbroken@1/binary>>,
                                Max_width,
                                New_width,
                                Rest
                            );

                        broken ->
                            _pipe@3 = (<<<<<<Acc/binary, Broken@1/binary>>/binary,
                                    "\n"/utf8>>/binary,
                                (indentation(Indent))/binary>>),
                            do_to_string(_pipe@3, Max_width, Indent, Rest);

                        force_broken ->
                            _pipe@3 = (<<<<<<Acc/binary, Broken@1/binary>>/binary,
                                    "\n"/utf8>>/binary,
                                (indentation(Indent))/binary>>),
                            do_to_string(_pipe@3, Max_width, Indent, Rest)
                    end;

                {force_break, Doc@1} ->
                    Docs@1 = [{Indent, force_broken, Doc@1} | Rest],
                    do_to_string(Acc, Max_width, Current_width, Docs@1);

                {concat, Docs@2} ->
                    Docs@3 = begin
                        _pipe@4 = gleam@list:map(
                            Docs@2,
                            fun(Doc@2) -> {Indent, Mode, Doc@2} end
                        ),
                        lists:append(_pipe@4, Rest)
                    end,
                    do_to_string(Acc, Max_width, Current_width, Docs@3);

                {group, Doc@3} ->
                    Fits = fits(
                        [{Indent, unbroken, Doc@3}],
                        Max_width,
                        Current_width
                    ),
                    New_mode = case Fits of
                        true ->
                            unbroken;

                        false ->
                            broken
                    end,
                    Docs@4 = [{Indent, New_mode, Doc@3} | Rest],
                    do_to_string(Acc, Max_width, Current_width, Docs@4);

                {nest, Doc@4, I} ->
                    Docs@5 = [{Indent + I, Mode, Doc@4} | Rest],
                    do_to_string(Acc, Max_width, Current_width, Docs@5);

                {text, Text, Length} ->
                    do_to_string(
                        <<Acc/binary, Text/binary>>,
                        Max_width,
                        Current_width + Length,
                        Rest
                    )
            end
    end.

-spec to_string(document(), integer()) -> binary().
to_string(Doc, Limit) ->
    do_to_string(<<""/utf8>>, Limit, 0, [{0, unbroken, Doc}]).

-spec parenthesise(document(), binary(), binary()) -> document().
parenthesise(Document, Open, Close) ->
    _pipe = [from_string(Open),
        nest({line, 1}, 2),
        nest(Document, 2),
        {line, 1},
        from_string(Close)],
    _pipe@1 = concat(_pipe),
    group(_pipe@1).

-spec debug(document()) -> document().
debug(Document) ->
    case Document of
        {text, Text, _} ->
            Escaped = gleam@string:replace(Text, <<"\""/utf8>>, <<"\\\""/utf8>>),
            from_string(<<<<"\""/utf8, Escaped/binary>>/binary, "\""/utf8>>);

        {force_break, Doc} ->
            parenthesise(debug(Doc), <<"force("/utf8>>, <<")"/utf8>>);

        {group, Doc@1} ->
            _pipe = parenthesise(debug(Doc@1), <<"["/utf8>>, <<"]"/utf8>>),
            force_break(_pipe);

        {nest, Doc@2, Indentation} ->
            _pipe@1 = parenthesise(
                debug(Doc@2),
                <<(superscript_number(Indentation))/binary, "⟨"/utf8>>,
                <<"⟩"/utf8>>
            ),
            force_break(_pipe@1);

        {break, <<" "/utf8>>, <<""/utf8>>} ->
            from_string(<<"space"/utf8>>);

        {break, Unbroken, Broken} ->
            from_string(
                <<<<<<<<"{ \""/utf8, Unbroken/binary>>/binary, "\", \""/utf8>>/binary,
                        Broken/binary>>/binary,
                    "\" }"/utf8>>
            );

        {flex_break, <<" "/utf8>>, <<""/utf8>>} ->
            from_string(<<"flex_space"/utf8>>);

        {flex_break, Unbroken@1, Broken@1} ->
            from_string(
                <<<<<<<<"flex{ \""/utf8, Unbroken@1/binary>>/binary,
                            "\", \""/utf8>>/binary,
                        Broken@1/binary>>/binary,
                    "\" }"/utf8>>
            );

        {line, Size} ->
            case gleam@int:compare(Size, 1) of
                lt ->
                    from_string(<<"lf"/utf8>>);

                eq ->
                    from_string(<<"lf"/utf8>>);

                gt ->
                    from_string(
                        <<"lf"/utf8, (superscript_number(Size))/binary>>
                    )
            end;

        {concat, Docs} ->
            _pipe@2 = split_groups(flatten(Docs)),
            _pipe@5 = gleam@list:map(
                _pipe@2,
                fun(Docs@1) ->
                    case Docs@1 of
                        [] ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"empty"/utf8>>,
                                    module => <<"glam/doc"/utf8>>,
                                    function => <<"debug"/utf8>>,
                                    line => 686});

                        _ ->
                            nil
                    end,
                    _pipe@3 = gleam@list:map(Docs@1, fun debug/1),
                    _pipe@4 = join(
                        _pipe@3,
                        flex_break(<<" . "/utf8>>, <<" ."/utf8>>)
                    ),
                    group(_pipe@4)
                end
            ),
            join(
                _pipe@5,
                concat([flex_break(<<" . "/utf8>>, <<" ."/utf8>>), {line, 1}])
            )
    end.
