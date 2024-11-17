-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map2/3, index_map/2, try_map/2, drop/2, take/2, new/0, wrap/1, append/2, prepend/2, concat/1, flatten/1, flat_map/2, fold/3, count/2, group/2, map_fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, key_filter/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1]).
-export_type([continue_or_stop/1, sorting/0]).

-type continue_or_stop(AAN) :: {continue, AAN} | {stop, AAN}.

-type sorting() :: ascending | descending.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 61).
-spec length_loop(list(any()), integer()) -> integer().
length_loop(List, Count) ->
    case List of
        [_ | List@1] ->
            length_loop(List@1, Count + 1);

        _ ->
            Count
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 57).
-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 130).
-spec reverse_loop(list(AAX), list(AAX)) -> list(AAX).
reverse_loop(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            reverse_loop(Rest, [Item | Accumulator])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 126).
-spec reverse(list(AAU)) -> list(AAU).
reverse(List) ->
    lists:reverse(List).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 158).
-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 194).
-spec contains(list(ABD), ABD) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [First | _] when First =:= Elem ->
            true;

        [_ | Rest] ->
            contains(Rest, Elem)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 221).
-spec first(list(ABF)) -> {ok, ABF} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 250).
-spec rest(list(ABJ)) -> {ok, list(ABJ)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Rest] ->
            {ok, Rest}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 293).
-spec update_group(fun((ABU) -> ABV)) -> fun((gleam@dict:dict(ABV, list(ABU)), ABU) -> gleam@dict:dict(ABV, list(ABU))).
update_group(F) ->
    fun(Groups, Elem) -> case gleam@dict:get(Groups, F(Elem)) of
            {ok, Existing} ->
                gleam@dict:insert(Groups, F(Elem), [Elem | Existing]);

            {error, _} ->
                gleam@dict:insert(Groups, F(Elem), [Elem])
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 321).
-spec filter_loop(list(ACF), fun((ACF) -> boolean()), list(ACF)) -> list(ACF).
filter_loop(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            New_acc = case Fun(First) of
                true ->
                    [First | Acc];

                false ->
                    Acc
            end,
            filter_loop(Rest, Fun, New_acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 317).
-spec filter(list(ACC), fun((ACC) -> boolean())) -> list(ACC).
filter(List, Predicate) ->
    filter_loop(List, Predicate, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 353).
-spec filter_map_loop(
    list(ACQ),
    fun((ACQ) -> {ok, ACS} | {error, any()}),
    list(ACS)
) -> list(ACS).
filter_map_loop(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            New_acc = case Fun(First) of
                {ok, First@1} ->
                    [First@1 | Acc];

                {error, _} ->
                    Acc
            end,
            filter_map_loop(Rest, Fun, New_acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 349).
-spec filter_map(list(ACJ), fun((ACJ) -> {ok, ACL} | {error, any()})) -> list(ACL).
filter_map(List, Fun) ->
    filter_map_loop(List, Fun, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 384).
-spec map_loop(list(ADC), fun((ADC) -> ADE), list(ADE)) -> list(ADE).
map_loop(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            map_loop(Rest, Fun, [Fun(First) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 380).
-spec map(list(ACY), fun((ACY) -> ADA)) -> list(ADA).
map(List, Fun) ->
    map_loop(List, Fun, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 411).
-spec map2_loop(list(ADN), list(ADP), fun((ADN, ADP) -> ADR), list(ADR)) -> list(ADR).
map2_loop(List1, List2, Fun, Acc) ->
    case {List1, List2} of
        {[], _} ->
            lists:reverse(Acc);

        {_, []} ->
            lists:reverse(Acc);

        {[A | As_], [B | Bs]} ->
            map2_loop(As_, Bs, Fun, [Fun(A, B) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 407).
-spec map2(list(ADH), list(ADJ), fun((ADH, ADJ) -> ADL)) -> list(ADL).
map2(List1, List2, Fun) ->
    map2_loop(List1, List2, Fun, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 466).
-spec index_map_loop(
    list(AED),
    fun((AED, integer()) -> AEF),
    integer(),
    list(AEF)
) -> list(AEF).
index_map_loop(List, Fun, Index, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            Acc@1 = [Fun(First, Index) | Acc],
            index_map_loop(Rest, Fun, Index + 1, Acc@1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 462).
-spec index_map(list(ADZ), fun((ADZ, integer()) -> AEB)) -> list(AEB).
index_map(List, Fun) ->
    index_map_loop(List, Fun, 0, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 520).
-spec try_map_loop(list(AER), fun((AER) -> {ok, AET} | {error, AEU}), list(AET)) -> {ok,
        list(AET)} |
    {error, AEU}.
try_map_loop(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, lists:reverse(Acc)};

        [First | Rest] ->
            case Fun(First) of
                {ok, First@1} ->
                    try_map_loop(Rest, Fun, [First@1 | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 513).
-spec try_map(list(AEI), fun((AEI) -> {ok, AEK} | {error, AEL})) -> {ok,
        list(AEK)} |
    {error, AEL}.
try_map(List, Fun) ->
    try_map_loop(List, Fun, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 555).
-spec drop(list(AFB), integer()) -> list(AFB).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Rest] ->
                    drop(Rest, N - 1)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 590).
-spec take_loop(list(AFH), integer(), list(AFH)) -> list(AFH).
take_loop(List, N, Acc) ->
    case N =< 0 of
        true ->
            lists:reverse(Acc);

        false ->
            case List of
                [] ->
                    lists:reverse(Acc);

                [First | Rest] ->
                    take_loop(Rest, N - 1, [First | Acc])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 586).
-spec take(list(AFE), integer()) -> list(AFE).
take(List, N) ->
    take_loop(List, N, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 610).
-spec new() -> list(any()).
new() ->
    [].

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 630).
-spec wrap(AFN) -> list(AFN).
wrap(Item) ->
    [Item].

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 651).
-spec append_loop(list(AFT), list(AFT)) -> list(AFT).
append_loop(First, Second) ->
    case First of
        [] ->
            Second;

        [Item | Rest] ->
            append_loop(Rest, [Item | Second])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 647).
-spec append(list(AFP), list(AFP)) -> list(AFP).
append(First, Second) ->
    lists:append(First, Second).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 671).
-spec prepend(list(AFX), AFX) -> list(AFX).
prepend(List, Item) ->
    [Item | List].

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 676).
-spec reverse_and_prepend(list(AGA), list(AGA)) -> list(AGA).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [First | Rest] ->
            reverse_and_prepend(Rest, [First | Suffix])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 699).
-spec concat_loop(list(list(AGI)), list(AGI)) -> list(AGI).
concat_loop(Lists, Acc) ->
    case Lists of
        [] ->
            lists:reverse(Acc);

        [List | Further_lists] ->
            concat_loop(Further_lists, reverse_and_prepend(List, Acc))
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 695).
-spec concat(list(list(AGE))) -> list(AGE).
concat(Lists) ->
    concat_loop(Lists, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 719).
-spec flatten(list(list(AGN))) -> list(AGN).
flatten(Lists) ->
    concat_loop(Lists, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 732).
-spec flat_map(list(AGR), fun((AGR) -> list(AGT))) -> list(AGT).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    flatten(_pipe).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 745).
-spec fold(list(AGW), AGY, fun((AGY, AGW) -> AGY)) -> AGY.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 90).
-spec count(list(AAS), fun((AAS) -> boolean())) -> integer().
count(List, Predicate) ->
    fold(List, 0, fun(Acc, Value) -> case Predicate(Value) of
                true ->
                    Acc + 1;

                false ->
                    Acc
            end end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 289).
-spec group(list(ABO), fun((ABO) -> ABQ)) -> gleam@dict:dict(ABQ, list(ABO)).
group(List, Key) ->
    fold(List, gleam@dict:new(), update_group(Key)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 436).
-spec map_fold(list(ADU), ADW, fun((ADW, ADU) -> {ADW, ADX})) -> {ADW,
    list(ADX)}.
map_fold(List, Initial, Fun) ->
    _pipe = fold(
        List,
        {Initial, []},
        fun(Acc, Item) ->
            {Current_acc, Items} = Acc,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun lists:reverse/1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 767).
-spec fold_right(list(AGZ), AHB, fun((AHB, AGZ) -> AHB)) -> AHB.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 795).
-spec index_fold_loop(
    list(AHF),
    AHH,
    fun((AHH, AHF, integer()) -> AHH),
    integer()
) -> AHH.
index_fold_loop(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            index_fold_loop(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 787).
-spec index_fold(list(AHC), AHE, fun((AHE, AHC, integer()) -> AHE)) -> AHE.
index_fold(List, Initial, Fun) ->
    index_fold_loop(List, Initial, Fun, 0).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 827).
-spec try_fold(list(AHI), AHK, fun((AHK, AHI) -> {ok, AHK} | {error, AHL})) -> {ok,
        AHK} |
    {error, AHL}.
try_fold(List, Initial, Fun) ->
    case List of
        [] ->
            {ok, Initial};

        [First | Rest] ->
            case Fun(Initial, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 866).
-spec fold_until(list(AHQ), AHS, fun((AHS, AHQ) -> continue_or_stop(AHS))) -> AHS.
fold_until(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [First | Rest] ->
            case Fun(Initial, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 903).
-spec find(list(AHU), fun((AHU) -> boolean())) -> {ok, AHU} | {error, nil}.
find(List, Is_desired) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, Is_desired)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 939).
-spec find_map(list(AHY), fun((AHY) -> {ok, AIA} | {error, any()})) -> {ok, AIA} |
    {error, nil}.
find_map(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 974).
-spec all(list(AIG), fun((AIG) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    all(Rest, Predicate);

                false ->
                    false
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1011).
-spec any(list(AII), fun((AII) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    true;

                false ->
                    any(Rest, Predicate)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1053).
-spec zip_loop(list(AIP), list(AIR), list({AIP, AIR})) -> list({AIP, AIR}).
zip_loop(One, Other, Acc) ->
    case {One, Other} of
        {[First_one | Rest_one], [First_other | Rest_other]} ->
            zip_loop(Rest_one, Rest_other, [{First_one, First_other} | Acc]);

        {_, _} ->
            lists:reverse(Acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1049).
-spec zip(list(AIK), list(AIM)) -> list({AIK, AIM}).
zip(List, Other) ->
    zip_loop(List, Other, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1087).
-spec strict_zip(list(AIV), list(AIX)) -> {ok, list({AIV, AIX})} | {error, nil}.
strict_zip(List, Other) ->
    case erlang:length(List) =:= erlang:length(Other) of
        true ->
            {ok, zip(List, Other)};

        false ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1115).
-spec unzip_loop(list({AJH, AJI}), list(AJH), list(AJI)) -> {list(AJH),
    list(AJI)}.
unzip_loop(Input, One, Other) ->
    case Input of
        [] ->
            {lists:reverse(One), lists:reverse(Other)};

        [{First_one, First_other} | Rest] ->
            unzip_loop(Rest, [First_one | One], [First_other | Other])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1111).
-spec unzip(list({AJC, AJD})) -> {list(AJC), list(AJD)}.
unzip(Input) ->
    unzip_loop(Input, [], []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1150).
-spec intersperse_loop(list(AJR), AJR, list(AJR)) -> list(AJR).
intersperse_loop(List, Separator, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Rest] ->
            intersperse_loop(Rest, Separator, [X, Separator | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1143).
-spec intersperse(list(AJO), AJO) -> list(AJO).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            intersperse_loop(Rest, Elem, [X])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1168).
-spec unique(list(AJV)) -> list(AJV).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1249).
-spec sequences(
    list(AKB),
    fun((AKB, AKB) -> gleam@order:order()),
    list(AKB),
    sorting(),
    AKB,
    list(list(AKB))
) -> list(list(AKB)).
sequences(List, Compare, Growing, Direction, Prev, Acc) ->
    Growing@1 = [Prev | Growing],
    case List of
        [] ->
            case Direction of
                ascending ->
                    [reverse_loop(Growing@1, []) | Acc];

                descending ->
                    [Growing@1 | Acc]
            end;

        [New | Rest] ->
            case {Compare(Prev, New), Direction} of
                {gt, descending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {lt, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {eq, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {gt, ascending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [reverse_loop(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {lt, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [reverse_loop(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {eq, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [reverse_loop(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1397).
-spec merge_ascendings(
    list(AKY),
    list(AKY),
    fun((AKY, AKY) -> gleam@order:order()),
    list(AKY)
) -> list(AKY).
merge_ascendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            reverse_loop(List, Acc);

        {List, []} ->
            reverse_loop(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_ascendings(Rest1, List2, Compare, [First1 | Acc]);

                gt ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc]);

                eq ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1350).
-spec merge_ascending_pairs(
    list(list(AKM)),
    fun((AKM, AKM) -> gleam@order:order()),
    list(list(AKM))
) -> list(list(AKM)).
merge_ascending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            reverse_loop(Acc, []);

        [Sequence] ->
            reverse_loop([reverse_loop(Sequence, []) | Acc], []);

        [Ascending1, Ascending2 | Rest] ->
            Descending = merge_ascendings(Ascending1, Ascending2, Compare, []),
            merge_ascending_pairs(Rest, Compare, [Descending | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1424).
-spec merge_descendings(
    list(ALD),
    list(ALD),
    fun((ALD, ALD) -> gleam@order:order()),
    list(ALD)
) -> list(ALD).
merge_descendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            reverse_loop(List, Acc);

        {List, []} ->
            reverse_loop(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_descendings(List1, Rest2, Compare, [First2 | Acc]);

                gt ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc]);

                eq ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1372).
-spec merge_descending_pairs(
    list(list(AKS)),
    fun((AKS, AKS) -> gleam@order:order()),
    list(list(AKS))
) -> list(list(AKS)).
merge_descending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            reverse_loop(Acc, []);

        [Sequence] ->
            reverse_loop([reverse_loop(Sequence, []) | Acc], []);

        [Descending1, Descending2 | Rest] ->
            Ascending = merge_descendings(Descending1, Descending2, Compare, []),
            merge_descending_pairs(Rest, Compare, [Ascending | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1316).
-spec merge_all(
    list(list(AKI)),
    sorting(),
    fun((AKI, AKI) -> gleam@order:order())
) -> list(AKI).
merge_all(Sequences, Direction, Compare) ->
    case {Sequences, Direction} of
        {[], _} ->
            [];

        {[Sequence], ascending} ->
            Sequence;

        {[Sequence@1], descending} ->
            reverse_loop(Sequence@1, []);

        {_, ascending} ->
            Sequences@1 = merge_ascending_pairs(Sequences, Compare, []),
            merge_all(Sequences@1, descending, Compare);

        {_, descending} ->
            Sequences@2 = merge_descending_pairs(Sequences, Compare, []),
            merge_all(Sequences@2, ascending, Compare)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1187).
-spec sort(list(AJY), fun((AJY, AJY) -> gleam@order:order())) -> list(AJY).
sort(List, Compare) ->
    case List of
        [] ->
            [];

        [X] ->
            [X];

        [X@1, Y | Rest] ->
            Direction = case Compare(X@1, Y) of
                lt ->
                    ascending;

                eq ->
                    ascending;

                gt ->
                    descending
            end,
            Sequences = sequences(Rest, Compare, [X@1], Direction, Y, []),
            merge_all(Sequences, ascending, Compare)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1464).
-spec range_loop(integer(), integer(), list(integer())) -> list(integer()).
range_loop(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [Stop | Acc];

        gt ->
            range_loop(Start, Stop + 1, [Stop | Acc]);

        lt ->
            range_loop(Start, Stop - 1, [Stop | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1460).
-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    range_loop(Start, Stop, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1490).
-spec repeat_loop(ALN, integer(), list(ALN)) -> list(ALN).
repeat_loop(Item, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            repeat_loop(Item, Times - 1, [Item | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1486).
-spec repeat(ALL, integer()) -> list(ALL).
repeat(A, Times) ->
    repeat_loop(A, Times, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1523).
-spec split_loop(list(ALU), integer(), list(ALU)) -> {list(ALU), list(ALU)}.
split_loop(List, N, Taken) ->
    case N =< 0 of
        true ->
            {lists:reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {lists:reverse(Taken), []};

                [First | Rest] ->
                    split_loop(Rest, N - 1, [First | Taken])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1519).
-spec split(list(ALQ), integer()) -> {list(ALQ), list(ALQ)}.
split(List, Index) ->
    split_loop(List, Index, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1559).
-spec split_while_loop(list(AMD), fun((AMD) -> boolean()), list(AMD)) -> {list(AMD),
    list(AMD)}.
split_while_loop(List, F, Acc) ->
    case List of
        [] ->
            {lists:reverse(Acc), []};

        [First | Rest] ->
            case F(First) of
                false ->
                    {lists:reverse(Acc), List};

                _ ->
                    split_while_loop(Rest, F, [First | Acc])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1552).
-spec split_while(list(ALZ), fun((ALZ) -> boolean())) -> {list(ALZ), list(ALZ)}.
split_while(List, Predicate) ->
    split_while_loop(List, Predicate, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1599).
-spec key_find(list({AMI, AMJ}), AMI) -> {ok, AMJ} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1630).
-spec key_filter(list({AMN, AMO}), AMN) -> list(AMO).
key_filter(Keyword_list, Desired_key) ->
    filter_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1671).
-spec pop_loop(list(BEW), fun((BEW) -> boolean()), list(BEW)) -> {ok,
        {BEW, list(BEW)}} |
    {error, nil}.
pop_loop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, lists:append(lists:reverse(Checked), Rest)}};

                false ->
                    pop_loop(Rest, Predicate, [X | Checked])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1664).
-spec pop(list(AMR), fun((AMR) -> boolean())) -> {ok, {AMR, list(AMR)}} |
    {error, nil}.
pop(List, Is_desired) ->
    pop_loop(List, Is_desired, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1711).
-spec pop_map_loop(
    list(ANJ),
    fun((ANJ) -> {ok, ANL} | {error, any()}),
    list(ANJ)
) -> {ok, {ANL, list(ANJ)}} | {error, nil}.
pop_map_loop(List, Mapper, Checked) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, lists:append(lists:reverse(Checked), Rest)}};

                {error, _} ->
                    pop_map_loop(Rest, Mapper, [X | Checked])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1704).
-spec pop_map(list(ANA), fun((ANA) -> {ok, ANC} | {error, any()})) -> {ok,
        {ANC, list(ANA)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    pop_map_loop(Haystack, Is_desired, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1749).
-spec key_pop(list({ANT, ANU}), ANT) -> {ok, {ANU, list({ANT, ANU})}} |
    {error, nil}.
key_pop(List, Key) ->
    pop_map(
        List,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1776).
-spec key_set(list({ANZ, AOA}), ANZ, AOA) -> list({ANZ, AOA}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1798).
-spec each(list(AOD), fun((AOD) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [First | Rest] ->
            F(First),
            each(Rest, F)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1824).
-spec try_each(list(AOG), fun((AOG) -> {ok, any()} | {error, AOJ})) -> {ok, nil} |
    {error, AOJ}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [First | Rest] ->
            case Fun(First) of
                {ok, _} ->
                    try_each(Rest, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1857).
-spec partition_loop(list(BHB), fun((BHB) -> boolean()), list(BHB), list(BHB)) -> {list(BHB),
    list(BHB)}.
partition_loop(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {lists:reverse(Trues), lists:reverse(Falses)};

        [First | Rest] ->
            case Categorise(First) of
                true ->
                    partition_loop(Rest, Categorise, [First | Trues], Falses);

                false ->
                    partition_loop(Rest, Categorise, Trues, [First | Falses])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1850).
-spec partition(list(AOO), fun((AOO) -> boolean())) -> {list(AOO), list(AOO)}.
partition(List, Categorise) ->
    partition_loop(List, Categorise, [], []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1877).
-spec permutations(list(AOX)) -> list(list(AOX)).
permutations(List) ->
    case List of
        [] ->
            [[]];

        _ ->
            _pipe@3 = index_map(
                List,
                fun(I, I_idx) ->
                    _pipe = index_fold(
                        List,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@1 = lists:reverse(_pipe),
                    _pipe@2 = permutations(_pipe@1),
                    map(_pipe@2, fun(Permutation) -> [I | Permutation] end)
                end
            ),
            flatten(_pipe@3)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1917).
-spec window_loop(list(list(APF)), list(APF), integer()) -> list(list(APF)).
window_loop(Acc, List, N) ->
    Window = take(List, N),
    case erlang:length(Window) =:= N of
        true ->
            window_loop([Window | Acc], drop(List, 1), N);

        false ->
            lists:reverse(Acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1910).
-spec window(list(APB), integer()) -> list(list(APB)).
window(List, N) ->
    case N =< 0 of
        true ->
            [];

        false ->
            window_loop([], List, N)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1940).
-spec window_by_2(list(APL)) -> list({APL, APL}).
window_by_2(List) ->
    zip(List, drop(List, 1)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1953).
-spec drop_while(list(APO), fun((APO) -> boolean())) -> list(APO).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    drop_while(Rest, Predicate);

                false ->
                    [First | Rest]
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1983).
-spec take_while_loop(list(APU), fun((APU) -> boolean()), list(APU)) -> list(APU).
take_while_loop(List, Predicate, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    take_while_loop(Rest, Predicate, [First | Acc]);

                false ->
                    lists:reverse(Acc)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1976).
-spec take_while(list(APR), fun((APR) -> boolean())) -> list(APR).
take_while(List, Predicate) ->
    take_while_loop(List, Predicate, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2015).
-spec chunk_loop(list(AQD), fun((AQD) -> AQF), AQF, list(AQD), list(list(AQD))) -> list(list(AQD)).
chunk_loop(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [First | Rest] ->
            Key = F(First),
            case Key =:= Previous_key of
                false ->
                    New_acc = [lists:reverse(Current_chunk) | Acc],
                    chunk_loop(Rest, F, Key, [First], New_acc);

                _ ->
                    chunk_loop(Rest, F, Key, [First | Current_chunk], Acc)
            end;

        _ ->
            lists:reverse([lists:reverse(Current_chunk) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2008).
-spec chunk(list(APY), fun((APY) -> any())) -> list(list(APY)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            chunk_loop(Rest, F, F(First), [First], [])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2060).
-spec sized_chunk_loop(
    list(AQP),
    integer(),
    integer(),
    list(AQP),
    list(list(AQP))
) -> list(list(AQP)).
sized_chunk_loop(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    lists:reverse(Acc);

                Remaining ->
                    lists:reverse([lists:reverse(Remaining) | Acc])
            end;

        [First | Rest] ->
            Chunk = [First | Current_chunk],
            case Left > 1 of
                true ->
                    sized_chunk_loop(Rest, Count, Left - 1, Chunk, Acc);

                false ->
                    sized_chunk_loop(
                        Rest,
                        Count,
                        Count,
                        [],
                        [lists:reverse(Chunk) | Acc]
                    )
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2056).
-spec sized_chunk(list(AQL), integer()) -> list(list(AQL)).
sized_chunk(List, Count) ->
    sized_chunk_loop(List, Count, Count, [], []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2104).
-spec reduce(list(AQW), fun((AQW, AQW) -> AQW)) -> {ok, AQW} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, fold(Rest, First, Fun)}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2128).
-spec scan_loop(list(ARE), ARG, list(ARG), fun((ARG, ARE) -> ARG)) -> list(ARG).
scan_loop(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            lists:reverse(Accumulated);

        [First | Rest] ->
            Next = Fun(Accumulator, First),
            scan_loop(Rest, Next, [Next | Accumulated], Fun)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2120).
-spec scan(list(ARA), ARC, fun((ARC, ARA) -> ARC)) -> list(ARC).
scan(List, Initial, Fun) ->
    scan_loop(List, Initial, [], Fun).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2163).
-spec last(list(ARJ)) -> {ok, ARJ} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2182).
-spec combinations(list(ARN), integer()) -> list(list(ARN)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _ ->
            case Items of
                [] ->
                    [];

                [First | Rest] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Rest, N - 1),
                            fun(Com) -> [First | Com] end
                        ),
                        lists:reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Rest, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2214).
-spec combination_pairs_loop(list(ARU)) -> list(list({ARU, ARU})).
combination_pairs_loop(Items) ->
    case Items of
        [] ->
            [];

        [First | Rest] ->
            First_combinations = map(Rest, fun(Other) -> {First, Other} end),
            [First_combinations | combination_pairs_loop(Rest)]
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2209).
-spec combination_pairs(list(ARR)) -> list({ARR, ARR}).
combination_pairs(Items) ->
    _pipe = combination_pairs_loop(Items),
    flatten(_pipe).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2251).
-spec transpose(list(list(ASC))) -> list(list(ASC)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Rest] ->
            transpose(Rest);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                flatten(_pipe@1)
            end,
            Rest@1 = transpose(
                map(Rows, fun(_capture) -> drop(_capture, 1) end)
            ),
            [Firsts | Rest@1]
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2233).
-spec interleave(list(list(ARY))) -> list(ARY).
interleave(List) ->
    _pipe = transpose(List),
    flatten(_pipe).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2292).
-spec shuffle_pair_unwrap_loop(list({float(), ASK}), list(ASK)) -> list(ASK).
shuffle_pair_unwrap_loop(List, Acc) ->
    case List of
        [] ->
            Acc;

        [Elem_pair | Enumerable] ->
            shuffle_pair_unwrap_loop(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2300).
-spec do_shuffle_by_pair_indexes(list({float(), ASO})) -> list({float(), ASO}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2285).
-spec shuffle(list(ASH)) -> list(ASH).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(_pipe, [], fun(Acc, A) -> [{rand:uniform(), A} | Acc] end),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    shuffle_pair_unwrap_loop(_pipe@2, []).
