-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, nil_error/1, values/1, try_recover/2]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 20).
-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 41).
-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 66).
-spec map({ok, BZN} | {error, BZO}, fun((BZN) -> BZR)) -> {ok, BZR} |
    {error, BZO}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 91).
-spec map_error({ok, BZU} | {error, BZV}, fun((BZV) -> BZY)) -> {ok, BZU} |
    {error, BZY}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 120).
-spec flatten({ok, {ok, CAB} | {error, CAC}} | {error, CAC}) -> {ok, CAB} |
    {error, CAC}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 158).
-spec 'try'({ok, CAJ} | {error, CAK}, fun((CAJ) -> {ok, CAN} | {error, CAK})) -> {ok,
        CAN} |
    {error, CAK}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 170).
-spec then({ok, CAS} | {error, CAT}, fun((CAS) -> {ok, CAW} | {error, CAT})) -> {ok,
        CAW} |
    {error, CAT}.
then(Result, Fun) ->
    'try'(Result, Fun).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 192).
-spec unwrap({ok, CBB} | {error, any()}, CBB) -> CBB.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 214).
-spec lazy_unwrap({ok, CBF} | {error, any()}, fun(() -> CBF)) -> CBF.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 236).
-spec unwrap_error({ok, any()} | {error, CBK}, CBK) -> CBK.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 258).
-spec unwrap_both({ok, CBN} | {error, CBN}) -> CBN.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 308).
-spec 'or'({ok, CBW} | {error, CBX}, {ok, CBW} | {error, CBX}) -> {ok, CBW} |
    {error, CBX}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 341).
-spec lazy_or({ok, CCE} | {error, CCF}, fun(() -> {ok, CCE} | {error, CCF})) -> {ok,
        CCE} |
    {error, CCF}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 367).
-spec all(list({ok, CCM} | {error, CCN})) -> {ok, list(CCM)} | {error, CCN}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 387).
-spec partition_loop(list({ok, CDB} | {error, CDC}), list(CDB), list(CDC)) -> {list(CDB),
    list(CDC)}.
partition_loop(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            partition_loop(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            partition_loop(Rest@1, Oks, [E | Errors])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 383).
-spec partition(list({ok, CCU} | {error, CCV})) -> {list(CCU), list(CCV)}.
partition(Results) ->
    partition_loop(Results, [], []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 409).
-spec replace({ok, any()} | {error, CDK}, CDN) -> {ok, CDN} | {error, CDK}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 430).
-spec replace_error({ok, CDQ} | {error, any()}, CDU) -> {ok, CDQ} | {error, CDU}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 280).
-spec nil_error({ok, CBQ} | {error, any()}) -> {ok, CBQ} | {error, nil}.
nil_error(Result) ->
    replace_error(Result, nil).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 446).
-spec values(list({ok, CDX} | {error, any()})) -> list(CDX).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 479).
-spec try_recover(
    {ok, CED} | {error, CEE},
    fun((CEE) -> {ok, CED} | {error, CEH})
) -> {ok, CED} | {error, CEH}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
