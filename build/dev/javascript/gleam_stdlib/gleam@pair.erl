-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/pair.gleam", 10).
-spec first({YY, any()}) -> YY.
first(Pair) ->
    {A, _} = Pair,
    A.

-file("/Users/louis/src/gleam/stdlib/src/gleam/pair.gleam", 24).
-spec second({any(), AAB}) -> AAB.
second(Pair) ->
    {_, A} = Pair,
    A.

-file("/Users/louis/src/gleam/stdlib/src/gleam/pair.gleam", 38).
-spec swap({AAC, AAD}) -> {AAD, AAC}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/pair.gleam", 53).
-spec map_first({AAE, AAF}, fun((AAE) -> AAG)) -> {AAG, AAF}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/pair.gleam", 68).
-spec map_second({AAH, AAI}, fun((AAI) -> AAJ)) -> {AAH, AAJ}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/pair.gleam", 83).
-spec new(AAK, AAL) -> {AAK, AAL}.
new(First, Second) ->
    {First, Second}.
