-module(gleam@string_tree).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([prepend_tree/2, append_tree/2, new/0, from_strings/1, concat/1, from_string/1, prepend/2, append/2, to_string/1, byte_size/1, join/2, lowercase/1, uppercase/1, reverse/1, split/2, replace/3, is_equal/2, is_empty/1]).
-export_type([string_tree/0, direction/0]).

-type string_tree() :: any().

-type direction() :: all.

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 48).
-spec prepend_tree(string_tree(), string_tree()) -> string_tree().
prepend_tree(Tree, Prefix) ->
    gleam_stdlib:iodata_append(Prefix, Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 59).
-spec append_tree(string_tree(), string_tree()) -> string_tree().
append_tree(Tree, Suffix) ->
    gleam_stdlib:iodata_append(Tree, Suffix).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 24).
-spec new() -> string_tree().
new() ->
    gleam_stdlib:identity([]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 71).
-spec from_strings(list(binary())) -> string_tree().
from_strings(Strings) ->
    gleam_stdlib:identity(Strings).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 83).
-spec concat(list(string_tree())) -> string_tree().
concat(Trees) ->
    gleam_stdlib:identity(Trees).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 95).
-spec from_string(binary()) -> string_tree().
from_string(String) ->
    gleam_stdlib:identity(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 32).
-spec prepend(string_tree(), binary()) -> string_tree().
prepend(Tree, Prefix) ->
    append_tree(from_string(Prefix), Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 40).
-spec append(string_tree(), binary()) -> string_tree().
append(Tree, Second) ->
    append_tree(Tree, from_string(Second)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 108).
-spec to_string(string_tree()) -> binary().
to_string(Tree) ->
    unicode:characters_to_binary(Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 118).
-spec byte_size(string_tree()) -> integer().
byte_size(Tree) ->
    erlang:iolist_size(Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 128).
-spec join(list(string_tree()), binary()) -> string_tree().
join(Trees, Sep) ->
    _pipe = Trees,
    _pipe@1 = gleam@list:intersperse(_pipe, from_string(Sep)),
    concat(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 137).
-spec lowercase(string_tree()) -> string_tree().
lowercase(Tree) ->
    string:lowercase(Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 148).
-spec uppercase(string_tree()) -> string_tree().
uppercase(Tree) ->
    string:uppercase(Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 158).
-spec reverse(string_tree()) -> string_tree().
reverse(Tree) ->
    string:reverse(Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 185).
-spec do_split(string_tree(), binary()) -> list(string_tree()).
do_split(Tree, Pattern) ->
    string:split(Tree, Pattern, all).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 176).
-spec split(string_tree(), binary()) -> list(string_tree()).
split(Tree, Pattern) ->
    do_split(Tree, Pattern).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 196).
-spec replace(string_tree(), binary(), binary()) -> string_tree().
replace(Tree, Pattern, Substitute) ->
    gleam_stdlib:string_replace(Tree, Pattern, Substitute).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 222).
-spec is_equal(string_tree(), string_tree()) -> boolean().
is_equal(A, B) ->
    string:equal(A, B).

-file("/Users/louis/src/gleam/stdlib/src/gleam/string_tree.gleam", 246).
-spec is_empty(string_tree()) -> boolean().
is_empty(Tree) ->
    string:is_empty(Tree).
