-module(gleam@bytes_tree).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([append_tree/2, prepend_tree/2, concat/1, new/0, from_string/1, prepend_string/2, append_string/2, from_string_tree/1, from_bit_array/1, prepend/2, append/2, concat_bit_arrays/1, to_bit_array/1, byte_size/1]).
-export_type([bytes_tree/0]).

-opaque bytes_tree() :: {bytes, bitstring()} |
    {text, gleam@string_tree:string_tree()} |
    {many, list(bytes_tree())}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 69).
-spec append_tree(bytes_tree(), bytes_tree()) -> bytes_tree().
append_tree(First, Second) ->
    gleam_stdlib:iodata_append(First, Second).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 60).
-spec prepend_tree(bytes_tree(), bytes_tree()) -> bytes_tree().
prepend_tree(Second, First) ->
    gleam_stdlib:iodata_append(First, Second).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 99).
-spec concat(list(bytes_tree())) -> bytes_tree().
concat(Trees) ->
    gleam_stdlib:identity(Trees).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 36).
-spec new() -> bytes_tree().
new() ->
    gleam_stdlib:identity([]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 120).
-spec from_string(binary()) -> bytes_tree().
from_string(String) ->
    gleam_stdlib:wrap_list(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 81).
-spec prepend_string(bytes_tree(), binary()) -> bytes_tree().
prepend_string(Second, First) ->
    gleam_stdlib:iodata_append(gleam_stdlib:wrap_list(First), Second).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 90).
-spec append_string(bytes_tree(), binary()) -> bytes_tree().
append_string(First, Second) ->
    gleam_stdlib:iodata_append(First, gleam_stdlib:wrap_list(Second)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 130).
-spec from_string_tree(gleam@string_tree:string_tree()) -> bytes_tree().
from_string_tree(Tree) ->
    gleam_stdlib:wrap_list(Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 139).
-spec from_bit_array(bitstring()) -> bytes_tree().
from_bit_array(Bits) ->
    gleam_stdlib:wrap_list(Bits).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 44).
-spec prepend(bytes_tree(), bitstring()) -> bytes_tree().
prepend(Second, First) ->
    gleam_stdlib:iodata_append(gleam_stdlib:wrap_list(First), Second).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 52).
-spec append(bytes_tree(), bitstring()) -> bytes_tree().
append(First, Second) ->
    gleam_stdlib:iodata_append(First, gleam_stdlib:wrap_list(Second)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 108).
-spec concat_bit_arrays(list(bitstring())) -> bytes_tree().
concat_bit_arrays(Bits) ->
    gleam_stdlib:identity(Bits).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 158).
-spec to_list(list(list(bytes_tree())), list(bitstring())) -> list(bitstring()).
to_list(Stack, Acc) ->
    case Stack of
        [] ->
            Acc;

        [[] | Remaining_stack] ->
            to_list(Remaining_stack, Acc);

        [[{bytes, Bits} | Rest] | Remaining_stack@1] ->
            to_list([Rest | Remaining_stack@1], [Bits | Acc]);

        [[{text, Tree} | Rest@1] | Remaining_stack@2] ->
            Bits@1 = gleam_stdlib:identity(gleam@string_tree:to_string(Tree)),
            to_list([Rest@1 | Remaining_stack@2], [Bits@1 | Acc]);

        [[{many, Trees} | Rest@2] | Remaining_stack@3] ->
            to_list([Trees, Rest@2 | Remaining_stack@3], Acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 151).
-spec to_bit_array(bytes_tree()) -> bitstring().
to_bit_array(Tree) ->
    erlang:list_to_bitstring(Tree).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_tree.gleam", 182).
-spec byte_size(bytes_tree()) -> integer().
byte_size(Tree) ->
    erlang:iolist_size(Tree).
