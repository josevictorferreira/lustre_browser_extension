-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 2).
-spec compose(fun((DRZ) -> DSA), fun((DSA) -> DSB)) -> fun((DRZ) -> DSB).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 7).
-spec curry2(fun((DSC, DSD) -> DSE)) -> fun((DSC) -> fun((DSD) -> DSE)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 12).
-spec curry3(fun((DSG, DSH, DSI) -> DSJ)) -> fun((DSG) -> fun((DSH) -> fun((DSI) -> DSJ))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 17).
-spec curry4(fun((DSL, DSM, DSN, DSO) -> DSP)) -> fun((DSL) -> fun((DSM) -> fun((DSN) -> fun((DSO) -> DSP)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 22).
-spec curry5(fun((DSR, DSS, DST, DSU, DSV) -> DSW)) -> fun((DSR) -> fun((DSS) -> fun((DST) -> fun((DSU) -> fun((DSV) -> DSW))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 27).
-spec curry6(fun((DSY, DSZ, DTA, DTB, DTC, DTD) -> DTE)) -> fun((DSY) -> fun((DSZ) -> fun((DTA) -> fun((DTB) -> fun((DTC) -> fun((DTD) -> DTE)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 36).
-spec flip(fun((DTG, DTH) -> DTI)) -> fun((DTH, DTG) -> DTI).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 42).
-spec identity(DTJ) -> DTJ.
identity(X) ->
    X.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 47).
-spec constant(DTK) -> fun((any()) -> DTK).
constant(Value) ->
    fun(_) -> Value end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 56).
-spec tap(DTM, fun((DTM) -> any())) -> DTM.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 62).
-spec apply1(fun((DTO) -> DTP), DTO) -> DTP.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 67).
-spec apply2(fun((DTQ, DTR) -> DTS), DTQ, DTR) -> DTS.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 72).
-spec apply3(fun((DTT, DTU, DTV) -> DTW), DTT, DTU, DTV) -> DTW.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
