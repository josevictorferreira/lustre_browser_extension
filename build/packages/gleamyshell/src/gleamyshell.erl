-module(gleamyshell).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([execute/3, cwd/0, os/0, home_directory/0, env/1, which/1]).
-export_type([command_output/0, os_family/0, os/0]).

-type command_output() :: {command_output, integer(), binary()}.

-type os_family() :: {unix, os()} | windows.

-type os() :: darwin |
    free_bsd |
    open_bsd |
    linux |
    sun_os |
    {other_os, binary()}.

-spec execute(binary(), binary(), list(binary())) -> {ok, command_output()} |
    {error, binary()}.
execute(Executable, Working_directory, Args) ->
    gleamyshell_ffi:execute(Executable, Working_directory, Args).

-spec cwd() -> {ok, binary()} | {error, nil}.
cwd() ->
    gleamyshell_ffi:cwd().

-spec os() -> os_family().
os() ->
    gleamyshell_ffi:os().

-spec home_directory() -> {ok, binary()} | {error, nil}.
home_directory() ->
    gleamyshell_ffi:home_directory().

-spec env(binary()) -> {ok, binary()} | {error, nil}.
env(Identifier) ->
    gleamyshell_ffi:env(Identifier).

-spec which(binary()) -> {ok, binary()} | {error, nil}.
which(Executable) ->
    gleamyshell_ffi:which(Executable).
