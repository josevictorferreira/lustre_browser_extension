-module(lustre_dev_tools@esbuild@preprocess).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([copy_deep_ffi/0]).

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/esbuild/preprocess.gleam", 80).
-spec can_resolve_relative_gleam_imports(binary()) -> boolean().
can_resolve_relative_gleam_imports(Path) ->
    case filepath:extension(Path) of
        {ok, <<"js"/utf8>>} ->
            true;

        {ok, <<"mjs"/utf8>>} ->
            true;

        {ok, <<"ts"/utf8>>} ->
            true;

        {ok, <<"mts"/utf8>>} ->
            true;

        _ ->
            false
    end.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/esbuild/preprocess.gleam", 87).
-spec resolve_relative_gleam_imports(binary(), binary()) -> binary().
resolve_relative_gleam_imports(Path, Source) ->
    gleam@bool:guard(
        not can_resolve_relative_gleam_imports(Path),
        Source,
        fun() ->
            Options = {options, false, true},
            _assert_subject = gleam@regex:compile(
                <<"^import.+\"(\\..+)\";$"/utf8>>,
                Options
            ),
            {ok, Re} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"lustre_dev_tools/esbuild/preprocess"/utf8>>,
                                function => <<"resolve_relative_gleam_imports"/utf8>>,
                                line => 90})
            end,
            gleam@list:fold(
                gleam@regex:scan(Re, Source),
                Source,
                fun(Source@1, Match) ->
                    {match, Match@1, [{some, Import_path}]} = case Match of
                        {match, _, [{some, _}]} -> Match;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        value => _assert_fail@1,
                                        module => <<"lustre_dev_tools/esbuild/preprocess"/utf8>>,
                                        function => <<"resolve_relative_gleam_imports"/utf8>>,
                                        line => 93})
                    end,
                    Resolved_import_path = gleam@string:replace(
                        Import_path,
                        <<".gleam"/utf8>>,
                        <<".mjs"/utf8>>
                    ),
                    Resolved_import = gleam@string:replace(
                        Match@1,
                        Import_path,
                        Resolved_import_path
                    ),
                    gleam@string:replace(Source@1, Match@1, Resolved_import)
                end
            )
        end
    ).

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/esbuild/preprocess.gleam", 19).
-spec copy_deep_ffi() -> lustre_dev_tools@cli:cli(nil).
copy_deep_ffi() ->
    lustre_dev_tools@cli:'try'(
        lustre_dev_tools@project:config(),
        fun(Config) ->
            Root = lustre_dev_tools@project:root(),
            Src = filepath:join(Root, <<"src"/utf8>>),
            Out = filepath:join(
                Root,
                <<"build/dev/javascript/"/utf8,
                    (erlang:element(2, Config))/binary>>
            ),
            _assert_subject = simplifile:get_files(Src),
            {ok, Files} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"lustre_dev_tools/esbuild/preprocess"/utf8>>,
                                function => <<"copy_deep_ffi"/utf8>>,
                                line => 25})
            end,
            lustre_dev_tools@cli:from_result(
                (gleam@list:try_each(
                    Files,
                    fun(Path) ->
                        gleam@bool:guard(
                            gleam@string:ends_with(Path, <<".gleam"/utf8>>),
                            {ok, nil},
                            fun() ->
                                <<"./src/"/utf8, Module_path/binary>> = case Path of
                                    <<"./src/"/utf8, _/binary>> -> Path;
                                    _assert_fail@1 ->
                                        erlang:error(
                                                #{gleam_error => let_assert,
                                                    message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                    value => _assert_fail@1,
                                                    module => <<"lustre_dev_tools/esbuild/preprocess"/utf8>>,
                                                    function => <<"copy_deep_ffi"/utf8>>,
                                                    line => 31}
                                            )
                                end,
                                Out_path = filepath:join(Out, Module_path),
                                Out_dir = filepath:directory_name(Out_path),
                                gleam@result:'try'(
                                    case filepath:extension(Path) of
                                        {ok, <<"js"/utf8>>} ->
                                            gleam@result:'try'(
                                                begin
                                                    _pipe = simplifile:read(
                                                        Path
                                                    ),
                                                    _pipe@1 = gleam@result:map_error(
                                                        _pipe,
                                                        fun(_capture) ->
                                                            {cannot_read_file,
                                                                _capture,
                                                                Path}
                                                        end
                                                    ),
                                                    gleam@result:map(
                                                        _pipe@1,
                                                        fun(_capture@1) ->
                                                            resolve_relative_gleam_imports(
                                                                Path,
                                                                _capture@1
                                                            )
                                                        end
                                                    )
                                                end,
                                                fun(Source) ->
                                                    gleam@result:'try'(
                                                        begin
                                                            _pipe@2 = simplifile:create_directory_all(
                                                                Out_dir
                                                            ),
                                                            gleam@result:map_error(
                                                                _pipe@2,
                                                                fun(_capture@2) ->
                                                                    {cannot_create_directory,
                                                                        _capture@2,
                                                                        Out_dir}
                                                                end
                                                            )
                                                        end,
                                                        fun(_) ->
                                                            gleam@result:'try'(
                                                                begin
                                                                    _pipe@3 = simplifile:write(
                                                                        Out_path,
                                                                        Source
                                                                    ),
                                                                    gleam@result:map_error(
                                                                        _pipe@3,
                                                                        fun(
                                                                            _capture@3
                                                                        ) ->
                                                                            {cannot_write_file,
                                                                                _capture@3,
                                                                                Out_path}
                                                                        end
                                                                    )
                                                                end,
                                                                fun(_) ->
                                                                    {ok, nil}
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            );

                                        {ok, <<"mjs"/utf8>>} ->
                                            gleam@result:'try'(
                                                begin
                                                    _pipe = simplifile:read(
                                                        Path
                                                    ),
                                                    _pipe@1 = gleam@result:map_error(
                                                        _pipe,
                                                        fun(_capture) ->
                                                            {cannot_read_file,
                                                                _capture,
                                                                Path}
                                                        end
                                                    ),
                                                    gleam@result:map(
                                                        _pipe@1,
                                                        fun(_capture@1) ->
                                                            resolve_relative_gleam_imports(
                                                                Path,
                                                                _capture@1
                                                            )
                                                        end
                                                    )
                                                end,
                                                fun(Source) ->
                                                    gleam@result:'try'(
                                                        begin
                                                            _pipe@2 = simplifile:create_directory_all(
                                                                Out_dir
                                                            ),
                                                            gleam@result:map_error(
                                                                _pipe@2,
                                                                fun(_capture@2) ->
                                                                    {cannot_create_directory,
                                                                        _capture@2,
                                                                        Out_dir}
                                                                end
                                                            )
                                                        end,
                                                        fun(_) ->
                                                            gleam@result:'try'(
                                                                begin
                                                                    _pipe@3 = simplifile:write(
                                                                        Out_path,
                                                                        Source
                                                                    ),
                                                                    gleam@result:map_error(
                                                                        _pipe@3,
                                                                        fun(
                                                                            _capture@3
                                                                        ) ->
                                                                            {cannot_write_file,
                                                                                _capture@3,
                                                                                Out_path}
                                                                        end
                                                                    )
                                                                end,
                                                                fun(_) ->
                                                                    {ok, nil}
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            );

                                        {ok, <<"ts"/utf8>>} ->
                                            gleam@result:'try'(
                                                begin
                                                    _pipe = simplifile:read(
                                                        Path
                                                    ),
                                                    _pipe@1 = gleam@result:map_error(
                                                        _pipe,
                                                        fun(_capture) ->
                                                            {cannot_read_file,
                                                                _capture,
                                                                Path}
                                                        end
                                                    ),
                                                    gleam@result:map(
                                                        _pipe@1,
                                                        fun(_capture@1) ->
                                                            resolve_relative_gleam_imports(
                                                                Path,
                                                                _capture@1
                                                            )
                                                        end
                                                    )
                                                end,
                                                fun(Source) ->
                                                    gleam@result:'try'(
                                                        begin
                                                            _pipe@2 = simplifile:create_directory_all(
                                                                Out_dir
                                                            ),
                                                            gleam@result:map_error(
                                                                _pipe@2,
                                                                fun(_capture@2) ->
                                                                    {cannot_create_directory,
                                                                        _capture@2,
                                                                        Out_dir}
                                                                end
                                                            )
                                                        end,
                                                        fun(_) ->
                                                            gleam@result:'try'(
                                                                begin
                                                                    _pipe@3 = simplifile:write(
                                                                        Out_path,
                                                                        Source
                                                                    ),
                                                                    gleam@result:map_error(
                                                                        _pipe@3,
                                                                        fun(
                                                                            _capture@3
                                                                        ) ->
                                                                            {cannot_write_file,
                                                                                _capture@3,
                                                                                Out_path}
                                                                        end
                                                                    )
                                                                end,
                                                                fun(_) ->
                                                                    {ok, nil}
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            );

                                        {ok, <<"mts"/utf8>>} ->
                                            gleam@result:'try'(
                                                begin
                                                    _pipe = simplifile:read(
                                                        Path
                                                    ),
                                                    _pipe@1 = gleam@result:map_error(
                                                        _pipe,
                                                        fun(_capture) ->
                                                            {cannot_read_file,
                                                                _capture,
                                                                Path}
                                                        end
                                                    ),
                                                    gleam@result:map(
                                                        _pipe@1,
                                                        fun(_capture@1) ->
                                                            resolve_relative_gleam_imports(
                                                                Path,
                                                                _capture@1
                                                            )
                                                        end
                                                    )
                                                end,
                                                fun(Source) ->
                                                    gleam@result:'try'(
                                                        begin
                                                            _pipe@2 = simplifile:create_directory_all(
                                                                Out_dir
                                                            ),
                                                            gleam@result:map_error(
                                                                _pipe@2,
                                                                fun(_capture@2) ->
                                                                    {cannot_create_directory,
                                                                        _capture@2,
                                                                        Out_dir}
                                                                end
                                                            )
                                                        end,
                                                        fun(_) ->
                                                            gleam@result:'try'(
                                                                begin
                                                                    _pipe@3 = simplifile:write(
                                                                        Out_path,
                                                                        Source
                                                                    ),
                                                                    gleam@result:map_error(
                                                                        _pipe@3,
                                                                        fun(
                                                                            _capture@3
                                                                        ) ->
                                                                            {cannot_write_file,
                                                                                _capture@3,
                                                                                Out_path}
                                                                        end
                                                                    )
                                                                end,
                                                                fun(_) ->
                                                                    {ok, nil}
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            );

                                        _ ->
                                            gleam@result:'try'(
                                                begin
                                                    _pipe@4 = simplifile:create_directory_all(
                                                        Out_dir
                                                    ),
                                                    gleam@result:map_error(
                                                        _pipe@4,
                                                        fun(_capture@4) ->
                                                            {cannot_create_directory,
                                                                _capture@4,
                                                                Out_dir}
                                                        end
                                                    )
                                                end,
                                                fun(_) ->
                                                    gleam@result:'try'(
                                                        begin
                                                            _pipe@5 = simplifile:copy_file(
                                                                Path,
                                                                Out_path
                                                            ),
                                                            gleam@result:map_error(
                                                                _pipe@5,
                                                                fun(_capture@5) ->
                                                                    {cannot_write_file,
                                                                        _capture@5,
                                                                        Out_path}
                                                                end
                                                            )
                                                        end,
                                                        fun(_) -> {ok, nil} end
                                                    )
                                                end
                                            )
                                    end,
                                    fun(_) -> {ok, nil} end
                                )
                            end
                        )
                    end
                ))
            )
        end
    ).
