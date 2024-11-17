-module(examples@todo_lists).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([example_todo_list/0, tasks_to_doc/1, main/0]).
-export_type([task/0, status/0]).

-type task() :: {task, status(), binary(), list(task())}.

-type status() :: todo | done | in_progress.

-spec status_to_bullet(status()) -> binary().
status_to_bullet(Status) ->
    case Status of
        todo ->
            <<"- [ ]"/utf8>>;

        done ->
            <<"- [X]"/utf8>>;

        in_progress ->
            <<"- […]"/utf8>>
    end.

-spec example_todo_list() -> list(task()).
example_todo_list() ->
    [{task,
            in_progress,
            <<"publish Glam v1.1.0"/utf8>>,
            [{task, in_progress, <<"write a tutorial on todo lists"/utf8>>, []},
                {task,
                    in_progress,
                    <<"add `doc.flex_break`"/utf8>>,
                    [{task,
                            done,
                            <<"add the appropriate type variant"/utf8>>,
                            []},
                        {task, done, <<"implement the missing cases"/utf8>>, []},
                        {task, todo, <<"add some tests"/utf8>>, []}]}]},
        {task, todo, <<"get some sleep"/utf8>>, []}].

-spec task_to_doc(task()) -> glam@doc:document().
task_to_doc(Task) ->
    Task_line = <<<<(status_to_bullet(erlang:element(2, Task)))/binary,
            " "/utf8>>/binary,
        (erlang:element(3, Task))/binary>>,
    Task_doc = glam@doc:from_string(Task_line),
    case gleam@list:is_empty(erlang:element(4, Task)) of
        true ->
            Task_doc;

        false ->
            _pipe = [Task_doc,
                {break, <<""/utf8>>, <<""/utf8>>},
                tasks_to_doc(erlang:element(4, Task))],
            _pipe@1 = glam@doc:concat(_pipe),
            glam@doc:nest(_pipe@1, 2)
    end.

-spec tasks_to_doc(list(task())) -> glam@doc:document().
tasks_to_doc(Tasks) ->
    _pipe = gleam@list:map(Tasks, fun task_to_doc/1),
    _pipe@1 = glam@doc:join(_pipe, {break, <<""/utf8>>, <<""/utf8>>}),
    glam@doc:force_break(_pipe@1).

-spec main() -> nil.
main() ->
    _pipe = example_todo_list(),
    _pipe@1 = tasks_to_doc(_pipe),
    _pipe@2 = glam@doc:to_string(_pipe@1, 10000),
    gleam@io:println(_pipe@2).
