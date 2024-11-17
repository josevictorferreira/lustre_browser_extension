-record(task, {
    status :: examples@todo_lists:status(),
    description :: binary(),
    subtasks :: list(examples@todo_lists:task())
}).
