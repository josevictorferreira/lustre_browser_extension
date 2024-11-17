-record(error, {
    code :: binary(),
    name :: binary(),
    message :: binary(),
    span :: examples@error_messages:span()
}).
