
-define(is_hook(X), (erlang:is_atom(X) andalso X /= module_info)).

-define(
    is_key(X),
    (
        erlang:is_atom(X) orelse
        erlang:is_binary(X) orelse
        erlang:is_number(X) orelse
        erlang:is_list(X) orelse
        erlang:is_tuple(X)
    )
).

-define(is_priority(X), (erlang:is_integer(X) andalso X > -1)).
