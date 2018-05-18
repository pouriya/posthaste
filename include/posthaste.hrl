-define(AST_FUNCTION_NAME, '$AST').

-define(is_hook(Arg), (erlang:is_atom(Arg) andalso Arg /= ?AST_FUNCTION_NAME)).
-define(is_key(Arg), (erlang:is_atom(Arg) orelse
                      erlang:is_binary(Arg) orelse
                      erlang:is_integer(Arg))).
-define(is_priority(Arg), (erlang:is_integer(Arg) andalso Arg > -1)).