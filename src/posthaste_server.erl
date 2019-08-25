%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(posthaste_server).
-author('pouriya.jahanbakhsh@gmail.com').
-behaviour(gen_server).
%% -----------------------------------------------------------------------------
%% Exports:

%% API
-export([
    add/6,
    delete/6,
    handlers/3,
    handlers/4,
    has_hook/2,
    has_key/3,
    keys/2,
    all/1
]).

%% 'gen_server' callbacks:
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(ADD_TAG, add).
-define(DELETE_TAG, delete).

-define(S, state).
-record(?S, {module, recent}).

-include("posthaste_try_compat.hrl").
-include("posthaste_guard.hrl").

%% -----------------------------------------------------------------------------
%% API:

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, Name, []).


add(
    Name,
    Hook,
    Key,
    Module,
    Function,
    Priority
) when erlang:is_atom(Name)     andalso
       ?is_hook(Hook)           andalso
       ?is_key(Key)             andalso
       erlang:is_atom(Module)   andalso
       erlang:is_atom(Function) andalso
       ?is_priority(Priority)        ->
    try erl_parse:abstract(Key) of
        _ ->
            gen_server:call(
                Name,
                {?ADD_TAG, Hook, Key, Module, Function, Priority},
                infinity
            )
    catch
        ?define_stacktrace(_, _, Stacktrace) ->
            erlang:raise(
                error,
                function_clause,
                erlang:tl(?get_stacktrace(Stacktrace))
            )
    end.


delete(
    Name,
    Hook,
    Key,
    Module,
    Function,
    Priority
) when erlang:is_atom(Name)     andalso
       ?is_hook(Hook)           andalso
       ?is_key(Key)             andalso
       erlang:is_atom(Module)   andalso
       erlang:is_atom(Function) andalso
       ?is_priority(Priority)        ->
    try erl_parse:abstract(Key) of
        _ ->
            gen_server:call(
                Name,
                {?DELETE_TAG, Hook, Key, Module, Function, Priority},
                infinity
            )
    catch
        ?define_stacktrace(_, _, Stacktrace) ->
            erlang:raise(
                error,
                function_clause,
                erlang:tl(?get_stacktrace(Stacktrace))
            )
    end.


handlers(Name, Hook, Key) ->
    try
        Name:Hook(Key)
    catch
        _:_ ->
            []
    end.


handlers(Name, Hook, Key, Notify) ->
    try
        Name:Hook(Key)
    catch
        _:_ ->
            _ = Notify == true andalso maybe_notify_server(Name, Hook),
            []
    end.


has_hook(Name, Hook) ->
    try
        _ = Name:Hook(undefined),
        true
    catch
        _:_ ->
            false
    end.


has_key(Name, Hook, Key) ->
    try Name:Hook(Key) of
        [] ->
            false;
        _ ->
            true
    catch
        _:_ ->
            false
    end.


keys(Name, Hook) ->
    try
        Name:Hook()
    catch
        _:_ ->
            []
    end.


all(Name) ->
    try
        hooks(Name)
    catch
        _:_ ->
            []
    end.

%% -----------------------------------------------------------------------------
%% 'gen_server' callbacks:

%% @hidden
init(Mod) ->
    _ = erlang:process_flag(trap_exit, true),
    _ = code:load_file(Mod),
    case erlang:function_exported(Mod, module_info, 0) of
        false ->
            _ = build_module(Mod, []),
            {ok, #?S{module = Mod}};
        _ -> % true
        {stop, {already_owned, #{module => Mod}}}
    end.


%% @hidden
handle_call({?ADD_TAG, Hook}, _, #?S{recent = Hook}=S) ->
    {reply, error, S};

handle_call(
    {?ADD_TAG, Hook},
    _,
    #?S{module = Mod}=S
) ->
    Hooks = hooks(Mod),
    {Result, Recent} =
        case lists:keyfind(Hook, 1, Hooks) of
            {_, _} ->
                {error, undefined};
            _ ->
                {
                    build_module(
                        Mod,
                        [
                            handler_function(Hook, []),
                            keys_function(Hook, []) |
                            to_ast(Hooks)
                        ]
                    ),
                    Hook
                }
        end,
    _ = maybe_reload(Result, Mod),
    {reply, Result, S#?S{recent = Recent}};

handle_call(
    {?ADD_TAG, Hook, Key, Module, Function, Priority},
    _,
    #?S{module = Mod}=S
) ->
    Result =
        case do_add(Hook, Key, Module, Function, Priority, hooks(Mod)) of
            {ok, Funcs} ->
                build_module(Mod, to_ast(Funcs));
            Err ->
                Err
        end,
    _ = maybe_reload(Result, Mod),
    {reply, Result, S#?S{recent = undefined}};

handle_call(
    {?DELETE_TAG, Hook, Key, Module, Function, Priority},
    _,
    #?S{module = Mod}=S
) ->
    Result =
        case do_delete(Hook, Key, Module, Function, Priority, hooks(Mod)) of
            {ok, Funcs} ->
                build_module(Mod, to_ast(Funcs));
            Err ->
                Err
        end,
    _ = maybe_reload(Result, Mod),
    {reply, Result, S#?S{recent = undefined}};

handle_call(Request, _, S) ->
    {reply, {error, {unknown, #{request => Request}}}, S}.


%% @hidden
handle_cast(_, S) ->
    {noreply, S}.


%% @hidden
handle_info(_, S) ->
    {noreply, S}.


%% @hidden
terminate(_, #?S{module = Mod}) ->
    _ = code:soft_purge(Mod),
    _ = code:delete(Mod),
    ok.


%% @hidden
code_change(_, S, _) ->
    {ok, S}.

%% -----------------------------------------------------------------------------
%% Internals:

maybe_reload(ok, Mod) ->
    code:soft_purge(Mod),
    code:load_file(Mod);
maybe_reload(_, _) ->
    ok.


build_module(Mod, ExtraForms) ->
    ModAttr = {attribute, 0, module, Mod},
    ExportAllAttr = {attribute, 0, compile, export_all},
    FileAttr = {
        attribute,
        0,
        file,
        {string, 0, erlang:atom_to_list(Mod) ++ ".erl"}
    },
    DynamicAttr = {attribute, 0, emeter_dynamic, [erlang:pid_to_list(self())]},
    Forms = [ModAttr, FileAttr, ExportAllAttr, DynamicAttr | ExtraForms],
    {ok, _, Binary} = compile:forms(Forms, [return_errors, nowarn_export_all]),
    {module, _} = code:load_binary(Mod, erlang:atom_to_list(Mod), Binary),
    ok.


do_add(Hook, Key, Module, Function, Priority, Hooks) ->
    Handler = {Priority, Module, Function},
    case lists:keytake(Hook, 1, Hooks) of
        {_, {_, Keys}, Hooks2} ->
            case lists:keytake(Key, 1, Keys) of
                {_, {_, Handlers}, Keys2} ->
                    case lists:member(Handler, Handlers) of
                        false ->
                            {
                                ok,
                                [
                                    {
                                        Hook,
                                        [
                                            {
                                                Key,
                                                add_handler(
                                                    false,
                                                    Handlers,
                                                    Handler,
                                                    []
                                                )
                                            } |
                                            Keys2
                                        ]
                                    } |
                                    Hooks2
                                ]
                            };
                        _ ->
                            {error, {already_exists, #{handler => Handler}}}
                    end;
                _ ->
                    {ok, [{Hook, [{Key, [Handler]} | Keys]} | Hooks2]}
            end;
        _ ->
            {ok, [{Hook, [{Key, [Handler]}]} | Hooks]}
    end.


do_delete(Hook, Key, Module, Function, Priority, Hooks) ->
    Handler = {Priority, Module, Function},
    case lists:keytake(Hook, 1, Hooks) of
        {_, {_, Keys}, Hooks2} ->
            case lists:keytake(Key, 1, Keys) of
                {_, {_, Handlers}, Keys2} ->
                    case lists:member(Handler, Handlers) of
                        true ->
                            {
                                ok,
                                [
                                    {
                                        Hook,
                                        [
                                            {
                                                Key,
                                                lists:delete(Handler, Handlers)
                                            } |
                                            Keys2
                                        ]
                                    } |
                                    Hooks2
                                ]
                            };
                        _ ->
                            {error, {not_found, #{handler => Handler}}}
                    end;
                _ ->
                    {error, {not_found, #{key => Key}}}
            end;
        _ ->
            {error, {not_found, #{hook => Hook}}}
    end.


handler_function(Hook, Keys) ->
    {function, 0, Hook, 1, clauses(Keys)}.


clauses([{Key, Handlers} | Keys]) ->
    [
        {
            clause,
            0,
            [erl_parse:abstract(Key)],
            [],
            [
                erl_parse:abstract(Handlers)
            ]
        } |
        clauses(Keys)
    ];

clauses(_) ->
    [{clause, 0, [{var, 0, '_'}], [], [{nil, 0}]}].


keys_function(Hook, Keys) ->
    {
        function,
        0,
        Hook,
        0,
        [{clause, 0, [], [], [erl_parse:abstract([Key || {Key, _} <- Keys])]}]
    }.

hooks(Mod) ->
    [
        {FuncName, [{Key, Mod:FuncName(Key)} || Key <- Mod:FuncName()]} ||
        {FuncName , 0} <- Mod:module_info(exports),
        FuncName /= module_info
    ].


to_ast([{Hook, Keys} | Hooks]) ->
    [handler_function(Hook, Keys), keys_function(Hook, Keys) | to_ast(Hooks)];

to_ast(_) ->
    [].


maybe_notify_server(Name, Hook) ->
    erlang:system_time(microsecond) rem 5 == 0
        andalso
    gen_server:call(Name, {?ADD_TAG, Hook}).


add_handler(
    false,
    [{Priority, _, _}=Handler | Handlers],
    {Priority2, _, _}=Handler2,
    Ret
) ->
    if
        Priority > Priority2 ->
            add_handler(true, Handlers, Handler2, [Handler, Handler2 | Ret]);
        true ->
            add_handler(false, Handlers, Handler2, [Handler | Ret])
    end;

add_handler(Add, [Handler | Handlers], Handler2, Ret) ->
    add_handler(Add, Handlers, Handler2, [Handler | Ret]);

add_handler(false, _, Handler2, Ret) ->
    lists:reverse([Handler2 | Ret]);

add_handler(_, _, _, Ret) ->
    lists:reverse(Ret).
