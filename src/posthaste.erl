%%% ------------------------------------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  18.5.19
%% @doc
%%           Erlang/Elixir hooking library.
%% @end

%% -------------------------------------------------------------------------------------------------
-module(posthaste).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API exports
-export([start_link/1
        ,start/1
        ,add/6
        ,callbacks/3
        ,safe_callbacks/3
        ,delete/6
        ,keep/1
        ,dump/1
        ,module/1
        ,hooks/1
        ,keys/2
        ,callback_count/3]).

%% -------------------------------------------------------------------------------------------------
%% Types:

-type name() :: atom().
-type hook() :: atom().
-type key() :: atom() | binary() | integer().
-type callbacks() :: [] | [callback()].
-type  callback() :: {priority(), {module(), func()}}.
-type   priority() :: non_neg_integer().
-type   func() :: atom().

-type start_return() :: {'ok', pid()} | {'error', term()}.

-export_type([name/0
             ,hook/0
             ,key/0
             ,callbacks/0
             ,callback/0
             ,priority/0
             ,func/0]).

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
start_link(name()) ->
    start_return().
%% @doc
%%     Starts and links a hook server process.<br/>
%%     Server process will load new module named <code>Name</code> then <code>Name</code> same as
%%     one of loaded modules.
%% @end
start_link(Name) ->
    posthaste_server:start_link(Name).


-spec
start(name()) ->
    start_return().
%% @doc
%%     Starts stand-alone hook server process.<br/>
%%     Server process will load new module named <code>Name</code> then <code>Name</code> same as
%%     one of loaded modules.
%% @end
start(Name) ->
    posthaste_server:start(Name).


-spec
add(name(), hook(), key(), module(), func(), priority()) ->
    'ok' | 'keep' | {'error', term()}.
%% @doc
%%     Adds a hook.
%% @end
add(Name, Hook, Key, Mod, Func, Priority) ->
    posthaste_server:add(Name, Hook, Key, Mod, Func, Priority).


-spec
delete(name(), hook(), key(), module(), func(), priority()) ->
    'ok' | 'keep' | {'error', term()}.
%% @doc
%%     Deletes a hook.<br/>
%%     It deletes all added hooks with module <code>Mod</code> and function <code>Func</code> and
%%     priority <code>Priority</code>.
%% @end
delete(Name, Hook, Key, Mod, Func, Priority) ->
    posthaste_server:delete(Name, Hook, Key, Mod, Func, Priority).


-spec
keep(name()) ->
    'ok' | 'error'.
%% @doc
%%     Turns server process to keep mode.<br/>
%%     Then server will keep all add/delete requests. You have to call dump/1 to dump requests to
%%     module. If server is already in keep mode, it yields <code>error</code>
%% @end
keep(Name) ->
    posthaste_server:keep(Name).


-spec
dump(name()) ->
    'ok' | 'error' | {'error', term()}.
%% @doc
%%     Makes a request to server process to dump all requests to module.<br/>
%%     If server is not in keep mode, it yields <code>error</code>
%% @end
dump(Name) ->
    posthaste_server:dump(Name).


-spec
module(name()) ->
    module().
%% @doc
%%     Gives the module name that process uses for hooks.
%% @end
module(Name) ->
    posthaste_server:module(Name).


-spec
callbacks(name(), hook(), key()) ->
    {'ok', callbacks()} | {'error', term()}.
%% @doc
%%     Yields callbacks of a key for a hook.
%% @end
callbacks(Name, Hook, Key) ->
    posthaste_code:callbacks(Name, Hook, Key).


-spec
safe_callbacks(name(), hook(), key()) ->
    {'ok', callbacks()} | {'error', term()}.
%% @doc
%%     Yields callbacks of a key for a hook.
%% @end
safe_callbacks(Name, Hook, Key) ->
    posthaste_code:safe_callbacks(Name, Hook, Key).


-spec
hooks(name()) ->
    {'ok', [] | [hook()]} | {'error', term()}.
%% @doc
%%     Yields hooks of a hook module.
%% @end
hooks(Name) ->
    posthaste_code:hooks(Name).


-spec
keys(name(), hook()) ->
    {'ok', [] | [key()]} | {'error', term()}.
%% @doc
%%     Yields keys of a hook.
%% @end
keys(Name, Hook) ->
    posthaste_code:keys(Name, Hook).


-spec
callback_count(name(), hook(), key()) ->
    {'ok', non_neg_integer()} | {'error', term()}.
%% @doc
%%     Yields callback count of a key for a hook.
%% @end
callback_count(Name, Hook, Key) ->
    posthaste_code:callback_count(Name, Hook, Key).