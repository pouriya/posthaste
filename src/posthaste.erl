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
-export([
    start_link/1,
    add/6,
    delete/6,
    handlers/3,
    handlers/4,
    has_hook/2,
    has_key/3,
    keys/2,
    all/1
]).

%% -------------------------------------------------------------------------------------------------
%% Types:

-type name() :: atom().
-type hook() :: atom().
-type key() :: atom() | binary() | number() | list() | tuple().
-type handlers() :: [] | [handler()].
-type  handler() :: {priority(), module(), func()}.
-type   priority() :: non_neg_integer().
-type   func() :: atom().

-type start_return() :: {'ok', pid()} | {'error', term()}.

-export_type([name/0
             ,hook/0
             ,key/0
             ,handlers/0
             ,handler/0
             ,priority/0
             ,func/0
             ,start_return/0]).

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
handlers(name(), hook(), key()) ->
    handlers().
%% @doc
%%     Yields handlers of a key for a hook.
%% @end
handlers(Name, Hook, Key) ->
    posthaste_server:handlers(Name, Hook, Key).


-spec
handlers(name(), hook(), key(), boolean()) ->
    handlers().
%% @doc
%%     Yields handlers of a key for a hook.<br/>
%%     Also if a hook does not exists, caller may request for adding it with
%%     no handlers.
%% @end
handlers(Name, Hook, Key, Notify) ->
    posthaste_server:handlers(Name, Hook, Key, Notify).


-spec
has_hook(name(), hook()) ->
    boolean().
has_hook(Name, Hook) ->
    posthaste_server:has_hook(Name, Hook).


-spec
has_key(name(), hook(), key()) ->
    boolean().
has_key(Name, Hook, Key) ->
    posthaste_server:has_key(Name, Hook, Key).


-spec
keys(name(), hook()) ->
    [] | [key()].
keys(Name, Hook) ->
    posthaste_server:keys(Name, Hook).


-spec
all(name()) ->
    [] | [{hook(), [] | [{key(), handlers()}]}].
all(Name) ->
    posthaste_server:all(Name).
