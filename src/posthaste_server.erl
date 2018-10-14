%%% ------------------------------------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  18.5.19
%% @hidden

%% -------------------------------------------------------------------------------------------------
-module(posthaste_server).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/1
        ,start/1
        ,add/6
        ,delete/6
        ,keep/1
        ,dump/1
        ,module/1]).

%% 'gen_server' callbacks:
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(S, state).
-record(?S, {module, requests, keep}).

-define(ADD_TAG, add).
-define(DELETE_TAG, delete).
-define(KEEP_TAG, keep).
-define(DUMP_TAG, dump).
-define(MODULE_TAG, module).

-include("posthaste.hrl").

%% -------------------------------------------------------------------------------------------------
%% API:

start_link(Name) when erlang:is_atom(Name)->
    gen_server:start_link({local, Name}, ?MODULE, Name, []).

start(Name) when erlang:is_atom(Name)->
    gen_server:start({local, Name}, ?MODULE, Name, []).


add(Name, Hook, Key, Mod, Func, Priority) when erlang:is_atom(Name) andalso
                                               ?is_hook(Hook)       andalso
                                               ?is_key(Key)         andalso
                                               erlang:is_atom(Mod)  andalso
                                               erlang:is_atom(Func) andalso
                                               ?is_priority(Priority)    ->
    gen_server:call(Name, {?ADD_TAG, {Hook, Key, Mod, Func, Priority}}, infinity).


delete(Name, Hook, Key, Mod, Func, Priority) when erlang:is_atom(Name) andalso
                                                  ?is_hook(Hook)       andalso
                                                  ?is_key(Key)         andalso
                                                  erlang:is_atom(Mod)  andalso
                                                  erlang:is_atom(Func) andalso
                                                  ?is_priority(Priority)    ->
    gen_server:call(Name, {?DELETE_TAG, {Hook, Key, Mod, Func, Priority}}, infinity).


keep(Name) when erlang:is_atom(Name) ->
    gen_server:call(Name, ?KEEP_TAG, infinity).


dump(Name) when erlang:is_atom(Name) ->
    gen_server:call(Name, ?DUMP_TAG, infinity).


module(Name) when erlang:is_atom(Name) ->
    gen_server:call(Name, ?MODULE_TAG, infinity).

%% -------------------------------------------------------------------------------------------------
%% 'gen_server' callbacks:

init(HookMod) ->
    case posthaste_code:load(HookMod) of
        ok ->
            _ = erlang:process_flag(trap_exit, true),
            {ok, #?S{module = HookMod, requests = [], keep = false}};
        {error, Rsn} ->
            {stop, Rsn}
    end.


handle_call({?ADD_TAG, {Hook, Key, Mod, Func, Priority}}
           ,_
           ,#?S{module = HookMod, keep = false}=S) ->
    case posthaste_code:add_handler(HookMod, Hook, Key, Mod, Func, Priority) of
        ok ->
            {reply, ok, S};
        Err -> % {error, _}
            {reply, Err, S}
    end;
handle_call({?DELETE_TAG, {Hook, Key, Mod, Func, Priority}}
           ,_
           ,#?S{module = HookMod, keep = false}=S) -> % Hook == Hook
    case posthaste_code:delete_handler(HookMod, Hook, Key, Mod, Func, Priority) of
        ok ->
            {reply, ok, S};
        Err -> % {error, _}
            {reply, Err, S}
    end;
handle_call({Tag, {_, _, _, _, _}}=Req
           ,_
           ,#?S{requests = Reqs}=S) when Tag == ?ADD_TAG orelse
                                         Tag == ?DELETE_TAG -> % keep == true
    {reply, keep, S#?S{requests = [Req|Reqs]}};
handle_call(?KEEP_TAG, _, #?S{keep = Keep}=S) ->
    {Reply, Keep2} =
        if
            Keep ->
                {error, Keep};
            true -> % Keep == false
                {ok, true}
        end,
    {reply, Reply, S#?S{keep = Keep2}};
handle_call(?DUMP_TAG, _, #?S{keep = Keep}=S) ->
    if
        Keep ->
            case posthaste_code:update_handlers(S#?S.module, S#?S.requests) of
                ok ->
                    {reply, ok, S#?S{keep = false, requests = []}};
                {error, _}=Err ->
                    {reply, Err, S}
            end;
        true ->
            {reply, error, S}
    end;
handle_call(?MODULE_TAG, _, #?S{module = HookMod}=S) ->
    {reply, HookMod, S}.


handle_cast(_, S) ->
    {noreply, S}.


handle_info(_, S) ->
    {noreply, S}.


terminate(_, #?S{module = HookMod}) ->
    posthaste_code:unload(HookMod),
    ok.


code_change(_, S, _) ->
    {ok, S}.