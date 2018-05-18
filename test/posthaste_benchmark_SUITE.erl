-module(posthaste_benchmark_SUITE).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% ct callbacks:
-export([init_per_suite/1
        ,end_per_suite/1
        ,all/0
        ,init_per_testcase/2
        ,end_per_testcase/2]).

-export([init/1
        ,handle_call/3
        ,terminate/2]).

-export([ets_add/1
        ,posthaste_add/1
        ,ets_lookup/1
        ,posthaste_lookup/1]).


-define(PNAME, posthaste_hook).
-define(ENAME, ets_hook).
-define(print(Txt), ct:pal(Txt)).
-define(print(Txt, Args), ct:pal(Txt, Args)).
-define(KEY_LOOKUP_COUNT, 10000).
-define(HOOK_COUNT, 100).
-define(HOOK_KEY_COUNT, 3).


all() ->
    [ets_add, posthaste_add, ets_lookup, posthaste_lookup].


init_per_suite(Config) ->
    case os:getenv("ERL_POSTHASTE_BENCHMARK") of
        "true" ->
            application:ensure_all_started(sasl),
            Config;
        "1" ->
            application:ensure_all_started(sasl),
            Config;
        _ ->
            {skip, "No benchmark needed"}
    end.


end_per_suite(Config) ->
    Config.


init_per_testcase(Testcase, Config) ->
    BenchData = init_testcase(Testcase),
    [{benchmark_data, BenchData}|Config].


end_per_testcase(TestCase, _) ->
    terminate_testcase(TestCase),
    ok.


init_testcase(TestCase) when TestCase == ets_add orelse TestCase == posthaste_add ->
    if
        TestCase == ets_add ->
            ?print("Starting server for using ETS"),
            {ok, _} = gen_server:start({local, ?ENAME}, ?MODULE, ?ENAME, []);
        true ->
            ?print("Starting server for using posthaste"),
            {ok, _} = posthaste:start_link(?PNAME)
    end,
    ?print("Server started"),
    ?print("Generating ~p hooks and ~p keys for each hook", [?HOOK_COUNT, ?HOOK_KEY_COUNT]),
    generate_hooks(?HOOK_COUNT, ?HOOK_KEY_COUNT);
init_testcase(TestCase) when TestCase == ets_lookup orelse TestCase == posthaste_lookup ->
    ?print("Regenerating ~p hooks and ~p keys for each hook for lookup", [?HOOK_COUNT, ?HOOK_KEY_COUNT]),
    generate_hooks(?HOOK_COUNT, ?HOOK_KEY_COUNT).


terminate_testcase(_) ->
    ok.


ets_add(Cfg) ->
    add(Cfg, ?ENAME),
    ok.%?print("~p", [ets:tab2list(?ENAME)]).

posthaste_add(Cfg) ->
    add(Cfg, ?PNAME),
    ok.%?print("~p", [erlang:binary_to_term(?PNAME:'$AST'())]).


ets_lookup(Cfg) ->
    lookup(Cfg, ?ENAME, fun(Name, Hook, Key) -> ets:lookup(Name, {Hook, Key}) end).


posthaste_lookup(Cfg) ->
    lookup(Cfg, ?PNAME, fun(Name, Hook, Key) -> Name:Hook(Key) end).

lookup(Cfg, Name, Fun) ->
    BenchData = proplists:get_value(benchmark_data, Cfg),
    Foreach =
        fun(Count) ->
            DoLookup =
                fun() ->
                    do_lookup(Count, BenchData, Name, Fun)
                end,
            erlang:spawn_link(DoLookup)
        end,
    erlang:process_flag(trap_exit, true),
    lists:foreach(Foreach, lists:seq(1,100)),
    receive_exit_signal(100).




do_lookup(Id, BenchData, Name, Fun) ->
    FoldFun =
        fun({Hook, Keys}, Count) ->
            ?print("~p-~p: New process started", [Id, Count]),
            LookupFun =
                fun() ->
                    ForeachFun =
                        fun(Key) ->
                            DoLookupFun =
                                fun(_) ->
                                    Fun(Name, Hook, Key)
                                end,
                            ?print("~p-~p: lookup callbacks of key ~p for ~p times", [Id, Count, Key, ?KEY_LOOKUP_COUNT]),
                            lists:foreach(DoLookupFun, lists:seq(1, ?KEY_LOOKUP_COUNT))
                        end,
                    ?print("~p-~p: lookup callbacks of each key", [Id, Count]),
                    lists:foreach(ForeachFun, Keys),
                    ?print("~p-~p: done, exiting", [Id, Count]),
                    erlang:exit(normal)
                end,
            erlang:spawn_link(LookupFun),
            Count + 1
        end,
    erlang:process_flag(trap_exit, true),
    Count = lists:foldl(FoldFun, 1, BenchData),
    receive_exit_signal(Count-1).


add(Cfg, Name) ->
    BenchData = proplists:get_value(benchmark_data, Cfg),
    FoldFun =
        fun({Hook, Keys}, Count) ->
            ?print("~p: New process started", [Count]),
            AddFun =
                fun() ->
                    ForeachFun =
                        fun(Key) ->
                            ?print("~p: Adding 3 callbacks for Key ~p", [Count, Key]),
                            posthaste_server:add(Name, Hook, Key, module, function, 1),
                            posthaste_server:add(Name, Hook, Key, module, function, 5),
                            posthaste_server:add(Name, Hook, Key, module, function, 2),
                            ?print("~p: Added 3 callbacks for Key ~p", [Count, Key])
                        end,
                    ?print("~p: Adding 3 callbacks for each key", [Count]),
                    lists:foreach(ForeachFun, Keys),
                    ?print("~p: Added 3 callbacks for each key, exiting.", [Count]),
                    erlang:exit(normal)
                end,
            erlang:spawn_link(AddFun),
            Count + 1
        end,
    erlang:process_flag(trap_exit, true),
    Count = lists:foldl(FoldFun, 1, BenchData),
    receive_exit_signal(Count-1).

receive_exit_signal(0) ->
    ok;
receive_exit_signal(Count) ->
    receive
        {'EXIT', _, normal} ->
            receive_exit_signal(Count-1);
        {'EXIT', _, Rsn} ->
            ?print("Exit: ~p", [Rsn]),
            receive_exit_signal(Count-1)

    end.


init(Name) ->
    ets:new(Name, [named_table]),
    {ok, Name}.


handle_call({add, {Hook, Key, Mod, Func, Priority}}, _, Name) ->
    case ets:lookup(Name, {Hook, Key}) of
        [{_, Callbacks}] ->
            Callbacks2 = add_to_callbacks({Priority, {Mod, Func}}, Callbacks, []),
            _ = ets:insert(Name, {{Hook, Key}, Callbacks2}),
            {reply, ok, Name};
        _ ->
            _ = ets:insert(Name, {{Hook, Key}, [{Priority, {Mod, Func}}]}),
            {reply, ok, Name}
    end.


terminate(_, Name) ->
    ets:delete(Name),
    ok.


add_to_callbacks({Int, _}=Callback, [{Int2, _}=Callback2|Callbacks], Buf) ->
    if
        Int2 >= Int ->
            add_to_callbacks(Callback, Callbacks, [Callback2|Buf]);
        true ->
            lists:merge(lists:reverse(Buf), [Callback2, Callback | Callbacks])
    end;
add_to_callbacks(Callback, _, Buf) ->
    lists:reverse([Callback|Buf]).


generate_hooks(HookCount, HookKeyCount) ->
    [{erlang:list_to_atom(erlang:integer_to_list(X))
     ,[Y || Y <- lists:seq(1, HookKeyCount)]}
    || X <- lists:seq(1, HookCount)].