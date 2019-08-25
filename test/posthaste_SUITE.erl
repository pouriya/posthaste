-module(posthaste_SUITE).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% ct callbacks:
-export([init_per_suite/1
        ,end_per_suite/1
        ,all/0
        ,init_per_testcase/2
        ,end_per_testcase/2]).

-export(['1'/1
        ,'2'/1
        ,'3'/1
        ,'4'/1
        ,'5'/1
        ,'6'/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NAME, posthaste_hook).


all() ->
    [erlang:list_to_atom(erlang:integer_to_list(Int))
        || Int <- lists:seq(1, erlang:length(?MODULE:module_info(exports))-8)].


init_per_suite(Config) ->
    application:start(sasl),
    Config.


end_per_suite(Config) ->
    application:stop(sasl),
    Config.


init_per_testcase(_, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    ok.



'1'(_) ->
    ?assertMatch(false, erlang:function_exported(?NAME, module_info, 0)),
    ?assertMatch({ok, _}, posthaste:start_link(?NAME)),
    ?assertMatch(true, erlang:function_exported(?NAME, module_info, 0)),
    ?assertMatch(ok, gen_server:stop(?NAME)),
    ?assertMatch(false, erlang:function_exported(?NAME, module_info, 0)),
    ok.


'2'(_) ->
    ?assertMatch([], posthaste:all(?NAME)),
    ?assertMatch([], posthaste:keys(?NAME, hook)),
    ?assertMatch({ok, _}, posthaste:start_link(?NAME)),
    ?assertMatch([], posthaste:all(?NAME)),
    ?assertMatch([], posthaste:keys(?NAME, hook)),
    ?assertMatch(ok, gen_server:stop(?NAME)),
    ?assertMatch([], posthaste:all(?NAME)),
    ?assertMatch([], posthaste:keys(?NAME, hook)),
    ok.


'3'(_) ->
    ?assertMatch({ok, _}, posthaste:start_link(?NAME)),
    ?assertMatch(ok, posthaste:add(?NAME, hook, key, mod, func, 2)),
    ?assertMatch({error, {already_exists, _}}, posthaste:add(?NAME, hook, key, mod, func, 2)),

    ?assertMatch([{hook, [{key, [{2, mod, func}]}]}], posthaste:all(?NAME)),
    ?assertMatch([key], posthaste:keys(?NAME, hook)),


    ok.



'4'(_) ->
    ok.



'5'(_) ->
    ok.


'6'(_) ->
    ok.
