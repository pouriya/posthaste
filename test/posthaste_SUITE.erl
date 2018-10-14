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
        ,'5'/1]).

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
    ?assertEqual({error, {invalid, [{module, ?NAME}]}},  posthaste_code:check_module(?NAME)),
    ?assertMatch({ok, _}, posthaste:start_link(?NAME)),
    ?assertMatch({error, {already_started, _}}, posthaste:start(?NAME)),
    ?assertMatch({error, {already_loaded, _}}, posthaste_code:load(?NAME)),

    ?assertEqual(ok,  posthaste_code:check_module(?NAME)),
    ?assertEqual(ok, gen_server:stop(?NAME)),
    ?assertMatch({error, {_, _}}, posthaste_code:unload(?NAME)),
    ?assertEqual({error, {invalid, [{module, ?NAME}]}},  posthaste_code:check_module(?NAME)),

    ?assertEqual({ok, []}, posthaste_code:callbacks(undefined, undefined, undefined)),
    ?assertMatch({error, {invalid, _}}, posthaste_code:safe_callbacks(undefined, undefined, undefined)).


'2'(_) ->
    ?assertMatch({ok, _}, posthaste:start_link(?NAME)),
    Hook1 = foo,
    Hook1Key1 = bar,
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),
    ?assertEqual({ok, []}, posthaste:safe_callbacks(?NAME, Hook1, Hook1Key1)),
    ?assertEqual({ok, []}, posthaste:hooks(?NAME)),
    ?assertMatch({error, {_, _}}, posthaste:hooks(undefined)),

    ?assertEqual({ok, []}, posthaste:keys(?NAME, Hook1)),
    ?assertMatch({error, {_, _}}, posthaste:keys(undefined, Hook1)),

    ?assertEqual({ok, 0}, posthaste:callback_count(?NAME, Hook1, Hook1Key1)),
    ?assertMatch({error, {_, _}}, posthaste:callback_count(undefined, Hook1, Hook1Key1)),

    Hook1Key1PMF1Priority = 1, % PMF: Priority, Module, Function
    Hook1Key1PMF1Module = module,
    Hook1Key1PMF1Function = function,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF1Module, Hook1Key1PMF1Function, Hook1Key1PMF1Priority)),
    ?assertMatch({error, {_, _}}, posthaste_code:update_handlers(undefined, [])),
    ?assertMatch({error, {_, _}}, posthaste_code:update_handlers(gen_server, [])),
    ?assertMatch({error, {_, _}}, posthaste_code:add_handler(undefined, undefined, undefined, mod, func, 0)),
    ?assertMatch({error, {_, _}}, posthaste_code:delete_handler(gen_server, undefined, undefined, mod, func, 0)),

    ?assertEqual({ok, [Hook1]}, posthaste:hooks(?NAME)),
    ?assertEqual({ok, [Hook1Key1]}, posthaste:keys(?NAME, Hook1)),
    ?assertEqual({ok, 1}, posthaste:callback_count(?NAME, Hook1, Hook1Key1)),
    ?assertEqual({ok, [{Hook1Key1PMF1Priority, {Hook1Key1PMF1Module, Hook1Key1PMF1Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),

    Hook1Key1PMF2Priority = 2,
    Hook1Key1PMF2Module = module2,
    Hook1Key1PMF2Function = function2,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF2Module, Hook1Key1PMF2Function, Hook1Key1PMF2Priority)),
    ?assertEqual({ok, [Hook1]}, posthaste:hooks(?NAME)),
    ?assertEqual({ok, [Hook1Key1]}, posthaste:keys(?NAME, Hook1)),
    ?assertEqual({ok, 2}, posthaste:callback_count(?NAME, Hook1, Hook1Key1)),
    ?assertEqual({ok, [{Hook1Key1PMF1Priority, {Hook1Key1PMF1Module, Hook1Key1PMF1Function}}
                      ,{Hook1Key1PMF2Priority, {Hook1Key1PMF2Module, Hook1Key1PMF2Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),

    Hook1Key1PMF3Priority = 3,
    Hook1Key1PMF3Module = module3,
    Hook1Key1PMF3Function = function3,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF3Module, Hook1Key1PMF3Function, Hook1Key1PMF3Priority)),
    ?assertEqual({ok, [Hook1]}, posthaste:hooks(?NAME)),
    ?assertEqual({ok, [Hook1Key1]}, posthaste:keys(?NAME, Hook1)),
    ?assertEqual({ok, 3}, posthaste:callback_count(?NAME, Hook1, Hook1Key1)),
    ?assertEqual({ok, [{Hook1Key1PMF1Priority, {Hook1Key1PMF1Module, Hook1Key1PMF1Function}}
                      ,{Hook1Key1PMF2Priority, {Hook1Key1PMF2Module, Hook1Key1PMF2Function}}
                      ,{Hook1Key1PMF3Priority, {Hook1Key1PMF3Module, Hook1Key1PMF3Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),

    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF2Module, Hook1Key1PMF2Function, Hook1Key1PMF2Priority)),
    ?assertEqual({ok, [Hook1]}, posthaste:hooks(?NAME)),
    ?assertEqual({ok, [Hook1Key1]}, posthaste:keys(?NAME, Hook1)),
    ?assertEqual({ok, 4}, posthaste:callback_count(?NAME, Hook1, Hook1Key1)),
    ?assertEqual({ok, [{Hook1Key1PMF1Priority, {Hook1Key1PMF1Module, Hook1Key1PMF1Function}}
                      ,{Hook1Key1PMF2Priority, {Hook1Key1PMF2Module, Hook1Key1PMF2Function}}
                      ,{Hook1Key1PMF2Priority, {Hook1Key1PMF2Module, Hook1Key1PMF2Function}}
                      ,{Hook1Key1PMF3Priority, {Hook1Key1PMF3Module, Hook1Key1PMF3Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),

    ?assertEqual(ok, posthaste:delete(?NAME, Hook1, Hook1Key1, Hook1Key1PMF1Module, Hook1Key1PMF1Function, Hook1Key1PMF1Priority)),
    ?assertEqual({ok, [Hook1]}, posthaste:hooks(?NAME)),
    ?assertEqual({ok, [Hook1Key1]}, posthaste:keys(?NAME, Hook1)),
    ?assertEqual({ok, 3}, posthaste:callback_count(?NAME, Hook1, Hook1Key1)),
    ?assertEqual({ok, [{Hook1Key1PMF2Priority, {Hook1Key1PMF2Module, Hook1Key1PMF2Function}}
                      ,{Hook1Key1PMF2Priority, {Hook1Key1PMF2Module, Hook1Key1PMF2Function}}
                      ,{Hook1Key1PMF3Priority, {Hook1Key1PMF3Module, Hook1Key1PMF3Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),

    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF1Module, Hook1Key1PMF1Function, Hook1Key1PMF1Priority)),
    ?assertEqual(ok, posthaste:delete(?NAME, Hook1, Hook1Key1, Hook1Key1PMF2Module, Hook1Key1PMF2Function, Hook1Key1PMF2Priority)),
    ?assertEqual({ok, [Hook1]}, posthaste:hooks(?NAME)),
    ?assertEqual({ok, [Hook1Key1]}, posthaste:keys(?NAME, Hook1)),
    ?assertEqual({ok, 2}, posthaste:callback_count(?NAME, Hook1, Hook1Key1)),
    ?assertEqual({ok, [{Hook1Key1PMF1Priority, {Hook1Key1PMF1Module, Hook1Key1PMF1Function}}
                      ,{Hook1Key1PMF3Priority, {Hook1Key1PMF3Module, Hook1Key1PMF3Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),

    ?assertEqual(ok, posthaste:delete(?NAME, Hook1, Hook1Key1, Hook1Key1PMF3Module, Hook1Key1PMF3Function, Hook1Key1PMF3Priority)),
    ?assertEqual({ok, [Hook1]}, posthaste:hooks(?NAME)),
    ?assertEqual({ok, [Hook1Key1]}, posthaste:keys(?NAME, Hook1)),
    ?assertEqual({ok, 1}, posthaste:callback_count(?NAME, Hook1, Hook1Key1)),
    ?assertEqual({ok, [{Hook1Key1PMF1Priority, {Hook1Key1PMF1Module, Hook1Key1PMF1Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),

    ?assertEqual(ok, posthaste:delete(?NAME, Hook1, Hook1Key1, Hook1Key1PMF1Module, Hook1Key1PMF1Function, Hook1Key1PMF1Priority)),
    ?assertEqual({ok, []}, posthaste:hooks(?NAME)),
    ?assertEqual({ok, []}, posthaste:keys(?NAME, Hook1)),
    ?assertEqual({ok, 0}, posthaste:callback_count(?NAME, Hook1, Hook1Key1)),
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),
    ?assertEqual(ok, gen_server:stop(?NAME)),
    ?assertEqual({error, {invalid, [{module, ?NAME}]}},  posthaste_code:check_module(?NAME)).



'3'(_) ->
    ?assertMatch({ok, _}, posthaste:start_link(?NAME)),
    Hook1 = foo,
    Hook1Key1 = bar,
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),
    Hook1Key1PMF1Priority = 1,
    Hook1Key1PMF1Module = module,
    Hook1Key1PMF1Function = function,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF1Module, Hook1Key1PMF1Function, Hook1Key1PMF1Priority)),

    Hook1Key1PMF2Priority = 2,
    Hook1Key1PMF2Module = module2,
    Hook1Key1PMF2Function = function2,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF2Module, Hook1Key1PMF2Function, Hook1Key1PMF2Priority)),

    Hook1Key1PMF3Priority = 3,
    Hook1Key1PMF3Module = module3,
    Hook1Key1PMF3Function = function3,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF3Module, Hook1Key1PMF3Function, Hook1Key1PMF3Priority)),

    ?assertEqual({ok, [{Hook1Key1PMF1Priority, {Hook1Key1PMF1Module, Hook1Key1PMF1Function}}
                      ,{Hook1Key1PMF2Priority, {Hook1Key1PMF2Module, Hook1Key1PMF2Function}}
                      ,{Hook1Key1PMF3Priority, {Hook1Key1PMF3Module, Hook1Key1PMF3Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),

    Hook1Key2 = baz,
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook1, Hook1Key2)),
    Hook1Key2PMF1Priority = 1,
    Hook1Key2PMF1Module = module,
    Hook1Key2PMF1Function = function,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key2, Hook1Key2PMF1Module, Hook1Key2PMF1Function, Hook1Key2PMF1Priority)),

    Hook1Key2PMF2Priority = 2,
    Hook1Key2PMF2Module = module2,
    Hook1Key2PMF2Function = function2,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key2, Hook1Key2PMF2Module, Hook1Key2PMF2Function, Hook1Key2PMF2Priority)),

    Hook1Key2PMF3Priority = 3,
    Hook1Key2PMF3Module = module3,
    Hook1Key2PMF3Function = function3,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key2, Hook1Key2PMF3Module, Hook1Key2PMF3Function, Hook1Key2PMF3Priority)),

    ?assertEqual({ok, [{Hook1Key2PMF1Priority, {Hook1Key2PMF1Module, Hook1Key2PMF1Function}}
                      ,{Hook1Key2PMF2Priority, {Hook1Key2PMF2Module, Hook1Key2PMF2Function}}
                      ,{Hook1Key2PMF3Priority, {Hook1Key2PMF3Module, Hook1Key2PMF3Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key2)),

    ?assertEqual(ok, posthaste:delete(?NAME, Hook1, Hook1Key1, Hook1Key1PMF1Module, Hook1Key1PMF1Function, Hook1Key1PMF1Priority)),
    ?assertEqual(ok, posthaste:delete(?NAME, Hook1, Hook1Key1, Hook1Key1PMF2Module, Hook1Key1PMF2Function, Hook1Key1PMF2Priority)),
    ?assertEqual(ok, posthaste:delete(?NAME, Hook1, Hook1Key1, Hook1Key1PMF3Module, Hook1Key1PMF3Function, Hook1Key1PMF3Priority)),
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),

    ?assertEqual({ok, [{Hook1Key2PMF1Priority, {Hook1Key2PMF1Module, Hook1Key2PMF1Function}}
                      ,{Hook1Key2PMF2Priority, {Hook1Key2PMF2Module, Hook1Key2PMF2Function}}
                      ,{Hook1Key2PMF3Priority, {Hook1Key2PMF3Module, Hook1Key2PMF3Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key2)),

    ?assertEqual(ok, gen_server:stop(?NAME)),
    ?assertEqual({error, {invalid, [{module, ?NAME}]}},  posthaste_code:check_module(?NAME)).





'4'(_) ->
    ?assertMatch({ok, _}, posthaste:start_link(?NAME)),
    Hook1 = foo,
    Hook1Key1 = bar,
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),
    Hook1Key1PMF1Priority = 1,
    Hook1Key1PMF1Module = module,
    Hook1Key1PMF1Function = function,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF1Module, Hook1Key1PMF1Function, Hook1Key1PMF1Priority)),

    Hook1Key1PMF2Priority = 2,
    Hook1Key1PMF2Module = module2,
    Hook1Key1PMF2Function = function2,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF2Module, Hook1Key1PMF2Function, Hook1Key1PMF2Priority)),

    Hook1Key1PMF3Priority = 3,
    Hook1Key1PMF3Module = module3,
    Hook1Key1PMF3Function = function3,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF3Module, Hook1Key1PMF3Function, Hook1Key1PMF3Priority)),

    ?assertEqual({ok, [{Hook1Key1PMF1Priority, {Hook1Key1PMF1Module, Hook1Key1PMF1Function}}
                      ,{Hook1Key1PMF2Priority, {Hook1Key1PMF2Module, Hook1Key1PMF2Function}}
                      ,{Hook1Key1PMF3Priority, {Hook1Key1PMF3Module, Hook1Key1PMF3Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),

    Hook1Key2 = <<"baz">>,
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook1, Hook1Key2)),
    Hook1Key2PMF1Priority = 1,
    Hook1Key2PMF1Module = module,
    Hook1Key2PMF1Function = function,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key2, Hook1Key2PMF1Module, Hook1Key2PMF1Function, Hook1Key2PMF1Priority)),

    Hook1Key2PMF2Priority = 2,
    Hook1Key2PMF2Module = module2,
    Hook1Key2PMF2Function = function2,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key2, Hook1Key2PMF2Module, Hook1Key2PMF2Function, Hook1Key2PMF2Priority)),

    Hook1Key2PMF3Priority = 3,
    Hook1Key2PMF3Module = module3,
    Hook1Key2PMF3Function = function3,
    ?assertEqual(ok, posthaste:add(?NAME, Hook1, Hook1Key2, Hook1Key2PMF3Module, Hook1Key2PMF3Function, Hook1Key2PMF3Priority)),

    ?assertEqual({ok, [{Hook1Key2PMF1Priority, {Hook1Key2PMF1Module, Hook1Key2PMF1Function}}
                      ,{Hook1Key2PMF2Priority, {Hook1Key2PMF2Module, Hook1Key2PMF2Function}}
                      ,{Hook1Key2PMF3Priority, {Hook1Key2PMF3Module, Hook1Key2PMF3Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key2)),

    Hook2 = qux,
    Hook2Key1 = 12345,
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook2, Hook2Key1)),
    Hook2Key1PMF1Priority = 1,
    Hook2Key1PMF1Module = module,
    Hook2Key1PMF1Function = function,
    ?assertEqual(ok, posthaste:add(?NAME, Hook2, Hook2Key1, Hook2Key1PMF1Module, Hook2Key1PMF1Function, Hook2Key1PMF1Priority)),

    Hook2Key1PMF2Priority = 2,
    Hook2Key1PMF2Module = module2,
    Hook2Key1PMF2Function = function2,
    ?assertEqual(ok, posthaste:add(?NAME, Hook2, Hook2Key1, Hook2Key1PMF2Module, Hook2Key1PMF2Function, Hook2Key1PMF2Priority)),

    Hook2Key1PMF3Priority = 3,
    Hook2Key1PMF3Module = module3,
    Hook2Key1PMF3Function = function3,
    ?assertEqual(ok, posthaste:add(?NAME, Hook2, Hook2Key1, Hook2Key1PMF3Module, Hook2Key1PMF3Function, Hook2Key1PMF3Priority)),

    ?assertEqual({ok, [{Hook2Key1PMF1Priority, {Hook2Key1PMF1Module, Hook2Key1PMF1Function}}
                      ,{Hook2Key1PMF2Priority, {Hook2Key1PMF2Module, Hook2Key1PMF2Function}}
                      ,{Hook2Key1PMF3Priority, {Hook2Key1PMF3Module, Hook2Key1PMF3Function}}]}, posthaste:callbacks(?NAME, Hook2, Hook2Key1)),

    Hook2Key2 = baz,
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook2, Hook2Key2)),
    Hook2Key2PMF1Priority = 1,
    Hook2Key2PMF1Module = module,
    Hook2Key2PMF1Function = function,
    ?assertEqual(ok, posthaste:add(?NAME, Hook2, Hook2Key2, Hook2Key2PMF1Module, Hook2Key2PMF1Function, Hook2Key2PMF1Priority)),

    Hook2Key2PMF2Priority = 2,
    Hook2Key2PMF2Module = module2,
    Hook2Key2PMF2Function = function2,
    ?assertEqual(ok, posthaste:add(?NAME, Hook2, Hook2Key2, Hook2Key2PMF2Module, Hook2Key2PMF2Function, Hook2Key2PMF2Priority)),

    Hook2Key2PMF3Priority = 3,
    Hook2Key2PMF3Module = module3,
    Hook2Key2PMF3Function = function3,
    ?assertEqual(ok, posthaste:add(?NAME, Hook2, Hook2Key2, Hook2Key2PMF3Module, Hook2Key2PMF3Function, Hook2Key2PMF3Priority)),

    ?assertEqual({ok, [{Hook2Key2PMF1Priority, {Hook2Key2PMF1Module, Hook2Key2PMF1Function}}
                      ,{Hook2Key2PMF2Priority, {Hook2Key2PMF2Module, Hook2Key2PMF2Function}}
                      ,{Hook2Key2PMF3Priority, {Hook2Key2PMF3Module, Hook2Key2PMF3Function}}]}, posthaste:callbacks(?NAME, Hook2, Hook2Key2)),

    ?assertEqual(ok, posthaste:delete(?NAME, Hook1, Hook1Key1, Hook1Key1PMF1Module, Hook1Key1PMF1Function, Hook1Key1PMF1Priority)),
    ?assertEqual(ok, posthaste:delete(?NAME, Hook1, Hook1Key1, Hook1Key1PMF2Module, Hook1Key1PMF2Function, Hook1Key1PMF2Priority)),
    ?assertEqual(ok, posthaste:delete(?NAME, Hook1, Hook1Key1, Hook1Key1PMF3Module, Hook1Key1PMF3Function, Hook1Key1PMF3Priority)),
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),


    ?assertEqual({ok, [{Hook2Key2PMF1Priority, {Hook2Key2PMF1Module, Hook2Key2PMF1Function}}
                      ,{Hook2Key2PMF2Priority, {Hook2Key2PMF2Module, Hook2Key2PMF2Function}}
                      ,{Hook2Key2PMF3Priority, {Hook2Key2PMF3Module, Hook2Key2PMF3Function}}]}, posthaste:callbacks(?NAME, Hook2, Hook2Key2)),

    ?assertEqual(ok, posthaste:delete(?NAME, Hook2, Hook2Key1, Hook2Key1PMF1Module, Hook2Key1PMF1Function, Hook2Key1PMF1Priority)),
    ?assertEqual(ok, posthaste:delete(?NAME, Hook2, Hook2Key1, Hook2Key1PMF2Module, Hook2Key1PMF2Function, Hook2Key1PMF2Priority)),
    ?assertEqual(ok, posthaste:delete(?NAME, Hook2, Hook2Key1, Hook2Key1PMF3Module, Hook2Key1PMF3Function, Hook2Key1PMF3Priority)),
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook2, Hook2Key1)),

    ?assertEqual(ok, gen_server:stop(?NAME)),
    ?assertEqual({error, {invalid, [{module, ?NAME}]}},  posthaste_code:check_module(?NAME)).


'5'(_) ->
    ?assertMatch({ok, _}, posthaste:start_link(?NAME)),
    ?assertEqual(ok, posthaste:keep(?NAME)),
    ?assertEqual(error, posthaste:keep(?NAME)),

    Hook1 = foo,
    Hook1Key1 = bar,
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),
    Hook1Key1PMF1Priority = 1,
    Hook1Key1PMF1Module = module,
    Hook1Key1PMF1Function = function,
    ?assertEqual(keep, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF1Module, Hook1Key1PMF1Function, Hook1Key1PMF1Priority)),
    ?assertEqual({ok, []}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),

    ?assertEqual(ok, posthaste:dump(?NAME)),
    ?assertEqual({ok, [{Hook1Key1PMF1Priority, {Hook1Key1PMF1Module, Hook1Key1PMF1Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),

    ?assertEqual(error, posthaste:dump(?NAME)),
    ?assertEqual(ok, posthaste:keep(?NAME)),


    Hook1Key1PMF2Priority = 2,
    Hook1Key1PMF2Module = module2,
    Hook1Key1PMF2Function = function2,
    ?assertEqual(keep, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF2Module, Hook1Key1PMF2Function, Hook1Key1PMF2Priority)),

    ?assertEqual(keep, posthaste:delete(?NAME, Hook1, Hook1Key1, Hook1Key1PMF1Module, Hook1Key1PMF1Function, Hook1Key1PMF1Priority)),

    Hook1Key1PMF3Priority = 3,
    Hook1Key1PMF3Module = module3,
    Hook1Key1PMF3Function = function3,
    ?assertEqual(keep, posthaste:add(?NAME, Hook1, Hook1Key1, Hook1Key1PMF3Module, Hook1Key1PMF3Function, Hook1Key1PMF3Priority)),

    ?assertEqual(ok, posthaste:dump(?NAME)),
    ?assertEqual({ok, [{Hook1Key1PMF2Priority, {Hook1Key1PMF2Module, Hook1Key1PMF2Function}}
                      ,{Hook1Key1PMF3Priority, {Hook1Key1PMF3Module, Hook1Key1PMF3Function}}]}, posthaste:callbacks(?NAME, Hook1, Hook1Key1)),
    ?assertEqual(ok, gen_server:stop(?NAME)),
    ?assertEqual({error, {invalid, [{module, ?NAME}]}},  posthaste_code:check_module(?NAME)).