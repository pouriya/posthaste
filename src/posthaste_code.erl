%%% ------------------------------------------------------------------------------------------------
%%% "Posthaste" is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2018-2019, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  18.5.19
%% @hidden
%% -------------------------------------------------------------------------------------------------
-module(posthaste_code).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([load/1
        ,unload/1
        ,add_handler/6
        ,delete_handler/6
        ,update_handlers/2
        ,check_module/1
        ,callbacks/3
        ,hooks/1
        ,keys/2
        ,callback_count/3]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("posthaste.hrl").

-define(MODULE_ATTRIBUTE(Mod), attribute(module, [atom(Mod)])).
-define(EXPORT_ALL_ATTRIBUTE, attribute(compile, [atom(export_all)])).
-define(AST_FUNCTION(Funcs)
       ,function(?AST_FUNCTION_NAME
                ,[clause([], [], [binary(erlang:binary_to_list(erlang:term_to_binary(Funcs)))])])).

%% -------------------------------------------------------------------------------------------------
%% API:

load(Mod) when erlang:is_atom(Mod) ->
    case code:is_loaded(Mod) of
        false ->
            Forms = [?MODULE_ATTRIBUTE(Mod), ?EXPORT_ALL_ATTRIBUTE, ?AST_FUNCTION([])],
            {ok, _, Binary} = compile:forms(Forms, [return_errors]),
            {module, _} = code:load_binary(Mod, erlang:atom_to_list(Mod), Binary),
            ok;
        _ -> % {loaded, Mod}
            {error, {already_loaded, [{module, Mod}]}}
    end.


unload(Mod) when erlang:is_atom(Mod) ->
    case check_module(Mod) of
        ok ->
            code:purge(Mod),
            code:delete(Mod),
            ok;
        Err -> % {error, _}
            Err
    end.


check_module(Mod) ->
    case erlang:function_exported(Mod, ?AST_FUNCTION_NAME, 0) of
        true ->
            ok;
        _ -> % false
            {error, {invalid, [{module, Mod}]}}
    end.


add_handler(Mod
           ,Hook
           ,Key
           ,CallbackMod
           ,CallbackFunc
           ,Priority) when erlang:is_atom(Mod) andalso
                           ?is_hook(Hook) andalso
                           ?is_key(Key) andalso
                           erlang:is_atom(CallbackMod) andalso
                           erlang:is_atom(CallbackFunc) andalso
                           ?is_priority(Priority) ->
    update_handler(add, Mod, Hook, Key, CallbackMod, CallbackFunc, Priority).


delete_handler(Mod
              ,Hook
              ,Key
              ,CallbackMod
              ,CallbackFunc
              ,Priority) when erlang:is_atom(Mod) andalso
                              ?is_hook(Hook) andalso
                              ?is_key(Key) andalso
                              erlang:is_atom(CallbackMod) andalso
                              erlang:is_atom(CallbackFunc) andalso
                              ?is_priority(Priority) ->
    update_handler(delete, Mod, Hook, Key, CallbackMod, CallbackFunc, Priority).


callbacks(Mod, Hook, Key) when erlang:is_atom(Mod) andalso ?is_hook(Hook) andalso ?is_key(Key)->
    case check_module(Mod) of
        ok ->
            try
                {ok, Mod:Hook(Key)}
            catch
                _:_ -> % undef
                    {ok, []}
            end;
        Err -> % {error, _}
            Err
    end.


hooks(Mod) when erlang:is_atom(Mod) ->
    case check_module(Mod) of
        ok ->
            Exports = Mod:module_info(exports),
            {ok, [Func || {Func, Arity} <- Exports
                 ,Func /= module_info andalso Func /= ?AST_FUNCTION_NAME andalso Arity == 0]};
        Err -> % {error, _}
            Err
    end.


keys(Mod, Hook) when erlang:is_atom(Mod) andalso ?is_hook(Hook) ->
    case check_module(Mod) of
        ok ->
            try
                {ok, maps:keys(Mod:Hook())}
            catch
                _:_ ->
                    {ok, []}
            end;
        Err ->
            Err
    end.


callback_count(Mod, Hook, Key) when erlang:is_atom(Mod) andalso
                                    ?is_hook(Hook) andalso
                                    ?is_key(Key)->
    case check_module(Mod) of
        ok ->
            try Mod:Hook() of
                Map ->
                    {ok, maps:get(Key, Map, 0)}
            catch
                _:_ ->
                    {ok, 0}
            end;
        Err ->
            Err
    end.


update_handlers(Mod, Updates) when erlang:is_atom(Mod) andalso erlang:is_list(Updates) ->
    case code:is_loaded(Mod) of
        false ->
            {error, {not_loaded, [{module, Mod}]}};
        _ -> % {loaded, Mod}
            case check_module(Mod) of
                ok ->
                    Funcs = update_handlers_2(Updates
                                             ,erlang:binary_to_term(Mod:?AST_FUNCTION_NAME())),
                    Forms = [?MODULE_ATTRIBUTE(Mod)
                            ,?EXPORT_ALL_ATTRIBUTE
                            ,?AST_FUNCTION(Funcs)
                            |Funcs],
                    {ok, _, Binary} = compile:forms(Forms, [return_errors]),
                    {module, _} = code:load_binary(Mod, erlang:atom_to_list(Mod), Binary),
                    ok;
                Err -> % {error, _}
                    Err
            end
    end.

%% -------------------------------------------------------------------------------------------------
%% Internals:

update_handlers_2([{Action, {Hook
                            ,Key
                            ,CallbackMod
                            ,CallbackFunc
                            ,Priority}}|Updates]
                 ,Funcs) when (Action == add orelse Action == delete) andalso
                              ?is_hook(Hook) andalso
                              ?is_key(Key) andalso
                              erlang:is_atom(CallbackMod) andalso
                              erlang:is_atom(CallbackFunc) andalso
                              ?is_priority(Priority) ->
    update_handlers_2(Updates, update_handler_2(Action
                                               ,Hook
                                               ,Key
                                               ,CallbackMod
                                               ,CallbackFunc
                                               ,Priority
                                               ,Funcs));
update_handlers_2([], Funcs) ->
    Funcs.


update_handler(Action, Mod, Hook, Key, CallbackMod, CallbackFunc, Priority) ->
    case code:is_loaded(Mod) of
        false ->
            {error, {not_loaded, [{module, Mod}]}};
        _ -> % {loaded, Mod}
            case check_module(Mod) of
                ok ->
                    Funcs = update_handler_2(Action
                                            ,Hook
                                            ,Key
                                            ,CallbackMod
                                            ,CallbackFunc
                                            ,Priority
                                            ,erlang:binary_to_term(Mod:?AST_FUNCTION_NAME())),
                    Forms2 = [?MODULE_ATTRIBUTE(Mod)
                             ,?EXPORT_ALL_ATTRIBUTE
                             ,?AST_FUNCTION(Funcs)
                             |Funcs],
                    {ok, _, Binary} = compile:forms(Forms2, [return_errors]),
                    {module, _} = code:load_binary(Mod, erlang:atom_to_list(Mod), Binary),
                    ok;
                Err -> % {error, _}
                    Err
            end
    end.


update_handler_2(add=Action, Hook, Key, CallbackMod, CallbackFunc, Priority, Funcs) ->
    {InfoFunc, HandlersFunc, Funcs2} =
        case take_functions(Hook, Funcs, []) of
            {ok, {InfoFunc2, HandlersFunc2, Funcs3}} ->
                {HandlersFunc3, Count} = update_handlers_function_clause(Action
                                                                        ,Key
                                                                        ,CallbackMod
                                                                        ,CallbackFunc
                                                                        ,Priority
                                                                        ,HandlersFunc2),
                {update_info_function(Action, Key, InfoFunc2, Count), HandlersFunc3, Funcs3};
            _ -> % {error, not_found}
                {make_info_function(Hook, Key)
                ,make_handlers_function(Hook, Key, CallbackMod, CallbackFunc, Priority)
                ,Funcs}
        end,
    [InfoFunc, HandlersFunc|Funcs2];
update_handler_2(Action, Hook, Key, CallbackMod, CallbackFunc, Priority, Funcs) -> % (delete, ...)
    case take_functions(Hook, Funcs, []) of
        {ok, {InfoFunc, HandlersFunc, Funcs2}} ->
            {HandlersFunc2, Count} = update_handlers_function_clause(Action
                                                                    ,Key
                                                                    ,CallbackMod
                                                                    ,CallbackFunc
                                                                    ,Priority
                                                                    ,HandlersFunc),
            case erlang:length(erl_syntax:function_clauses(HandlersFunc2)) of
                1 -> % (_)
                    Funcs2;
                _ ->
                    InfoFunc2 = update_info_function(Action, Key, InfoFunc, Count),
                    [InfoFunc2, HandlersFunc2|Funcs2]
            end;
        _ -> % {error, not_found}
            Funcs
    end.


make_info_function(Name, Key) ->
    Func = function(Name, [clause([], [], [map([])])]),
    update_info_function(add, Key, Func, 1).


update_info_function(Action, Key, Func, Count) ->
    [Map] = erl_syntax:clause_body(erlang:hd(erl_syntax:function_clauses(Func))),
    Map2 =
        case take_from_map(Key, Map) of
            {ok, {Val, Map3}} ->
                Val2 = Val + Count,
                if
                    Val2 > 0 ->
                        add_to_map(Key, Val + Count, Map3);
                    true ->
                        Map3
                end;
            _ -> % {error, not_found}
                if
                    Action == add ->
                        add_to_map(Key, 1, Map);
                    true ->
                        Map
                end
        end,
    function(erl_syntax:function_name(Func), [clause([], [], [Map2])]).


take_functions(FuncName, [Func, Func2| Funcs], Buf) ->
    case {erl_syntax:atom_value(erl_syntax:function_name(Func))
         ,erl_syntax:atom_value(erl_syntax:function_name(Func))} of
        {FuncName, FuncName} -> % FuncName == FuncName == (FuncName in Arguments)
            {ok, {Func, Func2, lists:merge(lists:reverse(Buf), Funcs)}};
        _ -> % {_, _}
            take_functions(FuncName, [Func2 | Funcs], [Func|Buf])
    end;
take_functions(_, _, _) ->
    {error, not_found}.


add_to_map(Key, Val, Map) ->
    Fields = [abstract(map_field_assoc, [erl_parse:abstract(Key), erl_parse:abstract(Val)])
             |erl_syntax:map_expr_fields(Map)],
    abstract(map_expr, [Fields]).


take_from_map(Key, Map) ->
    take_from_map(Key, erl_syntax:map_expr_fields(Map), []).


take_from_map(Key, [Field|Fields], Buf) ->
    FieldAbstract = erl_syntax:map_field_assoc_name(Field),
    KeyAbstract = erl_parse:abstract(Key),
    if
        KeyAbstract =:= FieldAbstract ->
            Val = erl_syntax:integer_value(erl_syntax:map_field_assoc_value(Field)),
            Map2 = abstract(map_expr, [lists:merge(Buf, Fields)]),
            {ok, {Val, Map2}};
        true ->
            take_from_map(Key, Fields, [Field|Buf])
    end;
take_from_map(_, _, _) -> % (_, [], _)
    {error, not_found}.


make_handlers_function(FuncName, Arg, CallbackMod, CallbackFunc, Priority) ->
    Func = function(FuncName, [clause([underscore()], [], [list([])])]),
    erlang:element(1, update_handlers_function_clause(add
                                                     ,Arg
                                                     ,CallbackMod
                                                     ,CallbackFunc
                                                     ,Priority
                                                     ,Func)).


update_handlers_function_clause(Action, Arg, CallbackMod, CallbackFunc, Priority, Func) ->
    Arg2 = erl_parse:abstract(Arg),
    Clauses = erl_syntax:function_clauses(Func),
    {Clauses2, Count} =
        case take_function_clause(Arg2, Clauses, []) of
            {ok, {Clause, Clauses3}} ->
                {Body, Count2} =
                    if
                        Action =:= add ->
                            {add_to_list(CallbackMod
                                        ,CallbackFunc
                                        ,Priority
                                        ,erlang:hd(erl_syntax:clause_body(Clause)))
                            ,1};
                        true ->
                            delete_from_list(CallbackMod
                                            ,CallbackFunc
                                            ,Priority
                                            ,erlang:hd(erl_syntax:clause_body(Clause)))
                    end,
                case erl_syntax:type(Body) of
                    nil ->
                        {Clauses3, Count2};
                    _ ->
                        {[clause([Arg2], [], [Body])|Clauses3], Count2}
                end;
            _ -> % {error, not_found}
                if
                    Action =:= add ->
                        Body = add_to_list(CallbackMod, CallbackFunc, Priority, list([])),
                        {[clause([Arg2], [], [Body])|Clauses], 1};
                    true -> % delete
                        {Clauses, 0}
                end
        end,
    {function(erl_syntax:function_name(Func), Clauses2), Count}.




take_function_clause(Arg, [Clause|Clauses], Buf) ->
    case {argument_value(hd(erl_syntax:clause_patterns(Clause))), argument_value(Arg)} of
        {X, X} -> % X == X
            {ok, {Clause, lists:merge(lists:reverse(Buf), Clauses)}};
        _ ->
            take_function_clause(Arg, Clauses, [Clause|Buf])
    end;
take_function_clause(_, _, _) -> % (_, [], _)
    {error, not_found}.


argument_value(Arg) ->
    case erl_syntax:type(Arg) of
        underscore ->
            underscore;
        atom ->
            erl_syntax:atom_value(Arg);
        binary ->
            erl_syntax:binary_field(Arg);
        integer ->
            erl_syntax:integer_value(Arg)
    end.


add_to_list(Mod, Func, Priority, Body) ->
    Elems =
        case erl_syntax:type(Body) of
            nil ->
                [];
            _ -> % list
                erl_syntax:list_elements(Body)
        end,
    list(add_to_list(Mod, Func, Priority, Elems, [])).


add_to_list(Mod, Func, Priority, [Elem|Elems], Buf) ->
    Int = erl_syntax:integer_value(erlang:hd(erl_syntax:tuple_elements(Elem))),
    if
        Int =< Priority ->
            add_to_list(Mod, Func, Priority, Elems, [Elem|Buf]);
        true ->
            Elem2 = tuple([integer(Priority), tuple([atom(Mod), atom(Func)])]),
            lists:merge(lists:reverse(Buf), [Elem2, Elem | Elems])
    end;
add_to_list(Mod, Func, Priority, _, Buf) -> % (_, _, _, [], _)
    Elem = tuple([integer(Priority), tuple([atom(Mod), atom(Func)])]),
    lists:reverse([Elem|Buf]).

delete_from_list(Mod, Func, Priority, Body) ->
    Elems =
        case erl_syntax:type(Body) of
            nil ->
                [];
            _ -> % list
                erl_syntax:list_elements(Body)
        end,
    {List, Count} = delete_from_list(Mod, Func, Priority, Elems, [], 0),
    {list(List), Count}.

delete_from_list(Mod, Func, Priority, [Elem|Elems], Buf, Count) ->
    Int = erl_syntax:integer_value(erlang:hd(erl_syntax:tuple_elements(Elem))),
    if
        Int =:= Priority ->
            [ElemMod
            ,ElemFunc] = erl_syntax:tuple_elements(lists:last(erl_syntax:tuple_elements(Elem))),
            case {erl_syntax:atom_value(ElemMod), erl_syntax:atom_value(ElemFunc)} of
                {Mod, Func} -> % Mod == (Mod in arguments) and Func == (Func in arguments)
                    delete_from_list(Mod, Func, Priority, Elems, Buf, Count-1);
                _ -> % {_, _}
                    delete_from_list(Mod, Func, Priority, Elems, [Elem|Buf], Count)
            end;
        true ->
            delete_from_list(Mod, Func, Priority, Elems, [Elem|Buf], Count)
    end;
delete_from_list(_, _, _, _, Buf, Count) -> % (_, _, _, [], _, _)
    {lists:reverse(Buf), Count}.


function(Name, Clauses) when erlang:is_atom(Name) andalso erlang:is_list(Clauses) ->
    abstract(function, [atom(Name), Clauses]);
function(Name, Clauses) when erlang:is_list(Clauses) ->
    abstract(function, [Name, Clauses]).


clause(Args, Guards, Bodies) when erlang:is_list(Args) andalso
                                  (erlang:is_list(Guards) orelse Guards =:= none) andalso
                                  erlang:is_list(Bodies) ->
    abstract(clause, [Args, Guards, Bodies]).


atom(Name) when erlang:is_atom(Name) ->
    abstract(atom, [Name]).


tuple(Elements) when erlang:is_list(Elements) ->
    abstract(tuple, [Elements]).


attribute(Name, Args) when erlang:is_atom(Name) andalso erlang:is_list(Args) ->
    abstract(attribute, [atom(Name), Args]).


map([]) ->
    abstract(map_expr, [[]]).


list([]) ->
    abstract(nil, []);
list(List) when erlang:is_list(List) ->
    abstract(list, [List]).


integer(Int) when erlang:is_integer(Int) ->
    abstract(integer, [Int]).


string(Str) when erlang:is_list(Str) ->
    abstract(string, [Str]).


binary(Str) when erlang:is_list(Str) ->
    abstract(binary, [[abstract(binary_field, [string(Str)])]]).


underscore() ->
    abstract(underscore, []).


abstract(Type, Args) ->
    MaybeTree = erlang:apply(erl_syntax, Type, Args),
    case erl_syntax:is_tree(MaybeTree) of
        true ->
            erl_syntax:revert(MaybeTree);
        _ -> % false
            MaybeTree
    end.