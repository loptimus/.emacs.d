%%% -*- coding: latin-1 -*-

%%% The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1, (the  "License");  you may  not  use this  file except  in
%%% compliance  with the License.  You should  have received  a copy  of the
%%% Erlang  Public License  along  with this  software.  If not,  it can  be
%%% retrieved at http://plc.inf.elte.hu/erlang/
%%%
%%% Software  distributed under  the License  is distributed  on an  "AS IS"
%%% basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
%%% the License  for the specific language governing  rights and limitations
%%% under the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the  Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc This module tests the {@link referl_rename_fun} module.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>
%%%
%%% @todo Does not work.
%%% referl_rename_fun:rename_function should be reftr_rename_fun:do.

-module(reftest_referl_rename_fun).
-vsn("$Rev: 5585 $").
-include("test.hrl").

-export([test_rename_fun/4]).

%%% @type data_checker_fun() = ((data(), data()) -> (true | {false, term()})).
%%%
%%% Represents a function that checks two nodes in two graphs and returns
%%% whether they are correct or not.

%% @spec test_rename_fun(atom(), atom(), integer(), atom()) ->
%%           true | {false, term()}
%%
%% @doc Tests whether the given "rename function" refactoring is executed
%% correctly.
%%
%% @todo Now there are two possible implementations that conforms this property:
%% the identity and the correct implementation. The former could be ruled out by
%% adding a property that says: the function name in the semantic function node
%% is changed from OldFunName to NewFunName.
test_rename_fun(ModName, OldFunName, Arity, NewFunName) ->

    %% creating graphs
    E1 = reflib_egraph:create_egraph(),
    referl_rename_fun:rename_function(ModName, OldFunName, Arity, NewFunName),
    E2 = reflib_egraph:create_egraph(),

    %% testing and deleting graphs
    TestResult = reftest_utils:test_graphs(
                   E1, E2, id_except(OldFunName, Arity, NewFunName)),
    ets:delete(E1),
    ets:delete(E2),
    TestResult.

%% @spec id_except(atom(), integer(), atom()) -> data_checker_fun()
%%
%% @doc Check whether the two given data are the same or the second one is the
%% renamed version of the first one.
id_except(OldFunName, Arity, NewFunName) ->
    fun (Data1, Data2) ->
        case {Data1, Data2} of
            {X, X} ->
                true;
            {#expr{role = expr, type = atom, value = OldFunName},
             #expr{role = expr, type = atom, value = NewFunName}} ->
                true;
            {#lex{type = token, data = T1 = #token{type = atom}},
             #lex{type = token, data = T2 = #token{type = atom}}} ->
                ?Token:get_value(T1) == OldFunName andalso
                ?Token:get_value(T2) == NewFunName;
            {#func{%%type = global,
                   name = OldFunName, arity = Arity},
             #func{%%type = global,
                   name = NewFunName, arity = Arity}} ->
                true;
            _ ->
                {false,
                 {id_except,"Data1 and Data2 are different",Data1,Data2}}
        end
    end.

