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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2010,
%%% Eötvös Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information


-module(reftest_ets).
-vsn("$Rev: 5069 $ ").

%% Random module based testing callbacks
-export([prepare/1, perform_and_check/2]).

%% Interface
-export([]).

-include("test.hrl").

%% @doc Prepare function for random module based testing.
prepare(Mods) -> [Mods].

%% @doc "Perform and check" function for random module based testing.
perform_and_check(_ModsOrFiles, _Args) ->
    refanal_ets:analyse(),
    check_news(),
    check_inserts(),
    check_lookups(),
    check_ets_objects().

check_news() ->
    Refs = find_ets_calls(new, 2),
    io:format("Checking analysis of ets:new/2 calls... (~p)~n", [length(Refs)]),
    lists:all(
      fun(Ref) ->
              %% There should be exactly one ETS table being defined
              [ETS] = ?Graph:path(Ref, [{ets_def, back}]),
              check_referred_ets(ETS)
      end,
      Refs).

check_inserts() ->
    Refs = find_ets_calls(insert, 2),
    io:format("Checking analysis of ets:insert/2 calls... (~p)~n", [length(Refs)]),
    lists:all(
      fun(Ref) ->
              %% There should be exactly one ETS table being referred to
              [ETS] = ?Graph:path(Ref, [{ets_ref, back}]),
              check_referred_ets(ETS)
      end,
      Refs).

check_lookups() ->
    Refs = find_ets_calls(lookup, 2),
    io:format("Checking analysis of ets:lookup/2 calls... (~p)~n", [length(Refs)]),
    lists:all(
      fun(Ref) ->
              %% There should be exactly one ETS table being referred to
              [ETS] = ?Graph:path(Ref, [{ets_ref, back}]),
              check_referred_ets(ETS)
      end,
      Refs).

check_ets_objects() ->
    AllETS = lists:usort(
               ?Query:exec(?Query:seq([?Mod:all(),
                                       ?Mod:locals(),
                                       ?Fun:definition(),
                                       ?Form:clauses(),
                                       ?Clause:body(),
                                       ?Expr:deep_sub(),
                                       ?Query:any([{ets_def, back}],
                                                  [{ets_ref, back}])]))),
    io:format("Checking ETS object referers... (~p tables)~n", [length(AllETS)]),

    %% Checks whether all ETS referers are application expressions
    lists:all(
      fun(ETS) ->
              #ets_tab{names=Ns} = ?Graph:data(ETS),
              io:format("\tChecking object named ~p~n", [Ns]),
              lists:all(
                fun(Expr) -> ?Expr:type(Expr) == application end,
                ?Query:exec(ETS, ?Query:any([ets_def], [ets_ref])))
      end,
      AllETS).

check_referred_ets(ETS) ->
    %% Its data is an ets_tab record
    #ets_tab{names = Ns} = ?Graph:data(ETS),
    %% Field `names' contains at least one element,which is of the
    %% type `atom'
    is_list(Ns) andalso is_atom(hd(Ns)).

find_ets_calls(Name, Arity) ->
    ?Query:exec(?Query:seq([?Mod:find(ets),
                            ?Mod:local(Name, Arity),
                            ?Fun:applications()])).
