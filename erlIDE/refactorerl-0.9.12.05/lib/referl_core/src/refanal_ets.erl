%% -*- coding: latin-1 -*-

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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Data flow and message passing based ETS table usage analysis.
%%%
%%% @author Melinda Toth <toth_m@inf.elte.hu>

-module(refanal_ets).
-vsn("$Rev: $ ").

-export([analyse/0]).

-include("core.hrl").
-include_lib("referl_lib/include/lib_export.hrl").

analyse() -> 
    NewFun = ?Query:exec(?Query:seq([?Mod:find(ets), ?Mod:local(new, 2)])),
    NewRefs = ?Query:exec(NewFun, ?Fun:applications()),
    RefList = [{Ref, is_tabref(reach(Ref))--[Ref]} || Ref <- NewRefs],
    NamePs  = ?Query:exec(NewRefs, 
                          ?Query:seq([?Expr:child(2), ?Expr:child(1)])),
    NamePsOrigin = lists:zip(NewRefs, [origin(Name) || Name <- NamePs]),
    NameRefList = 
        [{New, is_tabref(reach(RefR))--[New]} || {New, RefR} <- NamePsOrigin],
    Names = [{New, lists:map(fun refanal_message:atom_value/1, 
                             lists:filter(fun refanal_message:is_atom_expr/1,
                                          Orig))} 
             || {New, Orig} <- NamePsOrigin],
    AtomRefs = [{New, is_tabref(refanal_message:atomnodes(Name)) -- [New]} 
                       %% not so efficient...
                 || {New, Name} <- Names],
    References =  merge(RefList, NameRefList, AtomRefs, Names),
    lists:foreach(fun({N, R, Name}) -> 
                    Node = ?Graph:create(#ets_tab{names=Name}),
                    ?Graph:mklink(Node, ets_def, N),
                    [?Graph:mklink(Node, ets_ref, To) || To <- R]
                  end, References),
    {RefList, NameRefList, AtomRefs, Names, References}.

merge([{N, H1} | L1], [{N, H2} | L2], [{N, H3} | L3], [{N, H4} | L4]) ->
    [ {N, H1++H2++H3, H4}| merge(L1, L2, L3, L4)];
merge(_, _, _, _) ->
    [].

is_tabref(L) ->
    Fun =  fun(N) ->
                   [{_, ArgL}] = ?Syn:parent(N),
                   [{_, App}]  = ?Syn:parent(ArgL),
                   ets_parent(App, ?Syn:class(App))
           end,
    lists:append(lists:map(Fun, L)).

ets_parent(E, expr) ->
    case ?Query:exec(E, ?Query:seq([?Expr:function(), ?Fun:module()])) of 
    %% dynfun???
        []  -> [];
        [M] -> case ?Mod:name(M) of
                   ets -> [E];
                   _   -> []
               end
    end;
ets_parent(_, _) ->
    [].


reach(N) when is_list(N) -> 
    refanal_message:run(fun() -> ?Dataflow:reach(N, [{back, false}]) end);
reach(N) ->
    refanal_message:run(fun() -> ?Dataflow:reach([N], [{back, false}]) end).

origin(N) when is_list(N) -> 
    refanal_message:run(fun() -> ?Dataflow:reach(N, [{back, true}]) end);
origin(N) ->
    refanal_message:run(fun() -> ?Dataflow:reach([N], [{back, true}]) end).

