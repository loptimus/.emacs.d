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
%%%
%%% ============================================================================
%%% Module information
%%%
%%% @doc Slicing helper analyser module.
%%%
%%% @author Melinda Toth <tothmelinda@caesar.elte.hu>

-module(refanal_slice).
-vsn("$Rev $ ").
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-include("core.hrl").

%%% @private
schema() ->
    [].

%%% @private
externs(_) -> [].

%%% @private
insert(_Parent, _Pre, {_Tag, Child}, _Post) ->
    add_to_db(?RefChangeTab, expr_parent(Child, ?Anal:data(Child))).

%%% @private
remove(Parent, _Pre, {_Tag, _Child}, _Post) ->
    add_to_db(?RefChangeTab, expr_parent(Parent, ?Anal:data(Parent))).

%%% @private
update(Node, _) ->
    add_to_db(?RefChangeTab, expr_parent(Node, ?Anal:data(Node))).

add_to_db(Tab, Nodes) ->
    dets:open_file(Tab, [{type, set}]),
    [dets:insert(Tab, {E, changed}) || E <- Nodes],
    dets:close(Tab).

expr_parent(Node, #expr{}) ->
    [Node];
expr_parent(_Node, #form{}) ->
    [];
expr_parent(_Node, #file{}) ->
    [];
expr_parent(Node, _) ->
    [{_, Parent}] = ?Syn:parent(Node),
    expr_parent(Parent, ?Anal:data(Parent)).
