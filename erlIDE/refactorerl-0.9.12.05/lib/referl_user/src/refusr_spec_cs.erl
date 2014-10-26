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

%%% ============================================================================
%%% Module information

%%% @doc

%%% == Implementation status ==
%%% This feature is _not_ fully implemented.

%%% @author Gabor Olah <olikas.g@gmail.com>
-module(refusr_spec_cs).

-vsn("$Rev: $"). %for emacs"

-include("user.hrl").

-include("spec.hrl").

-export([new/0, new/1, add/3, concat/2]).


new() ->
    {conj, ordsets:new()}.

new(OldSet) ->
    OldSet.



add(Set1, Way, Set2 = {_Way2, _Set2}) ->
    add(Set1, Way, [Set2]);
add({conj, OldSet}, conj, Cons) when is_list(Cons) ->
    {conj, ordsets:union(OldSet, ordsets:from_list(Cons))};
add({conj, OldSet}, disj, Cons) when is_list(Cons) ->
    {conj,
     ordsets:add_element({disj,
                          ordsets:from_list(Cons)}, OldSet)};
add({disj, _OldSet} = Cs, conj, Cons) when is_list(Cons) ->
    {conj,
     ordsets:add_element(Cs,
                         ordsets:from_list(Cons))};
add({disj, OldSet}, disj, Cons) when is_list(Cons) ->
    {disj, ordsets:union(ordsets:from_list(Cons), OldSet)}.



%% add({conj, OldSet}, conj, Cons) when is_list(Cons) ->
%%     {conj, ordsets:union(OldSet, ordsets:from_list(Cons))};
%% add({conj, OldSet}, conj, {Way, Set}) ->
%%     add({conj, OldSet}, conj, Cons);

%% add({conj, OldSet}, disj, Cons) when is_list(Cons) ->
%%     {conj,
%%      ordsets:add_element({disj,
%%                           ordsets:from_list(Cons)}, OldSet)};
%% add({conj, OldSet}, disj, Cons) ->
%%     add({conj, OldSet}, disj, [Cons]);



%% add({disj, _OldSet} = Cs, conj, Cons) when is_list(Cons) ->
%%     {conj,
%%      ordsets:add_element(Cs,
%%                          ordsets:from_list(Cons))};
%% add({disj, OldSet}, conj, Cons) ->
%%     add({disj, OldSet}, conj, [Cons]);

%% add({disj, OldSet}, disj, Cons) when is_list(Cons) ->
%%     {disj, ordsets:union(ordsets:from_list(Cons), OldSet)};
%% add({disj, OldSet}, disj, Cons) ->
%%     add({disj, OldSet}, disj, [Cons]).


concat([], _Way) when (_Way =:= conj) ; (_Way =:= disj) ->
    new();
concat([CS], _Way) when (_Way =:= conj) ; (_Way =:= disj) ->
    CS;
concat(Consets, Way) when is_list(Consets) and
                          ((Way =:= conj) or (Way =:= disj)) ->
    Fun = fun(C, Acc) -> ?CS:add(Acc, Way, C) end,
    lists:foldl(Fun, ?CS:new(), Consets).





