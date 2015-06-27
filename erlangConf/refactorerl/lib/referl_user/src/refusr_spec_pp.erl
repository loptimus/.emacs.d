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

-module(refusr_spec_pp).

-include("user.hrl").

-include("spec.hrl").

-export([print/1]).

print({Func, Funsig}) ->
    %?d({Func, Funsig}),
    FuncName  = reflib_function:name(Func),
    FuncArity = reflib_function:arity(Func),
    L         = separate([type(X) || X <- (Funsig#type.value)#funsigvalue.args], ", "),
    ArgTypes  = lists:append(L),
    RetValue  = type((Funsig#type.value)#funsigvalue.retval),
     lists:flatten(
       lists:concat(
         [FuncName,"/",FuncArity," :: (",ArgTypes,") -> ",RetValue])).



type({'type', 'tuple', 'any'}) ->
    ["tuple()"];
type({'type', 'tuple', Elements}) ->
    ["{"] ++ separate([type(X) || X <- Elements], ", ") ++ ["}"];
type({'type', 'list', 'nil'}) ->
    ["nil()"];
type({'type', 'list', 'any'}) ->
    ["list()"];
type({'type', 'list', V}) ->
    ["["] ++ type(V) ++ ["]"];
type({'type', 'union', Types}) ->
    ["("] ++ separate([type(X) || X <- Types], "|") ++ [")"];
type({'type', 'any', _}) ->
    ["any()"];
type({'type', 'int', 'any'}) ->
    ["integer()"];
type({'type', 'int', N}) ->
    [lists:concat(["integer(", N, ")"])];
type({'type', 'float', 'any'}) ->
    ["float()"];
type({'type', 'float', N}) ->
    [lists:concat(["float(", N, ")"])]; % @todo: nicer float to string
type({'type', 'atom', 'any'}) ->
    ["atom()"];
type({'type', 'atom', Value}) ->
    [lists:concat(["atom(", Value, ")"])];
type({'type', 'string', Value}) ->
    [lists:concat(["string(\"", Value, "\")"])];
type({'type', 'none', 'none'}) ->
    ["none()"];
type(#type{kind=funsig, value=any}) ->
    ["fun(...) -> any()"];
type(Funsig = #type{kind=funsig}) ->
    L         = separate([type(X) || X <- (Funsig#type.value)#funsigvalue.args], ", "),
    ArgTypes  = lists:append(L),
    RetValue  = type((Funsig#type.value)#funsigvalue.retval),
     lists:flatten(
       lists:concat(
         ["(",ArgTypes,") -> ",RetValue]));
type({'type', X, _Y}) ->
    %?d({X,_Y}),
    [atom_to_list(X) ++ "()"];
type([T]) ->
    type(T).% @todo: Ask: list or nil?

separate([], _)            -> [];
separate([H], _)           -> H;
separate([H|T], Separator) -> H ++ [Separator] ++ separate(T, Separator).



