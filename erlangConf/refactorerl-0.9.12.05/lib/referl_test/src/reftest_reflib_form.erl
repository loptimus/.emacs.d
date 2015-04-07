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

%%% @doc Unit test for {@link reflib_form}.
%%% @author István Bozó <bozo_i@inf.elte.hu>

-module(reftest_reflib_form).
-vsn("$Rev: 4236 $").

-compile([export_all]).

-include("test.hrl").

files() ->
    [{module, "test2.erl",
      "-module(test2).\n"
      "-export([fourth/3]).\n"
      "-define(Macro,macro).\n"
      "-import(test1, [third/2]).\n"
      "fourth(X,Y,Z) when true->\n"
      "  length([X, third(Y, Z)]);\n"
      "fourth(X,Y,Z) ->\n"
      "  ok.\n"
      "fifth() ->\n"
      "  ?Macro,\n"
      "  test1:first().\n"
      "sixth(I) when integer(I) ->\n"
      "  integer_to_list(I).\n"
      "seventh() -> \n"
      "  fun sixth/1.\n"}].

data()->
    Mod = ?Query:exec1(?Mod:find(test2), test_module),
    Forms = ?Query:exec(Mod, ?Query:seq(?Mod:file(), ?File:forms())),
    {Mod, Forms}.

test_type() ->
    {_, Forms} = data(),
    [module, export, macro, import, func, func, func, func] =
        [ ?Form:type(Form) || Form <- Forms],
    ok.

test_module() ->
    {Mod, Forms} = data(),
    [ Mod = ?Query:exec1(Form, ?Form:module(), not_unique_mod)||Form <- Forms],
    ok.

test_func() ->
    {_, Forms} = data(),
    [[],[],[],[] | Funs] =
        [ ?Query:exec(Form, ?Form:func()) || Form <- Forms],
    [{fourth,3}, {fifth, 0}, {sixth, 1}, {seventh, 0}] =
        [ {?Fun:name(Func), ?Fun:arity(Func)} || [Func] <- Funs],
    
    ok.

test_clauses() ->
    {_, Forms} = data(),
    F = lists:sublist(Forms, 4, 3),
    [0,2,1] =
        [ length(?Query:exec(Form, ?Form:clauses())) || Form <- F],
    ok.
    
test_clause() ->
    {_, Forms} = data(),
    F = lists:sublist(Forms, 4, 3),
    [[],[_C],[]] =
        [ ?Query:exec(Form, ?Form:clause(2)) || Form <- F],
    [[],[_C1],[_C2]] =
        [ ?Query:exec(Form, ?Form:clause(1)) || Form <- F],
    
    ok.
    
test_file() ->
    [File] = ?Query:exec(?File:find("test2.erl")),
    {_, Forms} = data(),
    [File = ?Query:exec1(Form, ?Form:file(), not_unique_file) || Form <- Forms],
    ok.

test_macros() ->
    {_Mod, Forms} = data(),
    [Macro] = lists:flatten(?Query:exec(Forms, ?Form:macros())),
    "Macro" = ?Macro:name(Macro),
    ok.

test_exprs() ->
    {_, Forms} = data(),
    [0,1,0,2,0,0,0,0] =
        [length(?Query:exec(Form, ?Form:exprs())) || Form <- Forms],
    ok.
