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

%%% @doc Unit test for {@link reflib_clause}.
%%% @author István Bozó <bozo_i@inf.elte.hu>

-module(reftest_reflib_clause).
-vsn("$Rev: 3948 $").

-compile([export_all]).

-include("test.hrl").

files() ->
    [{module, "test2.erl",
      "-module(test2).\n"
      "-export([fourth/3]).\n"
      "-import(test1, [third/2]).\n"
      "f(X,Y,Z) when true ->\n"
      "  A =X + Y + Z,\n"
      "  length([A, third(Y, Z)]);\n"
      "f(X,Y,Z) ->\n"
      "  ok.\n"}].

clauses()->
    Mod = ?Query:exec1(?Mod:find(test2), test_module),
    ?Query:exec(Mod, ?Query:seq([?Mod:locals(), 
                                 ?Fun:definition(), 
                                 ?Form:clauses()])).
    

test_name() ->
    Clauses = clauses(),
    Names = ?Query:exec(Clauses, ?Clause:name()),
    Clauses = ?Query:exec(Names, ?Expr:nameof()),
    ok.
    
test_patterns() ->
    Clause = lists:nth(1, clauses()),
    Patterns = ?Query:exec(Clause, ?Clause:patterns()),
    [pattern] = lists:usort([?Expr:role(Pattern) || Pattern <- Patterns]),
    [Clause] = lists:usort(?Query:exec(Patterns, ?Expr:clause())),
    ok.

test_pattern() ->
    Clause = lists:nth(1, clauses()),
    Patterns = ?Query:exec(Clause, ?Clause:patterns()),
    {From, To} = 
        case 
            length(Patterns) of
            0 -> {0, 0};
            N -> {1, N}
        end,
    Patterns = [ ?Query:exec1(Clause, ?Clause:pattern(Nth), ambiguous_pattern)
                 || Nth <- lists:seq(From, To)],
    ok.
    
test_guard() ->
    Clause = lists:nth(1, clauses()),
    [Guard] = ?Query:exec(Clause, ?Clause:guard()),
    guard = ?Expr:role(Guard),
    [Clause] = ?Query:exec(Guard, ?Expr:clause()),
    ok.
    
test_body() ->
    Clause = lists:nth(1, clauses()),
    BodyNodes = ?Query:exec(Clause, ?Clause:body()),
    [Clause] = lists:usort(?Query:exec(BodyNodes, ?Expr:clause())),
    ok.

test_bodynth() ->
    Clause = lists:nth(1, clauses()),
    BodyNodes = ?Query:exec(Clause, ?Clause:body()),
    {From, To} = 
        case 
            length(BodyNodes) of
            0 -> {0, 0};
            N -> {1, N}
        end,
    BodyNodes = [ ?Query:exec1(Clause, ?Clause:body(Nth), ambiguous_body_node)
                 || Nth <- lists:seq(From, To)],
    ok.

test_exprs() ->
    Clause = lists:nth(1, clauses()),
    Exprs = ?Query:exec(Clause, ?Clause:exprs()),
    [Clause] = lists:usort(?Query:exec(Exprs, ?Expr:clause())),
    ok.
    
test_expr() ->
    Clause = lists:nth(1, clauses()),
    Exprs = ?Query:exec(Clause, ?Clause:exprs()),
    {From, To} = 
        case 
            length(Exprs) of
            0 -> {0, 0};
            N -> {1, N}
        end,
    Exprs = [ ?Query:exec1(Clause, ?Clause:expr(Nth), ambiguous_expr_node)
                 || Nth <- lists:seq(From, To)],
    ok.

test_form() ->
    File = ?Query:exec1(?File:find("test2.erl"), test_form),
    Form = ?Query:exec1(File, ?File:form(4), form_ambiguous),
    Clause = lists:nth(1, clauses()),
    Form = ?Query:exec1(Clause, ?Clause:form(), form_ambiguous),
    ok.

test_variables() ->
    Clause = lists:nth(1, clauses()),
    Variables = ?Query:exec(Clause, ?Clause:variables()),
    ["X","Y","Z","A"] = [?Var:name(Var) || Var <- Variables],
    ok.

test_variable() ->
    Clause = lists:nth(1, clauses()),
    "X" = ?Var:name(
             ?Query:exec1(Clause, ?Clause:variable("X"), variable_ambiguous)),
    ok.
