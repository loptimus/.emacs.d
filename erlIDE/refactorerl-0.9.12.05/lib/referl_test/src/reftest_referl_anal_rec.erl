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

%%% @doc Unit test for {@link referl_anal_rec}.
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(reftest_referl_anal_rec).
-vsn("$Rev: 5963 $").

-compile([export_all]).

-include("test.hrl").

files() ->
    [{module, "m1.erl",
      "-module(m1).                  \n"
      "-export([f/0]).               \n"
      "-record(r, {a=alma, b}).      \n"
      "f() ->                        \n"
      "  R = #r{a=korte},            \n"
      "  R#r.a.                      \n"
     }].

test_modify_record_expr_name() ->
    F = ?Query:exec1(?File:find("m1.erl"), file_not_found),
    R = ?Query:exec1(F, ?Rec:find(r), rec_not_found),
    [RE, _RA] = ?Query:exec(R, ?Rec:references()),

    Fields = length(?Query:exec(RE, [esub, esub, fieldref])),

    ?Syn:replace(RE, {elex, 2}, ["newrecname"]),
    ?ESG:finalize(),

    [NewR] = ?Query:exec(RE, ?Expr:record()),
    [NewR] = ?Query:exec(F, ?Rec:find(newrecname)),
    Fields = length(?Query:exec(RE, [esub, esub, fieldref])),


    ?Syn:replace(RE, {elex, 2}, ["r"]),
    ?ESG:finalize(),

    [R] = ?Query:exec(RE, ?Expr:record()),
    Fields = length(?Query:exec(RE, [esub, esub, fieldref])),

    ok.

test_modify_record_access_name() ->
    F = ?Query:exec1(?File:find("m1.erl"), file_not_found),
    R = ?Query:exec1(F, ?Rec:find(r), rec_not_found),
    [RE, RA] = ?Query:exec(R, ?Rec:references()),

    Fields = length(?Query:exec(RE, [esub, esub, fieldref])),
    Fields1 = length(?Query:exec(RA, [esub, fieldref])),

    ?Syn:replace(RA, {elex, 2}, ["newrecname"]),
    ?ESG:finalize(),

    [NewR] = ?Query:exec(RA, ?Expr:record()),
    [NewR] = ?Query:exec(F, ?Rec:find(newrecname)),

    Fields = length(?Query:exec(RE, [esub, esub, fieldref])),
    Fields1 = length(?Query:exec(RA, [esub, fieldref])),

    ?Syn:replace(RA, {elex, 2}, ["r"]),
    ?ESG:finalize(),

    Fields = length(?Query:exec(RE, [esub, esub, fieldref])),
    Fields1 = length(?Query:exec(RA, [esub, fieldref])),

    ok.


test_modify_field_expr_name() ->
    F = ?Query:exec1(?File:find("m1.erl"), file_not_found),
    R = ?Query:exec1(F, ?Rec:find(r), rec_not_found),
    [A, B] = ?Query:exec(R, ?Rec:fields()),
    [RE, _RA] = ?Query:exec(R, ?Rec:references()),

    [FieldList] = ?Query:exec(RE, ?Expr:children()),
    [FieldExpr] = ?Query:exec(FieldList, ?Expr:children()),

    [A] = ?Query:exec(FieldExpr, ?Expr:field()),

    ?Syn:replace(FieldExpr, {elex, 1}, ["b"]),
    ?ESG:finalize(),

    [B] = ?Query:exec(FieldExpr, ?Expr:field()),

    ?Syn:replace(FieldExpr, {elex, 1}, ["a"]),
    ?ESG:finalize(),

    [A] = ?Query:exec(FieldExpr, ?Expr:field()),
    ok.

test_modify_field_access_name() ->
    F = ?Query:exec1(?File:find("m1.erl"), file_not_found),
    R = ?Query:exec1(F, ?Rec:find(r), rec_not_found),
    [A, B] = ?Query:exec(R, ?Rec:fields()),
    [_RE, RA] = ?Query:exec(R, ?Rec:references()),
    [_,NameExpr] = ?Query:exec(RA, ?Expr:children()),

    [A] = ?Query:exec(NameExpr, ?Expr:field()),

    ?Syn:replace(NameExpr, {elex, 1}, ["b"]),
    ?ESG:finalize(),

    [B] = ?Query:exec(NameExpr, ?Expr:field()),

    ?Syn:replace(NameExpr, {elex, 1}, ["a"]),
    ?ESG:finalize(),

    [A] = ?Query:exec(NameExpr, ?Expr:field()),
    ok.

test_modify_record_def_name() ->
    F = ?Query:exec1(?File:find("m1.erl"), file_not_found),
    R = ?Query:exec1(F, ?Rec:find(r), rec_not_found),
    Form = ?Query:exec1(R, ?Rec:form(), form_not_found),

    ?Syn:replace(Form, {flex, 4}, ["newrecname"]),
    ?ESG:finalize(),

    [NewR] = ?Query:exec(Form, ?Form:record()),
    [NewR] = ?Query:exec(F, ?Rec:find(newrecname)),

    ?Syn:replace(Form, {flex, 4}, ["r"]),
    ?ESG:finalize(),

    [R] = ?Query:exec(Form, ?Form:record()),

    ok.

test_modify_field_def_name() ->
    F = ?Query:exec1(?File:find("m1.erl"), file_not_found),
    R = ?Query:exec1(F, ?Rec:find(r), rec_not_found),
    [A, B] = ?Query:exec(R, ?Rec:fields()),
    Form = ?Query:exec1(R, ?Rec:form(), form_not_found),

    [First,_] = ?Query:exec(Form, [tattr]),

    ?Syn:replace(First, {tlex, 1}, ["c"]),
    ?ESG:finalize(),

    [A, B, C] = ?Query:exec(R, ?Rec:fields()),

    [C] = ?Query:exec(First, ?Expr:fielddef()),
    [C] = ?Query:exec(R,  ?Rec:field(c)),


    ?Syn:replace(First, {tlex, 1}, ["a"]),
    ?ESG:finalize(),

    [A, B] = ?Query:exec(R, ?Rec:fields()),
    [A] = ?Query:exec(First, ?Expr:fielddef()),
    ok.


test_modify_field_def_name_2() ->
    F = ?Query:exec1(?File:find("m1.erl"), file_not_found),
    R = ?Query:exec1(F, ?Rec:find(r), rec_not_found),
    [A, _B] = ?Query:exec(R, ?Rec:fields()),
    Form = ?Query:exec1(R, ?Rec:form(), form_not_found),

    [_,FieldExpr] = ?Query:exec(Form, [tattr]), %%b

    ?Syn:replace(FieldExpr, {tlex, 1}, ["c"]),
    ?ESG:finalize(),

    [A, C] = ?Query:exec(R, ?Rec:fields()),
    [C] = ?Query:exec(FieldExpr, ?Expr:fielddef()),
    [C] = ?Query:exec(R,  ?Rec:field(c)),

    ?Syn:replace(FieldExpr, {tlex, 1}, ["b"]),
    ?ESG:finalize(),

    [A, NewB] = ?Query:exec(R, ?Rec:fields()),
    [NewB] = ?Query:exec(FieldExpr, ?Expr:fielddef()),

    ok.
