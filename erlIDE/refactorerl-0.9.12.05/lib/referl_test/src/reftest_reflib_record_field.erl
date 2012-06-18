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

%%% @doc Unit test for {@link reflib_record_field}.
%%% @author Gabor Czini <gczini@gmail.com>

-module(reftest_reflib_record_field).
-vsn("$Rev: 4139 $").

-compile([export_all]).

-include("test.hrl").

files() ->
    [{module, "recfield.erl",
      "-module(recfield).    \n"
      "-record(person, {name,age,phone}).\n"
      "-record(person2, {name,sex,height}).\n"
      "-export([run/0]).\n"
      "run() ->\n"
      "Hilo = #person{name=\"Hilo\", age=24, phone=\"999-9999\"}.\n"
     }].

test_file() ->
    File  = ?Query:exec1(?File:find("recfield.erl"), file_not_found),
    Rec   = ?Query:exec1(?Query:seq(?File:find("recfield.erl"),
                                  ?File:record(person)), rec_not_found),
    Field = ?Query:exec(Rec, ?Rec:field(age)),
    File  = ?Query:exec1(Field, ?RecField:file(), file_not_found),
    ok.

test_form() ->
    File = ?Query:exec1(?File:find("recfield.erl"), file_not_found),
    Rec  = ?Query:exec1(?Query:seq(?File:find("recfield.erl"),
                                  ?File:record(person)), rec_not_found),
    Form = ?Query:exec1(Rec, ?Rec:form(), form_not_found),
    File = ?Query:exec1(Form, ?Form:file(), file_not_found),
    ok.   

test_names() ->
    Rec = ?Query:exec1(?Query:seq(?File:find("recfield.erl"),
                                  ?File:record(person)), rec_not_found),
    FieldNames = [?RecField:name(Name) || Name <- ?Query:exec(Rec, ?Rec:fields()) ],
    [name,age,phone] = FieldNames,
    ok.

test_def() ->
    Rec = ?Query:exec1(?Query:seq(?File:find("recfield.erl"),
                                  ?File:record(person)), rec_not_found),
    Field = ?Query:exec(Rec, ?Rec:field(name)),
    Rec = ?Query:exec1(Field, ?RecField:recorddef(),rec_not_found),
    ok.

test_ref() ->
    Field = ?Query:exec1(?Query:seq([?File:find("recfield.erl"),
                                  ?File:record(person), ?Rec:field(age)]), field_not_found),
    FieldRef = ?Query:exec1(Field, ?RecField:references(), no_field_expr),
    Field = ?Query:exec1(FieldRef, ?Expr:field(), no_field),
    ok.
