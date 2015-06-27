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

%%% @doc Unit test for {@link reflib_record}.
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

%%% TODO
%%% Create test case for ?Rec:references()


-module(reftest_reflib_record).
-vsn("$Rev: 4210 $").

-compile([export_all]).

-include("test.hrl").

files() ->
[{module, "rec.erl",
	      "-module(recfield).    \n"
	      "-record(person, {name,age,phone}).\n"
	      "-record(person2, {name,sex,height}).\n"
	      "-export([run/0]).\n"
	      "run() ->\n"
	      "Hilo = #person{name=\"Hilo\", age=24, phone=\"999-9999\"}.\n"
	     }].

test_name() ->
    Rec = ?Query:exec1(?Query:seq(?File:find("rec.erl"),
                                  ?Rec:find(person)), rec_not_found),
    person = ?Rec:name(Rec),
    ok.

test_file() ->
    File = ?Query:exec1(?File:find("rec.erl"), file_not_found),
    Rec = ?Query:exec1(?Query:seq(?File:find("rec.erl"),
                                  ?File:record(person)), rec_not_found),
    File = ?Query:exec1(Rec, ?Rec:file(), file_not_found),
    ok.

test_form() ->
    File = ?Query:exec1(?File:find("rec.erl"), file_not_found),
    Rec = ?Query:exec1(?Query:seq(?File:find("rec.erl"),
                                  ?File:record(person)), rec_not_found),
    Form = ?Query:exec1(Rec, ?Rec:form(), form_not_found),
    File = ?Query:exec1(Form, ?Form:file(), file_not_found),
    ok.

