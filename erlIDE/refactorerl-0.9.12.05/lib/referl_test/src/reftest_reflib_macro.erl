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

%%% @doc Unit test for {@link reflib_macro}.
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(reftest_reflib_macro).
-vsn("$Rev: 3893 $").

-compile([export_all]).

-include("test.hrl").

files() ->
    [{module, "m1.erl",
      "-module(m1).            \n"
      "-record(r1, {}).        \n"
      "-define(M1, #r1{}).     \n"
      "-record(r2, {}).        \n"
      "-define(M2, ?M1, #r2{}).\n"
      "f() -> ?M2.             \n"
      %% Without the macro application (`f() -> ?M2') the macro and
      %% record cross-references are not analyzed and cannot detected
      %% (macros/0, records/0, references/0)
     }].

test_name() ->
    Mac = ?Query:exec1(?Query:seq(?File:find("m1.erl"),
                                  ?File:macro("M1")), mac_not_found),
    "M1" = ?Macro:name(Mac),
    ok.

test_file() ->
    File = ?Query:exec1(?File:find("m1.erl"), file_not_found),
    Mac = ?Query:exec1(?Query:seq(?File:find("m1.erl"),
                                  ?File:macro("M1")), mac_not_found),
    File = ?Query:exec1(Mac, ?Macro:file(), file_not_found),
    ok.

test_macros() ->
    M1 = ?Query:exec1(?Query:seq(?File:find("m1.erl"),
                                 ?File:macro("M1")), mac_not_found),
    M2 = ?Query:exec1(?Query:seq(?File:find("m1.erl"),
                                 ?File:macro("M2")), mac_not_found),
    []   = ?Query:exec(M1, ?Macro:macros()),
    [M1] = ?Query:exec(M2, ?Macro:macros()),
    ok.

test_records() ->
    M1 = ?Query:exec1(?Query:seq(?File:find("m1.erl"),
                                 ?File:macro("M1")), mac_not_found),
    M2 = ?Query:exec1(?Query:seq(?File:find("m1.erl"),
                                 ?File:macro("M2")), mac_not_found),
    R1 = ?Query:exec1(?Query:seq(?File:find("m1.erl"),
                                 ?File:record(r1)), rec_not_found),
    R2 = ?Query:exec1(?Query:seq(?File:find("m1.erl"),
                                 ?File:record(r2)), rec_not_found),
    [R1]     = ?Query:exec(M1, ?Macro:records()),
    [R1, R2] = ?Query:exec(M2, ?Macro:records()),
    ok.

test_references() ->
    M1 = ?Query:exec1(?Query:seq(?File:find("m1.erl"),
                                 ?File:macro("M1")), mac_not_found),
    M2 = ?Query:exec1(?Query:seq(?File:find("m1.erl"),
                                 ?File:macro("M2")), mac_not_found),
    [M2]   = ?Query:exec(M1, ?Macro:references()),
    [] = ?Query:exec(M2, ?Macro:references()),
    ok.
