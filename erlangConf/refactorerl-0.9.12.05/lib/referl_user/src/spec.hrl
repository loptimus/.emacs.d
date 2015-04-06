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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.


-record(type, {kind,
               value = 'any'}).

-record(funsigvalue, {arity,
                      args,
                      retval}).

-record(recvalue, {name,
                   fields}).

-record(recfield, {name,
                   type}).

-record(st_state, {success,
                   type}).

-record(cg_state, {ret_type,
                   conset}).

-define(UNION_MAX_LENGTH, 10).

-define(DEFAULT_RECURSION_LEVEL, 10).

-define(TV(X), {type_variable, X}).

-define(SLIB, refusr_spec_lib).

-define(CS, refusr_spec_cs).

-define(SOL, refusr_spec_sol).

-define(OPTTABLE, refspec_options).
