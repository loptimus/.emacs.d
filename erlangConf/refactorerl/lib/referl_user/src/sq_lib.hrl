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
%%% The Initial Developer of the  Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s  Lor�nd University are  Copyright 2009,
%%% E�tv�s Lor�nd University. All Rights Reserved.

-record(entity, {name, selectors, properties}).
-record(selector, {name, type, func, desc=" "}).
-record(property, {name, type, func, desc=" "}).

-record(initial_selector, {name, type, func, desc=" "}).
-record(statistics, {name, func, desc=" "}).

%% The record `state' stores the data of a single step in the semantic query.
%% `action' is the type of the current action. The action can be:
%%   selection
%%   closure
%%   iteration
%%   property_query
%%   statistics
%% `type', `res' and `prev_type', `prev_res' contains the data of the
%% current and the previous result.
-record(state, {action=selection, type, res, prev_type=[], prev_res=[]}).
-record(chains, {complete=[], incomplete=[], recursive=[]}).
