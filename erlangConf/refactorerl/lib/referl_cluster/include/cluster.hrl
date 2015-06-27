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
%%% Portions created  by E�tv�s  Lor�nd University are  Copyright 2008-2009,
%%% E�tv�s Lor�nd University. All Rights Reserved.

-include_lib("referl_gen/include/gen_export.hrl").
-include_lib("referl_core/include/core_global.hrl").
-include_lib("referl_lib/include/lib_export.hrl").

-include_lib("referl_core/include/core_export.hrl").


%% Records for the attribute matrix of clustering
-record(rec_attr,{file,name}).
-record(macro_attr,{file,name}).
-record(fun_attr,{mod,name,arity}).

%% Records for the fitness function
-record(which_entities, {funs=true, recs=true, macros=false}).


%%
-define(CMISC,      cl_misc).
-define(Matrix,     cl_matrix).
