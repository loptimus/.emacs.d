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

%%% @doc Rename unused variables implementation module. This    refactoring
%%% renames every  unused  variable  to  _VarName (where VarName is the old 
%%% name of the variable). A variable is unused, if it is not referenced in
%%% any expression.

%%% == Parameters ==
%%% <ul>
%%% <li>The name of the module, where unused variables should be 
%%%     renamed.</li>
%%% </ul>

%%% == Conditions of applicability ==
%%% <ul>
%%% <li></li>
%%% </ul>

%%% == Transformation steps and compensations ==
%%% <ol>
%%% <li></li>
%%% </ol>

%%% == Implementation status ==
%%% This refactoring is fully implemented.

%%% @author Gabor Hosszu <hogsabi@inf.elte.hu>

-module(reftr_rename_unused_vars).
-vsn("").

%% Callbacks
-export([prepare/1]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Module     = ?Args:module(Args),
    Funs       = ?Query:exec(Module, ?Mod:locals()),
    Forms      = lists:concat([ ?Query:exec(Fun, 
                                            ?Fun:definition()) || Fun <- Funs ]),
    DeepExprs  = [ ?Query:exec(Form, ?Form:deep_exprs()) || Form <- Forms],
    DeepVars   = lists:usort(lists:append([ ?Query:exec(Expr, 
                             ?Expr:variables()) || Expr <- DeepExprs ])),
    
    
    VarsToRef  = [ Var || Var <- DeepVars, 
                          get_var_refnum(Var) == 0, 
                          string:left(?Var:name(Var), 1) =/= "_" ],

%%    VarsToRefNames   = [ ?Var:name(Var) || Var <- VarsToRef ], 

%%    ArgsInfo   = add_transformation_info(Args, VarsToRefNames),

    [ fun() -> [] end ] ++ 
    [ rename_var(Var) || Var <- VarsToRef ] ++
    [ fun(List) -> [ hd(?Query:exec(Item, 
                        ?Expr:variables())) || Item <- List]  end ].

%%  add_transformation_info(Args, VarsToRefNames) ->
%%    ?d(VarsToRefNames),
%%    VNames = string:join(VarsToRefNames, ", "),
%%    ?d(VNames),
%%    Info    = ?MISC:format("Renaming unused variables : "
%%                           ++ VNames),
%%    [{transformation_text, Info} | Args].

get_var_refnum(Var) ->
    References  = ?Query:exec(Var,?Var:references()),
    length(References)
. 

rename_var(Var) ->
   	Occs        = ?Query:exec(Var,  ?Var:occurrences()),
	VName = ?Var:name(Var), 	
    fun(List) ->
        ?Macro:inline_single_virtuals(Occs, elex),
        [?Macro:update_macro(VarOcc, {elex, 1}, ("_" ++ VName)) || VarOcc <- Occs],
	[ hd(Occs) | List ]
    end
.

%%% ============================================================================
%%% Checks

