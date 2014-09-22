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

%%% @doc

%%% == Implementation status ==
%%% This feature is _not_ fully implemented.

%%% @author Gabor Olah <olikas.g@gmail.com>
-module(reftr_genspec).

-export([prepare/1]).

-include("user.hrl").
-include("spec.hrl").

prepare(Args) ->
    Func = ?Args:function(Args),
    Spec = refusr_spec:run_tr(Func),
    ConstructRes = make_construct_spec(Func, Spec),
    [fun() ->
             NewNode = ?Syn:construct(ConstructRes),
	     [FunDef] = ?Query:exec(Func, [{fundef, back}]),
             [File] = ?Syn:get_file(FunDef),
             Index = ?ESG:index(File,form,FunDef),
	     ?Transform:touch(File),
             ?ESG:insert(File, {form,Index}, NewNode)
     end].




make_construct_spec(Func, Spec) ->
    mcs(Func, Spec).

mcs(Func, Funsig) ->
    %?d({Func, Funsig}),
    FuncName  = reflib_function:name(Func),
    ArgTypes  = [type(A) || A <- (Funsig#type.value)#funsigvalue.args],
    RetValue  = type((Funsig#type.value)#funsigvalue.retval),
    {{spec,FuncName}, ArgTypes, RetValue}.

type([T]) ->
    type(T);
type({'type', 'tuple', 'any'}) ->
    {type, tuple};
type({'type', 'tuple', Elements}) ->
    {{type, tuple}, [type(X) || X <- Elements]};
type({'type', 'list', 'nil'}) ->
    {type, nil};
type({'type', 'list', 'any'}) ->
    {type, list};
type({'type', 'list', V}) ->
    {{type, list}, [type(V)]};
type({'type', 'union', Types}) ->
    {union, [type(X) || X <- Types]};
type({'type', TypeName, _Value}) when is_atom(TypeName) ->
    {type, TypeName}.


