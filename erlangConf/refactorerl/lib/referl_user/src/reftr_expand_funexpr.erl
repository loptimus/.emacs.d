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

%%% ============================================================================
%%% Module information

%%% @doc This refactoring expands an implicit fun expression to its
%%% explicit form. The variable names are generated for the argument
%%% list.
%%%
%%% Example:
%%%
%%% <pre>fun foo/2</pre> becomes to <pre>fun (V1, V2) -> foo(V1, V2) end</pre>
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li>An expression
%%%       (see {@link reflib_args:expression/1}).</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li>The selected expression should be an implicit fun expression
%%%   or part/subexpression of an implicit fun expression</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%   <li>If the implicit fun expression is found, the new syntax
%%%   structure is created and the old expression is replaced with the
%%%   new one</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(reftr_expand_funexpr).
-vsn("$Rev: 8204 $ ").

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Errors

%% @private
error_text(implicit_not_found, []) ->
    "Implicit fun expression has to be given".

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    case [E || {_, E} <- ?Syn:root_path(?Args:expression(Args)),
               ?Syn:class(E) == expr, is_implicit_fun_expr(E)] of
        [] ->
            throw(?LocalErr0r(implicit_not_found));
        [Expr|_] ->
            fun() ->
                    File = ?Syn:get_file(Expr),
                    New = ?Expr:expand_funexpr(Expr),
                    ?Transform:touch(File),
                    [New]
            end
    end.

is_implicit_fun_expr(Expr) ->
    SubExprKindOk =
        case ?Query:exec(Expr, ?Expr:children()) of
            [] -> false;
            [FunRef|_Arity] ->
                case {?Expr:type(FunRef), ?Expr:value(FunRef)} of
                    {infix_expr, ':'} -> true;
                    {atom, _}         -> true;
                    _                 -> false
                end
        end,
    ?Expr:type(Expr) == implicit_fun andalso SubExprKindOk.
