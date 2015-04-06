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
-module(refusr_spec_analspec).

-vsn("$Rev: 7872 $"). %for emacs"

-include("user.hrl").

-include("spec.hrl").

-export([analyzeSpecs/0]).

%% =========================================================================
%% =========================================================================
%% retrieving the existing -spec forms

analyzeSpecs() ->
    AllSpecForms = ?Query:exec(?SLIB:specs()),
    Specs = lists:map(fun buildFromSpec/1, AllSpecForms),
    [ ets:insert(specs, X) || X <- Specs].

buildFromSpec(Form) ->
    [Modpart, Funpart]  = ?Query:exec(Form, [tattr]),
    [Mod] = ?Query:exec(Form, ?Form:module()),
    ModName = ?Mod:name(Mod),
    {Mod_ret, Name} = analModpart(Modpart),
    case {Mod_ret, ModName} of
        {{ok, ModName}, ModName} ->
            Mod;
        {{ok, ModName}, _Name} ->
            io:format("Warning! Module name ~p doesn't match in specification! (~p)~n",
                      [ModName, Name]);
        {{error, unknown}, _} ->
            Mod
    end,
    FunPart = analFunpart(Funpart),
    #type{kind = funsig, value = {Args, _RetV}} = FunPart,
    [Func] = ?Query:exec(Mod, ?Fun:find(Name, length(Args))),
    {Func, FunPart}.

analModpart(Node) ->
    case ?SLIB:typexp_type(Node) of
        module_qualifier ->
            [Modq, Name] = ?Query:exec(Node, [tsub]),
            {{ok, ?SLIB:typexp_tag(Modq)}, ?SLIB:typexp_tag(Name)};
        atom ->
            Name = ?SLIB:typexp_tag(Node),
            {{error, unknown}, Name}
    end.

analFunpart(Node) ->
    case ?SLIB:typexp_type(Node) of
        fun_sig ->
            [Arglist, RetType] = ?Query:exec(Node, [tsub]),
            #type{kind = funsig,
                  value = {analFunpart(Arglist), analFunpart(RetType)}};
        arglist ->
            Args = ?Query:exec(Node, [tsub]),
            lists:map(fun analFunpart/1, Args);
        call ->
            [Type, ArgList] = ?Query:exec(Node, [tsub]),
            R = case ?Query:exec(ArgList, [tsub]) of
                    [] ->
                        any;
                    Args ->
                        lists:map(fun analFunpart/1, Args)
                end,
            #type{kind = ?SLIB:typexp_tag(Type),
                  value = R};
        atom ->
            #type{kind = atom,
                  value = ?SLIB:typexp_tag(Node)};
        tuple ->
            Fields = ?Query:exec(Node, [tsub]),
            #type{kind = tuple,
                  value = lists:map(fun analFunpart/1, Fields)};
        list ->
            Elems = ?Query:exec(Node, [tsub]),
            #type{kind = list,
                  value = lists:map(fun analFunpart/1, Elems)};
        vardef ->
            [_VarName, Type] = ?Query:exec(Node, [tsub]),
            analFunpart(Type);
        paren ->
            [Children] = ?Query:exec(Node, [tsub]),
            analFunpart(Children);
        joker ->
            #type{kind = any,
                  value = any};
        union ->
            Fields = ?Query:exec(Node, [tsub]),
            #type{kind = union,
                  value = lists:map(fun analFunpart/1, Fields)};
        _ ->
            not_implemented_yet
    end.


analyze(Node) ->
    analyze(Node, ?SLIB:typexp_type(Node)).



analyze(Node, 'atom') ->
    ?SLIB:t_atom( ?SLIB:typexp_tag(Node) );
analyze(Node, 'integer') ->
    ?SLIB:t_int( ?SLIB:typexp_tag(Node) );
%%TODO: what to do with it?
analyze(Node, 'variable') ->
    ?SLIB:t_variable( ?SLIB:typexp_tag(Node) );
analyze(Node, joker) ->
    ?SLIB:t_any();
analyze(Node, paren) ->
    analyze(Node, ?SLIB:typexp_type(Node));
analyze(Node, func) ->
    case ?SLIB:typexp_tag(Node) of
        'any' ->
                                                % TODO that does it mean?
            what_to_do;
        'sig' ->
            [FS] = ?Query:exec(Node, [tsub]),
            analyze(FS)
    end;
analyze(Node, interval) ->
    [LB, HB] = ?Query:exec(Node, [tsub]),
    LowBound = analyze(LB),
    HighBound = analyze(HB),
    Diff = LowBound#type.value - HighBound#type.value,
    ?SLIB:t_int(Diff);
analyze(Node, negate) ->
    [N] = ?Query:exec(Node, [tsub]),
    ?SLIB:t_int( - ( (analyze(N) )#type.value ) );
analyze(Node, list) ->
    case ?SLIB:typexp_tag(Node) of
        'empty' ->
            ?SLIB:t_nil();
        'any' ->
            [T] = ?Query:exec(Node, [tsub]),
            ?SLIB:t_list( analyze(T) );
        'nonempty' ->
            [T] = ?Query:exec(Node, [tsub]),
            ?SLIB:t_list( analyze(T) )
    end;
analyze(Node, tuple) ->
    case ?Query:exec(Node, [tsub]) of
        [] ->
            ?SLIB:t_tuple();
        NodeList ->
            ?SLIB:t_tuple( [ analyze(N) || N <- NodeList ] )
    end;
analyze(Node, union) ->
    NodeList = ?Query:exec(Node, [tsub]),
    ?SLIB:t_union( [ analyze(N) || N <- NodeList ] );
analyze(Node, record) ->
    %% TODO: implement
    unimplemeted.
