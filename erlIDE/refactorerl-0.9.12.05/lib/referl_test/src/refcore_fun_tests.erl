
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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2010,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc todo
%%%
%%% @todo author

-module(refcore_fun_tests).
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").

insrem_test_() ->
    {setup, fun add_files/0, fun del_files/1,
     fun ({F1, F2}) ->
             {inorder, [{"ins def",  fun() -> ins_def(F1) end},
                        {"ins call", fun() -> ins_call(F1) end},
                        {"del call", fun() -> move_call(F1, F2) end}
                       ]}
     end}.


ins_def(F) ->
    ?FileMan:add_text(F, last, "f(_,_)->ok.\n"),
    ?ESG:finalize(),

    [Def] = ?ESG:path(F, [{form, last}]),

    [Fun] = ?ESG:path(F, [moddef,
                          {func, {{name, '==', f}, 'and', {arity, '==', 2}}}]),

    ?assertMatch([Def], ?Graph:path(Fun, [{fundef, back}])),
    ok.

ins_call(F) ->
    ?FileMan:add_text(F, last, "g()->f(1,2).\n"),
    ?ESG:finalize(),

    [Call] = ?ESG:path(F, [{form, last}, {funcl,1}, {body, 1}]),
    [Fun] = ?ESG:path(Call, [funlref]),
    ?assertMatch(#func{name=f, arity=2}, ?ESG:data(Fun)),
    ?assertMatch([F], ?ESG:path(Fun, [{fundef, back}, {form, back}])),
    ok.

move_call(F1, F2) ->
    [Fun] = ?ESG:path(F1, [moddef,
                          {func, {{name, '==', f}, 'and', {arity, '==', 2}}}]),

    [Frm] = ?ESG:path(F1, [{form, last}]),
    ?ESG:remove(F1, form, Frm),
    ?ESG:insert(F2, form, Frm),
    ?ESG:finalize(),

    ?assertMatch([], ?ESG:path(Fun, [{funlref, back}])),
    ok.


add_files() ->
    F1 = ?ESG:create(#file{type=module, path="test1.erl"}),
    ?ESG:insert(?ESG:root(), file, F1),
    ?FileMan:add_text(F1, last, "-module(test1).\n"),
    F2 = ?ESG:create(#file{type=module, path="test2.erl"}),
    ?ESG:insert(?ESG:root(), file, F2),
    ?FileMan:add_text(F2, last, "-module(test2).\n"),
    ?ESG:finalize(),
    {F1, F2}.

del_files({F1, F2}) ->
    ?ESG:remove(?ESG:root(), file, F1),
    ?ESG:remove(?ESG:root(), file, F2),
    ?ESG:finalize().

