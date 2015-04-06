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

%%% @doc Unit test for {@link reflib_module}.
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(reftest_reflib_module).

-vsn("$Rev: 5137 $").

-compile([export_all]).

-include("test.hrl").

files() ->
    [{module, "module1.erl",
      "-module(module1).\n"
      "-export([h/0,f/0]).\n"
      "-import(module2,[g/1]).\n"
      "f() ->\n"
      "  ok.\n"
      "h() ->\n"
      "  {g(1),f()}.\n"},
     {module, "module2.erl",
      "-module(module2).\n"
      "-export([z/0,g/1]).\n"
      "z() ->\n"
      "  g(1).\n"
      "g(X) ->\n"
      "   X.\n"
      }].

test_module_props() ->
    [ModuleNode] = ?ESG:path(?ESG:root(),[{module, {name, '==', module1}}]),
    ModuleNode = ?Query:exec1(?Mod:find(module1), module_node),

    [FileNode]   = ?ESG:path(ModuleNode,[{moddef, back}]),
    FileNode   = ?Query:exec1(ModuleNode, ?Mod:file(), file_node),   
    ok.

test_module_fun_props()->
    ModuleNode  = ?Query:exec1(?Mod:find(module1), module_node),

    [LocalFun]    = ?ESG:path(ModuleNode, 
                  [{func, {{name,'==', f},'and',{arity,'==', 0}}}]), 
    LocalFun    = ?Query:exec1(ModuleNode, ?Mod:local(f, 0), local_fun),

    [ImportedFun] = ?ESG:path(ModuleNode, 
                             [{funimp, {{name, '==', g} ,'and' ,
                                                        {arity,'==', 1}}}]),
    ImportedFun = ?Query:exec1(ModuleNode, ?Mod:imported(g, 1), imported_fun),

    [ExportedFun] = ?ESG:path(ModuleNode, 
                                  [{funexp, {{name,'==', h} ,'and' ,
                                                        {arity,'==', 0}}}]),
    ExportedFun = ?Query:exec1(ModuleNode, ?Mod:exported(h, 0), exported_fun),
    Funimp = refcore_esg:path(ModuleNode,
                  [{funimp,{{name, '==', g}, 'and', {arity, '==', 1}}}]),
    Func = refcore_esg:path(ModuleNode,
                  [{func,{{name, '==', g}, 'and', {arity, '==', 1}}}]),
    [VisibleFuns] = Funimp ++ Func, 
    VisibleFuns = ?Query:exec1(ModuleNode, ?Mod:visible(g, 1), visible_fun),
    ok.

test_module_rel_transf_add() ->
    ModuleNode1  = ?Query:exec1(?Mod:find(module1), module_node2),
    ModuleNode2  = ?Query:exec1(?Mod:find(module2), module_node2),  
    [Fun] = ?ESG:path(ModuleNode1, 
                       [{funexp, {{name,'==', h} ,'and' ,
                                                {arity,'==', 0}}}]),
    ?Mod:add_import(ModuleNode2, Fun),
    ?ESG:finalize(),
    ?Mod:del_import(ModuleNode2, Fun),
    ?ESG:finalize(),
    ok.

