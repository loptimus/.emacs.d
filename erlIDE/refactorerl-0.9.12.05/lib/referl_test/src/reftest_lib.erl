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

%%% @doc Unit testing helper module. A test module should contain the
%%% following callback functions:
%%% <dl>
%%%  <dt>`files() -> [{Type, Name, Text}]'</dt>
%%%  <dd>Specifies the files that should be loaded into the database prior to
%%%      running the tests. `Type' can be `module' or `header', `Name' is a
%%%      filename (a good convetion is to specify it without a path), and
%%%      `Text' is a string that contains the source code of the file.</dd>
%%%
%%%  <dt>`test_*() -> ok'</dt>
%%%  <dd>An arbitrary test function. These functions will be run in alphabetic
%%%      order. When an error occurs in the function, it is displayed. Calling
%%%      `exit/1' stops the processing, other errors are ignored. Another good
%%%      convention is to use match expressions with constants in these
%%%      functions to check things; these will generate `bad_match' errors
%%%      automatically.</dd>
%%% </dl>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(reftest_lib).
-vsn("$Rev: 5809 $").

-export([run/0, run/1, load/1]).

-define(TEST_MODULES, [reftest_reflib_args,
                       reftest_reflib_expression,
                       reftest_reflib_file,
                       reftest_reflib_function,
                       reftest_reflib_macro,
                       reftest_reflib_record,
                       reftest_reflib_record_field,
                       reftest_reflib_syntax,
                       reftest_reflib_token,
                       reftest_reflib_variable,
                       reftest_reflib_form,
                       reftest_reflib_clause,
                       reftest_reflib_module,
                       reftest_referl_anal_fun,
                       reftest_referl_anal_rec,
		       reftest_reflib_header]).

-include("test.hrl").

%% @spec run(atom()) -> ok | error | exit
%% @doc Runs the tests specified in the test callback module `Mod'. Returns `ok'
%% when all tests have finished without problem, `error' when errors happened,
%% and `exit' when testing has been aborted.
run(Mod) ->
    ?Graph:reset_schema(),
    run_(Mod).

run_(Mod) ->
    LoadedFiles = load(Mod),
    try
        lists:foldl(fun run_test/2, ok, test_funs(Mod)),
	?MISC:drop_files(LoadedFiles),
	[delete_file(Name) || Name <- Mod:files()],
	ok
    catch
        throw:exit -> exit
    end.


%% @spec run() -> ok | error
%% @doc Runs all known module tests. Returns `ok' if all tests passed, `error'
%% otherwise. Tests to be run are specified in the TEST_MODULES macro.
run() ->
    ?Graph:reset_schema(),
    R = [begin
             io:format("~s~n", [M]),
             run(M)
         end || M <- ?TEST_MODULES],
    case lists:all(fun (X) -> X == ok end, R) of
        true  -> ok;
        false -> error
    end.

%% @spec load(atom()) -> [Nodes]
%% @doc Loads the test file from the unit test in `Mod'.
load(Mod) ->
    [add_file(File)|| File <- Mod:files()].

add_file({_Type,Name0,Text})->
    Name = ?MISC:canonical_filename(Name0),
    FileInfo = file:write_file(Name,Text),
    case FileInfo of
	ok              -> 
	    {file,Node} = ?FileMan:add_file(Name),
	    Node;
	{error, Reason} ->
	    io:format("File creating error: ~p~n",[Reason])
    end.

delete_file({_Type,Name0,_Text}) ->
    DeleteInfo = file:delete(?MISC:canonical_filename(Name0)),
    case DeleteInfo of
	ok -> ok;
	{error,Reason} -> io:format("Cannot delete ~p: ~p~n",[Name0,Reason])
    end.

test_funs(Mod) ->
    lists:sort(
      [{Mod, Fun} ||
          {Fun, Arity} <- Mod:module_info(exports),
          Arity =:= 0,
          lists:prefix("test", atom_to_list(Fun))]).

run_test(Test, Res) ->
%    ?d(Test),
    case run_test(Test) of
        ok    -> Res;
        error -> error
    end.

run_test({Mod, Fun}) ->
    try Mod:Fun() of
        ok -> ok;
        Result ->
            io:format("~n** FAILURE ~p~n   in test ~s:~s~n",
                      [Result, Mod, Fun]),
            error
    catch
        error:Reason ->
            io:format("~n** ERROR ~p~n   in test ~s:~s~n Stack: ~p~n",
                      [Reason, Mod, Fun, erlang:get_stacktrace()]),
            error;
        throw:Reason ->
            io:format("~n** EXCEPTION ~p~n   in test ~s:~s~n",
                      [Reason, Mod, Fun]),
            error;
        exit:Reason ->
            io:format("~n** EXIT ~p~n   in test ~s:~s~n",
                      [Reason, Mod, Fun]),
            throw(exit)
    end.
