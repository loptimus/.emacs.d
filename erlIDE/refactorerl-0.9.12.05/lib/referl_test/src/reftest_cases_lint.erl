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

%%% @doc This module does some simple tests on test cases.
%%%
%%% It checks to see if both the TEST file and its 'Parameters' field is
%%% well-formed. It also does further verification if either a `position'
%%% or a `posrange' parameter is given in the file. In this case, it tries
%%% to find if any quoted instance in the free text of the TEST file that
%%% agrees with either the selection or the position. It emits warnings
%%% depending on context.

%% TODO: saner exception handling...
%% TODO: automatic hint generation from positions
%% TODO: automatic position generation from hints
%% TODO: more input sanitization
%% TODO: check for possible reuses into test_util
%% TODO: find phrases like:
%%        The selected text is `[^']*'.
%%        The selection is `[^']*'.
%%        The cursor is on `[^']*'.
%% TODO:wishlist possibly improved context dependency for the above
%% TODO: refactor Parameter/'file' checking into a separate lint
%% TODO: gather possible lints for other transformations
%% TODO: new lint: check for erl input and output files to be parseable
%% TODO: new lint: check for missing module declaration
%% TODO:wishlist generate missing module declaration
%% TODO: new lint: check for unsupported constructs (namespaces)
%% TODO:wishlist edoc documentation
%% TODO:wishlist add program arguments like lints to skip,
%%       or some options
%% TODO:wishlist check the parameters for other transformations
%% TODO:wishlist check for possible integration
%% TODO:wishlist add syntax for nth match: "A, B"#2
%% TODO:wishlist better handling of quoted quotes
%% TODO:wishlist possibility of preconditions for lints
%% TODO:wishlist possibly discount matches that fit parameters
%%       to improve false positive warning rate

%%%
%%% == New heading ==
%%%
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reftest_cases_lint).
-vsn("$Rev: 5569 $").

-export([main/0, show_result/1, do_lint_dir/1, do_lint_file/1,
         do_lint_file/2]).

%-include("refactorerl.hrl").

-record(state, {parsedtest, dirprefix}).

%% =============================
%% general helper functions
%%

%% show the emitted warnings, errors, etc. in textual form
%%TODO: should show results for multiple files and dirs
show_result({Atom,String}) ->
    Type = proplists:get_value( Atom,
               [ {error,"Error"}, {fail,"Fail"},
                 {warn,"warning"},{info," info"} ],
               "[unkown error atom]" ),
    lists:flatten(io_lib:format("~s, ~s", [Type,String]));
show_result(Result) ->
    lists:flatten(io_lib:format("~p", [Result])).

%% print the showed results with io:format/2
print_results(File, Results) ->
    PrintRes =
        fun(Result)->
            ShowedR = show_result(Result),
            io:format("~s: ~s~n",[File,ShowedR]) end,
    lists:foreach(PrintRes,Results).

%% =============================
%% the (main) lint_pos check and some needed regexp magic
%%

%% "local/1" -> "local("
%% TODO: strip "fun" prefix
subst_fun_to_app(E)->
     % todo The module `regexp' is deprecated.
     case regexp:match(E, "[^a-zA-Z0-9_#@/]*(fun +)?([a-zA-Z0-9_@]+:)?" ++
                          "[a-z][a-zA-Z0-9_@]*/[0-9]+") of
         {match,1,_} -> {ok,S,1}=regexp:sub(E, "/[0-9]+","\\("),
                        S;
         _ -> E
     end.

%% "g(1), g(2)" -> "g(1),[ \n]+g(2)"
tweak1(L) ->
    Space_expC = fun($ )-> "[ \n]+";
                    (C) -> [C] end,
    Space_exp  = fun(S) -> lists:flatmap(Space_expC, S) end,
    Transform = fun(E) -> subst_fun_to_app(Space_exp(E)) end,
    [Transform(E) || E <- L, E /= Transform(E)].

%% strip some of the regexp features to play nice with strings
strip_regexps(ZL) ->
    Char = fun(C)->case lists:member(C,"+*.?()[]|") of
                       true -> [$\\, C];
                       _    -> [C] end end,
    {_,L} = lists:unzip(ZL),
    lists:map(
        fun(S)-> lists:flatmap(Char,S) end,
        lists:usort(L)).

%% get a list of candidate strings to scan for in the Erlang source
get_candidates(Pattern, Test) ->
    [Elem || Key <- ['Goal','Documentation'],
     Elem<-begin
         String = proplists:get_value(Key, Test, []),
         % todo The module `regexp' is deprecated.
         {match,Matches} = regexp:matches(String,Pattern),
         Ofs2str = fun({B,L}) -> {{B,L},
                       lists:sublist(String,B+1,L-2)} end,
         lists:map(Ofs2str, Matches)
     end].

%% scan for the strings in the Erlang source file
scan_for_strings(String,List,Pos,Is_req,Can_tweak) ->
%    try
        % todo The module `regexp' is deprecated.
        C=[Match || Pattern <- List,
           Match<-case regexp:matches(String,Pattern) of
                      {match,Matches} -> Matches;
                      _ -> []
                  end ],
        Posmatch = fun({B1,E})-> L1 = E-B1+1,
                                 fun({B2,L2}) -> (B1==B2) and (L1==L2) end;
                      ( B1   )-> fun({B2,_L}) -> (B1==B2) end end,
        Anymatch = lists:any(Posmatch(Pos), C),
        Single = length(List)==1,
        F=false, T=true,
        case {Can_tweak, Anymatch, Is_req, Single} of
            {T,F,_,_} -> List2 = tweak1(List),
                         scan_for_strings(String,List2,Pos,Is_req,false);
            {_,F,T,_} -> {fail,"no match"};
            {_,F,F,T} -> {warn,"doesn't match, consider adding one"};
            {_,F,F,F} -> {warn,"none match, consider adding one"};
            {T,T,_,T} -> {info,"success! :)"};
            {F,T,_,T} -> {info,"success after tweaking! :)"};
            {T,T,F,F} -> {info,"one matched out of many"};
            {T,T,T,F} -> {warn,"one matched out of many"};
            {F,T,F,F} -> {info,"one matched out of many" ++
                               " after tweaking"};
            {F,T,T,F} -> {warn,"one matched out of many" ++
                               " after tweaking"}
        end
%       , io:format("~p~n", [{Pos,C}])
%    catch
%        X:Y -> error(io_lib:format("~p:~p",[X,Y]));
%    end
    .

%%extract the strings to scan for and do the scanning
extract_and_scan(Test, Params, Dirprefix) ->
    try
      {file,[_|_]=File} = proplists:lookup(file,Params),
      try
        Fullname    = filename:join([Dirprefix,File]),
        {ok,Binary} = file:read_file(Fullname),
        String      = binary_to_list(Binary),
        Pos = case proplists:get_value(position,Params) of
                  undefined ->
                      case proplists:get_value(posrange,Params) of
                          {U,V} when is_integer(U)
                                 and is_integer(V) -> {U,V}
                      end;
                  P when is_integer(P) -> P
              end,
        % todo The module `regexp' is deprecated.
        {ok,Regular} = regexp:parse("\"[^\"]*\""),
        {ok,Adhoc}   = regexp:parse("`+[^']*'+"),
        RegularsZ1 = get_candidates(Regular, Test),
        AdhocsZ    = get_candidates(Adhoc, Test),
        RegularsZ  = referl_misc:substract_ranges(RegularsZ1, AdhocsZ),
        Adhocs     = strip_regexps(AdhocsZ),
        Regulars   = strip_regexps(RegularsZ),
        case Regulars of
            [] -> case Adhocs of
                      [] -> {warn,"no position hint found"};
                      L  -> scan_for_strings(String,L,Pos,false,true)
                  end;
            L  -> scan_for_strings(String,L,Pos,true,true)
        end
      catch
%        X:Y -> {error,io_lib:format("~p:~p",[X,Y])};
        error:{badmatch,{error,_}}->
            {fail,"failed to read 'file' '" ++ File ++ "'"};
        error:{case_clause,undefined}->
            {warn,"missing 'position' from 'Parameters'"};
        error:{case_clause,_}->
            {error,"invalid 'position' specified"}
      end
    catch
      error:{badmatch,none}->
          {fail,"missing 'file' from 'Parameters'"};
      error:{badmatch,{file,_}}->
          {fail,"invalid 'file' fields in 'Parameters'"}
    end.

%% check the file for position or posrange related anomalies
lint_pos(Q) ->
    try
        {_, ParamS} = proplists:lookup('Parameters',Q#state.parsedtest),
        {ok,ParamT} = reftest_utils:string_to_term(ParamS),
        [extract_and_scan( Q#state.parsedtest, ParamT, Q#state.dirprefix )]
    catch
        error:{badmatch,none}->
            [{error,"missing 'Parameters' field"}];
        error:{badmatch,{error,_}}->
            [{error,"unparseable 'Parameters' field"}];
        error:{badmatch,{ok,_}}->
            [{error,"invalid 'Parameters' field"}]
    end.

%% =============================
%% lint_fields: check if all fields are present
%%

%% check if all recommended fields are present and unique
%% TODO: generally check non-uniqueness
lint_fields(Q) ->
    Fields = ['Title', 'Goal', 'Documentation', 'Parameters'],
    CheckFields =
        fun(K) -> case proplists:get_all_values(K,Q#state.parsedtest) of
                      []      -> {warn,"missing '" ++
                                       atom_to_list(K) ++ "' field"};
                      [_,_|_] -> {warn,"duplicates found for '" ++
                                       atom_to_list(K) ++ "' field"};
                      [_]     -> ok
                  end end,
    [ R || R <- lists:map(CheckFields, Fields), R /= ok ].

%% =============================
%% file and test case processing
%%


%% list of the lint checks that you wish to apply to each file
%%TODO: consider other lints, like checking for
%%       * bad spelling,
%%       * unbalanced paranthesis,
%%       * Step Two: ???
%%       * Step Three: Take Over World!!
lint_checks() -> [fun lint_fields/1, fun lint_pos/1].

%% check a single file
do_lint_file(File,Pre)->
    try
        Dir    = filename:join(filename:dirname(File),Pre),
        Test   = reftest_utils:parse_testfile(File),
        State  = #state{parsedtest=Test, dirprefix=Dir},
        Result = lists:flatmap(fun(X)->X(State) end, lint_checks()),
        lists:flatten(Result)
    catch
        error:{badmatch,{error,_}}->
            [{error,"failed to read file '" ++ File ++ "'"}]
    end.
do_lint_file(File)-> do_lint_file(File,"").

%% check all test cases of a transformation directory
%%TODO: return value should be a list, smarter show_results
do_lint_dir(Dir)->
    Files = filelib:wildcard(filename:join([Dir,"*","TEST"])),
    case Files of
        [] -> [{warn,"no test cases found in "++Dir}];
        L  -> Chk = fun(F) -> print_results(F,do_lint_file(F)) end,
              lists:foreach(Chk,L)
    end.

%% this is an example main
main() ->
    do_lint_dir("test/gen"). %%try: "/PATH_TO_MY_TEST_ROOT/*"
