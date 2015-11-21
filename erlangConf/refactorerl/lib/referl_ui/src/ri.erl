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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc This module implements a console user interface for the tool.
%%%
%%% The idea behind `ri' is to enable the user to be able to do refactorings
%%% as easily as you can do debugging with Erlang's dbg module.
%%% You give a simple, short command, and get response on the console.
%%% Functions suffixed by `_h' give brief help about the respective function.
%%%
%%% == Server management command list ==
%%% <ul>
%%%   <li>add(FDML)
%%%       - add a module, file/dir or a list of these to the database</li>
%%%   <li>drop(FDML)
%%%       - drop (remove) the said way from the database</li>
%%%   <li>save(FDML)
%%%       - save the said way from the database to physical files</li>
%%%   <li>ls()
%%%       - lists files that are in the database</li>
%%%   <li>undo()
%%%       - undo the transformation (rollback, only once!)</li>
%%%   <li>reset()
%%%       - reset the database to an empty state, but valid schema</li>
%%%   <li>graph(Target)
%%%       - assume no options and call one of the next two</li>
%%%   <li>graph(Atom,Options)
%%%       - assume ".dot" extension and call the one below</li>
%%%   <li>graph(File,Options)
%%%       - draw the database graph with the given options</li>
%%% </ul>

%%% == Transformation command list ==
%%% <ul>
%%%   <li>elimvar(In, Pos)
%%%       - eliminate variable</li>
%%%   <li>extfun (In, Range)
%%%       - extract the selected expressions into a function</li>
%%%   <li>expfun (In, Pos)
%%%       - expand implicit funexpression to function call</li>
%%%   <li>genfun (In, Range, NewVarName)
%%%       - generalize function with a new argument</li>
%%%   <li>inlfun (In, Pos)
%%%       - inline function at application</li>
%%%   <li>inlmac (In, Pos)
%%%       - inline macro at use</li>
%%%   <li>intrec (In, Range, NewRec, [RecFldName1, RecFldName2, ...]))
%%%       - introduce record instead of tuple</li>
%%%   <li>merge  (In, Range, NewVarName)
%%%       - merge common subexpressions into a new variable</li>
%%%   <li>movfun (From, ToMod, [@{FunName,Arity@}|_])
%%%       - move function</li>
%%%   <li>movrec (From, To, [RecName|_])
%%%       - move record</li>
%%%   <li>movmac (From, To, [MacName|_])
%%%       - move macro</li>
%%%   <li>reorder(In, @{FunName,Arity@}, [ArgIdx|_])
%%%       - reorder function arguments</li>
%%%   <li>renfld (In, RecName, RecFldName, NewRecFldName)
%%%       - rename record field</li>
%%%   <li>renfun (In, @{FunName,Arity@}, NewFunName)
%%%       - rename function</li>
%%%   <li>renhrl (FromFile, ToFile)
%%%       - rename header file</li>
%%%   <li>renrec (In, RecName, NewRecName)
%%%       - rename record</li>
%%%   <li>renmac (In, MacName, NewMacName)
%%%       - rename macro</li>
%%%   <li>renmod (From, ToMod)
%%%       - rename module</li>
%%%   <li>renuvars (ModName)
%%%       - rename unused variables</li>
%%%   <li>renvar (In, Range, NewVarName)
%%%       - rename variable</li>
%%%   <li>tupfun (In, Range)
%%%       - change function arguments into tuple</li>
%%%   <li>upregex()
%%%       - upgrade regexp from "regexp" module to "re" module usage</li>
%%% </ul>
%%%
%%% == An explanation of argument types ==
%%% <ul>
%%%   <li>filename as string or module name as atom: In, From and To.</li>
%%%   <li>strings: FromFile, ToFile and MacName.</li>
%%%   <li>atoms: ToMod, RecName, RecFldName and FunName.</li>
%%%   <li>integers: Arity and ArgIdx.</li>
%%% </ul>
%%%
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

% @todo :wishlist factor down to smaller modules
% @todo expand the comments at the top to reflect added features
% @todo generated function and menu help from spec and doc tags
% @todo support for filename suffixes for add/drop
% @todo :wishlist better parameter checking (Name, Pos, ...)!
% @todo :wishlist verbose commenting with edoc tags
% @todo :wishlist spec tags
% @todo :wishlist research alternatives like
%       Beg,End -> line.col:line.col; line.col:col; line:regex ;
% @todo :wishlist function scope regexps
% @todo: better regexp mark handling
%

-module(ri).
-vsn("$Rev: 8415 $ ").

-export([help/0, help/1, h/0, h/1]).

-export([add/1, add/2,  drop/1, save/1, stop/0, add_hrl/1,
         generate/1, addQuery/2,
         ls/0,    ls/1,   reset/0, clean/0, cat/1, cat/2, cat/3,
         checkpoint/0, checkpoint/1, backup/0, backup/1,
         ls_backups/0, ls_checkpoints/0,
         backup_info/1, checkpoint_info/1,
         restore/1, undo/0, graph/0, graph/1, graph/2,
         svg/0,   svg/1,  svg/2, gn/2,
         getcfg/0,setcfg/3,
         build/0, build/1, transform/2,
         anal_dyn/0, clean_dyn/0, anal_message/0]).
-export([envs/0, env/1, env/2, addenv/2, addenv/3, setenv/2,
         delenv/1, delenv/2, delenv/3]).

-export([create_graph/1, rename_graph/2, ls_graphs/0, actual_graph/0,
         load_graph/1, delete_graph/1, delete_all_graphs/0]).

-export([create_graph_h/0, rename_graph_h/0, ls_graphs_h/0, actual_graph_h/0,
         load_graph_h/0, delete_graph_h/0, delete_all_graphs_h/0, clean_h/0]).

-export([add_h/0,    drop_h/0,  save_h/0, stop_h/0,
         add_hrl_h/0,
         ls_h/0,     reset_h/0, cat_h/0,
         backup_h/0, checkpoint_h/0,
         ls_backups_h/0, ls_checkpoints_h/0,
         backup_info_h/0, checkpoint_info_h/0,
         restore_h/0, undo_h/0, graph_h/0,  svg_h/0, gn_h/0,
         getcfg_h/0, setcfg_h/0,
         build_h/0,  transform_h/0,
         genspec_h/0]).
-export([envs_h/0, env_h/0, addenv_h/0, setenv_h/0,
         delenv_h/0]).

-export([elimvar/2,extfun/3, expfun/2, genfun/3,
         inlfun/2, inlmac/2, intrec/4,
         merge/3,  movfun/3, movrec/3, movmac/3,
         reorder/3,reorder/1,renfld/4, renfun/3, renofuns/3,
         renhrl/2, renrec/3, renmac/3,
         renmod/2, renuvars/1, renvar/3, tupfun/2, upregex/0,
         appfuncl/1, genspec/2]).
-export([q/1, q/2, q/3, q/4, metric/1, cluster/0, cluster/2,
         metricmode/1, metricmode/0]).

-export([lsl/0]).

-export([elimvar_h/0,extfun_h/0, expfun_h/0, genfun_h/0,
         inlfun_h/0, inlmac_h/0, intrec_h/0,
         merge_h/0,  movfun_h/0, movrec_h/0, movmac_h/0,
         reorder_h/0,renfld_h/0, renfun_h/0, renofuns_h/0,
         renhrl_h/0, renrec_h/0, renmac_h/0,
         renmod_h/0, renuvars_h/0,
	 renvar_h/0, tupfun_h/0, upregex_h/0,
         appfuncl_h/0, metricmode_h/0]).
-export([q_h/0, metric_h/0, cluster_h/0]).

-export([lsl_h/0]).

-export([start_yaws/0, start_yaws/1, stop_yaws/0]).


-export([start_nitrogen/0, start_nitrogen/1, stop_nitrogen/0]).

-export([check_layered_arch/2, show_layered_arch/2, show_layered_arch/3]).

-export([search_duplicates/0, search_duplicates/1, stored_dupcode_results/0, 
         save_dupcode_result/2, show_dupcode/1, show_dupcode_group/2]).


-export([errors/0, errors/1]).

-export([error_text/2]).
-export([test/0, test/1, test/2, testall/0]).
%-export([rgxpos_text/2]). %%DEBUG
%-export([getparen/1]). %%DEBUG

-export([cat_errors/0, cat_errors/1]).

-export([modsave/1, modload/1]).

-export([draw_dep/1, print_dep/1]).
-export([draw_dep_h/0, print_dep_h/0]).

-export([dir_sort/0, dir_sort/1]).
-export([dir_sort_h/0]).

-export([fb_relations/1]).

-export([fb_regexp/1]).

-export([draw_gen_server/2, draw_gen_fsm/2]).

% @private only for internal use!
-export([global_printer_process/0, get_constrained/3, ui_loop/1]).

-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").

-define(DEF_GRAPH_NAME,graph).
-define(PrintProc,referl_ri_global_printer). %@todo supervise

ui(NameArgs)->
%    io:format(" DEBUG ri request ~p~n",[NameArgs]),
    _PIDPORT = spawn_printer_process(),
    ReqID = ?UI:getid(),
    ok = ?UI:request(ReqID,NameArgs),
    ui_loop(ReqID).

ui_loop(ReqID) ->
    receive
        {ReqID, reply, R} ->
            case R of
                {error, {_Code, Msg}} ->
                    message("Error: " ++ Msg),
                    error;
                _ ->
                    R
            end;

        {ReqID, progress, {add, File, 1, Max}} ->
            message2(io_lib:format("loading: ~s (~p forms)~n~4w|",
                   [File, Max, 1])),
            ?MODULE:ui_loop(ReqID);
        {ReqID, progress, {drop, File, 1, Max}} ->
            message2(io_lib:format("dropping: ~s (~p forms)~n~4w|",
                   [File, Max, 1])),
            ?MODULE:ui_loop(ReqID);
        {ReqID, progress, {add, File, Percent, FormCount, FormMax, KBps}} ->
            print_progress(File, Percent, FormCount, FormMax, KBps),
            ?MODULE:ui_loop(ReqID);
        {ReqID, progress, {drop, File, _Percent, FormCount, FormMax, KBps}} ->
            print_progress(File, FormCount/FormMax, FormCount, FormMax, KBps),
            ?MODULE:ui_loop(ReqID);

        {ReqID, question, {QID,Questions}} ->
            Text1 = " (blank to abort).",
            case form_type(Questions) of
                checkbox->
                    Text0 = "Please select some items from the list",
                    io:format("~s~s~n",[Text0,Text1]),
                    ok = answer_box(ReqID, QID, Questions, checkbox);
                radio->
                    Text0 = "Please choose an item from the list",
                    io:format("~s~s~n",[Text0,Text1]),
                    ok = answer_box(ReqID, QID, Questions, radio);
                other->
                    case interactive(Questions) of
                        true ->
                            Text0 = "Please answer the following question",
                            io:format("~s~s~n",[?MISC:plural(Text0,Questions),Text1]);
                        false ->
                            Text0 = "See the direct information feed below:",
                            io:format("~s~n",[Text0])
                        end,
                    ok = answer0(ReqID, QID, Questions)
            end,
            ?MODULE:ui_loop(ReqID);
        _M ->
            %Ui_loop = io_lib:format(" WARNING ri:ui_loop(~p) got:~n ~p ~n",[ReqID,M]),
            %?d(Ui_loop),
            ?d(io_lib:format(" WARNING ri:ui_loop(~p) got:~n ~p ~n",[ReqID,_M])),
            ?MODULE:ui_loop(ReqID)
    end.

print_progress(File, Percent, FormCnt, FormMax, KBps) ->
    PrgMax       = 30,
    Mark         = $\>,
    KBpsTxt      = ?MISC:format("~5.2f kB/s", [0.0 + KBps]), % format/2 requires float
    KBpsLen      = length(KBpsTxt),
    MarkCnt      = round(Percent*PrgMax),
    PrgWidth     = PrgMax - 1 - KBpsLen,
    PrgWidthTxt  = integer_to_list(PrgWidth),
    FormFNameTxt = ?MISC:format("[~4w/~4w] ~s",
                                [FormCnt, FormMax, filename:basename(File)]),
    case PrgWidth < MarkCnt of
        false ->
            MarkCntTxt = integer_to_list(MarkCnt),
            io:format("\r|~-" ++ PrgWidthTxt ++ "." ++ MarkCntTxt ++ "c ~s| ~s",
                      [Mark, KBpsTxt, FormFNameTxt]);
        true ->
            MarkCntTxt = integer_to_list(MarkCnt - 1 - KBpsLen),
            io:format("\r|~s ~-" ++ PrgWidthTxt ++ "." ++ MarkCntTxt ++ "c| ~s",
                      [KBpsTxt, Mark, FormFNameTxt])
    end,
    case Percent == 1 of
        true  -> io:put_chars("\n");
        false -> ok
    end.


interactive(Questions)->
    Formats = lists:usort([ hd(?MISC:pgetu([format],Q)) || Q <- Questions ]),
    not is_subset(Formats,[info]).

form_type(Questions)->
    Formats = lists:usort([ hd(?MISC:pgetu([format],Q)) || Q <- Questions ]),
    case ?MISC:intersect(Formats, [radio, checkbox]) == [] of
        true -> other;
        false ->
            case is_subset(Formats, [info, radio]) of
                true -> radio;
                false ->
                    case is_subset(Formats, [info, checkbox]) of
                        true -> checkbox;
                        false -> other
                    end
            end
    end.

is_subset(Sub,Sup)->
    []==Sub--Sup.

answer0(ReqID, QID, Questions) ->
    answer1(ReqID, QID, [], Questions). %@todo check validators

answer1(ReqID, QID, Answers, _Questions=[Question|Qs]) ->
    [Format,Text] = ?MISC:pgetu([format,text], Question),
    Result =
        case Format of
            textbox ->
                get_constrained(
                  " "++Text++" ",
                  "",
                  fun(X)->{ok, X}end);
            T when T==yesno; T==checkbox -> % TODO: spank interaction developer
                get_constrained(
                  " "++Text++" (y/n) -> ",
                  "Answer either 'y' or 'n'",
                  fun
                      ("y") -> {ok, yes};
                      ("n") -> {ok, no};
                      (_) ->   {error, incorrect}
                  end);
            info ->
                io:format(" ~s~n", [Text]),
                {just, info};
%            RC when RC==radiotodo orelse RC==checkbox -> %@todo
%                _Max = show_boxes(Questions, RC, 1),
%                answer2(ReqID, QID, Answers, Questions, RC, 1);
            _ ->
                Prompt = " Type in the answer for this question" ++
                    " as a raw Erlang term:",
                get_term(io_lib:format("~s~n ~p ~n -> ", [Prompt,Question]))
        end,
    case Result of
        nothing ->
            ?UI:request(ReqID, {cancel, QID});
        {just, Answer} ->
            answer1(ReqID, QID, [Answer|Answers], Qs)
    end;
answer1(ReqID, QID, Answers, []) ->
    ?UI:request(ReqID, {reply, QID, lists:reverse(Answers)}).

answer_box(ReqID, QID, Questions, Type) ->
    Min = 1,
    Idx0 = show_boxes(Questions, Type, Min),
    {Init, Err, Idx} =
        case Type of
            radio ->
                {" type the index of your choice: ",
                 "Answer with an index in range",
                 Idx0};
            checkbox ->
                {" type the indices (space delimited): ",
                 "Answer with indices in range",
                 tl(Idx0)}
        end,
    Result =
        get_constrained(
          Init,
          Err,
          fun
              (S) ->
                  {Split,_} = ?MISC:string_split(S, [" "], -1, false, false),
                  Indices0 = [case string:to_integer(M) of
                                {error,_} -> Min-1;
                                {N,_Rest}-> N
                              end || M <- Split ],
                  Indices = lists:usort(Indices0),
                  case (Type==checkbox) andalso ([0] == Indices) of
                      true ->
                          A2 = calc_answer(Questions, [], Idx),
                          {ok, A2};
                      false ->
                        Inrange =
                            ((Type==checkbox) or (length(Indices)==1)) andalso
                            lists:all(fun(I)-> I>=Min andalso I<Min+length(Idx) end, Indices),
                        case Inrange of
                            true  ->
                                A2 = calc_answer(Questions, Indices, Idx),
                                {ok, A2};
                            false ->
                                {error, incorrect}
                        end
                  end
          end),
    case Result of
        nothing ->
            ?UI:request(ReqID, {cancel, QID});
        {just, Answers} ->
            ?UI:request(ReqID, {reply, QID, Answers})
    end.

calc_answer(Questions, AnswerIndices, QuestionIndices) ->
    calc_answer(Questions, AnswerIndices, QuestionIndices, 1, 1).

calc_answer([], _, _, _, _) ->
    [];
calc_answer([Q|Questions], AnswerIndices, QuestionIndices, QII, AII) ->
    case (QuestionIndices==[]) orelse (QII<hd(QuestionIndices)) of
        true ->
            [hd(?MISC:pgetu([format], Q)) |
             calc_answer(Questions, AnswerIndices, QuestionIndices, QII+1, AII)];
        false->
            case (AnswerIndices==[]) orelse (AII<hd(AnswerIndices)) of
                true->
                    [no |
                     calc_answer(Questions, AnswerIndices, tl(QuestionIndices), QII+1, AII+1)];
                false->
                    [yes |
                     calc_answer(Questions, tl(AnswerIndices), tl(QuestionIndices), QII+1, AII+1)]
            end
    end.

show_boxes([Question|Qs], Type=checkbox, Min) ->
    io:format(" 0. (none)~n", []),
    show_boxes([Question|Qs], Type, Min, Min, [0]);
show_boxes([Question|Qs], Type=radio, Min) ->
    show_boxes([Question|Qs], Type, Min, Min, []).
show_boxes([Question|Qs], Type, Num, Idx, Idxs) ->
    [Format,Text] = ?MISC:pgetu([format,text], Question),
    case Format of
        info ->
            io:format(" ~s~n", [Text]),
            show_boxes(Qs, Type, Num, Idx+1, Idxs);
        Type ->
            io:format(" ~p. ~s~n", [Num,Text]),
            show_boxes(Qs, Type, Num+1, Idx+1, [Idx | Idxs])
    end;
show_boxes([], _Type, _Num, _Idx, Idxs)->
    lists:reverse(Idxs).


last_chars(String, N) ->
    K=string:len(String),
    case (0=<N andalso N=<K) of
        true ->
            string:substr(String, K-N+1);
        false ->
            String
    end.

get_term(Prompt) ->
    get_constrained(Prompt,
                    "You have given an invalid term.",
                    fun(Pruned)->
                            String = case last_chars(Pruned,1) of
                                         [$.] -> Pruned;
                                         _    -> Pruned ++ "."
                                     end,
                            ?MISC:string_to_term(String)
                    end).

get_constrained(Prompt, Error, Process) ->
    Raw = io:get_line(Prompt),
    Pruned = ?MISC:strip(Raw),
    case Pruned of
        "" ->
            nothing;
        _ ->
            case Process(Pruned) of
                {ok, Result} ->
                    {just, Result};
                _ ->
                    io:format("~s~n",[Error]),
                    ?MODULE:get_constrained(Prompt, Error, Process)
            end
    end.

spawn_printer_process()->
    case whereis(?PrintProc) of
        undefined ->
            PID = spawn_link(fun global_printer_init/0),
            register(?PrintProc,PID),
            referl_ui_evsend:start(PID);
        PID ->
            PID
    end.

global_printer_init()->
%   ?d(spawn_global_printer), TODO
   receive
      installed ->
%         ?d(connected_global_printer), TODO
         global_printer_process()
   end.

global_printer_process()->
    receive
        {global, statusinfo, [{change, Changes}]} ->
            [case ?MISC:pget([rename,content,present,error],Prop) of
                [[New],_,_,_]->
                    message("renamed "++File++" to "++New);
                [_,_,_,[Err]] when Err =/= [] ->
                    message("error in "++File);
                [_,[true],_,_]->
                    message("modified "++File);
                [_,_,[true],_]->
                    ok;
                [_,_,_,[[]]] ->
                    ok;
                [_,_,[false],_]->
                    message("dropped "++File);
                 _->
                    ?d({debug, Changes})
             end || {File,Prop} <- Changes],
            ?MODULE:global_printer_process();
        _M={global, _, _} ->
%            GlobPrnt = M,
%            ?d(GlobPrnt),
            ?MODULE:global_printer_process();
        M ->
            Msg = "terminating due to unexpected message:",
            io:format(" ERROR: ~p ~s~n ~p~n",[?PrintProc,Msg,M]),
            error
    end.

%% @type mod_file() = atom() | string().
%% @type mod_file_dir_list() = mod_file() | [mod_file_dir_list()].
%% @type eol() = 'cr' | 'lf' | 'crlf' | {eol(), 'eol' | 'noeol'}.
%% @type ri_result() = any(). %% TODO


%% @private
message(Data) ->
    io:format("~s~n", [Data]).

%% @private
message2(Data) ->
    io:format("~s", [Data]).

message3({ok, Data}) ->
    message3(Data);

message3(Data) ->
    Data.

%% @private
error_text(not_ready,[File]) ->
    ["File ", File, " is not ready for refactoring"];
error_text(invalid_regexp,[Rgx]) ->
    ["The regular expression ", io_lib:print(Rgx), " is invalid"];
error_text(invalid_pos,[Pos]) ->
    ["The position ", io_lib:print(Pos), " is invalid"];
error_text(unmatched_regexp,[Rgx]) ->
    ["The regular expression ", Rgx, " yielded no result"];
error_text(unbound_idx_regexp,[Rgx,Idx]) ->
    ["The index ",integer_to_list(Idx),
     " is invalid for the regular expression ", Rgx,
     " in this file"];
error_text(no_file,[]) ->
    ["No file specified"];
error_text(internal_unexpected,[X]) ->
    ["Unexpected return value ",
     io_lib:print(X)];
error_text(load_beam, [String]) ->
    ["BEAM loading failure: ", String];

error_text(ErrType, ErrParams) ->
    ["Unknown error: {",
     io_lib:print(ErrType), ", ", io_lib:print(ErrParams), "}"].


%% @doc Shows brief help text
help() ->
    h().

help(Topic) when is_atom(Topic) ->
    h(Topic).

%% @doc Shows brief help text
h() ->
  message2([
    "The following help topics are available:\n"
    " server   - server management\n"
    " renmov   - rename and move refactorings\n"
    " refac    - other refactorings\n"
    " allrefac - all refactorings\n"
    " regexp   - regexp syntax information\n"
    " usage    - a few words about basic usage\n"
    "\n"
    "call " ++ io_lib:write(?MODULE) ++ ":h(Item) for"
    " a brief description of one of the items above.\n"
    "You can also give one of the exported functions to get help on that.\n"
  ]).

%% @doc Shows brief help text on a topic or function
h(all) ->
    h(),
    lists:foreach(fun h/1, [usage,refac,server,regexp]);
h(Topic) when is_atom(Topic) ->
    try
        message2(ht(Topic))
    catch
        error:function_clause ->
            try
                Name = list_to_atom(atom_to_list(Topic)++"_h"),
                apply(?MODULE,Name,[])
            catch
                error:undef ->
                    message("The given help topic cannot be found!\n"),
                    h()
            end
    end;
h(_) ->
    message("Invalid argument!\n"),
    h().

ht(usage) ->
  [ "You first need to add some files to the database with"
    " the add/1 command. Then you can commit transformations on them,"
    " by referring to them by either module name or filename.\n" ];

ht(renmov) ->
  [ "movfun(from_mod,to_mod,[{f,2},{g,0}])"
    " - move functions to another module\n"
    "movrec(from_mod,to_mod,[rec1,rec2])"
    " - move records to another module\n"
    "movmac(from_mod,to_mod,[\"Mac1\",\"Mac2\"])"
    " - move macro to another module\n"
    "renfld(mod_or_file,rec,oldfield1,newfield1)"
    " - rename record field\n"
    "renfun(mod_or_file,{func,2},newfun)"
    " - rename function\n"
    "renrec(mod_or_file,oldrec,newrec)"
    " - rename record\n"
    "renmac(mod_or_file,\"OldMac\",\"NewMac\")"
    " - rename macro\n"
    "renmod(mod_or_file, newmod)"
    " - rename module\n"
    "renhrl(\"old.hrl\", \"new.hrl\")"
    " - rename header file\n"
    "renvar(mod_or_file, \"X=\", \"NewVar\")"
    " - rename variable\n"
  ];

ht(refac) ->
  [
    "elimvar(mod_or_file,\"X=\")"
    " - eliminate variable\n"
    "extfun(mod_or_file,\"A+B\",newfunc)"
    " - extract function\n"
    "expfun(mod_or_file, \"fun g\")"
    " - expand implicit funexpression to function call\n"
    "genfun(mod_or_file, \"[+]<2\", \"NewArg\")"
    " - generalize function with new argument\n"
    "inlfun(mod_or_file,\"f\\(1\")"
    " - inline function at application\n"
    "inlmac(mod_or_file,\"?Mac\")"
    " - inline macro at use\n"
    "intrec(mod_or_file, \"{X,Y}\", newrec, [f1, f2])"
    " - introduce record in place of tuple\n"
    "merge(mod_or_file,\"1+2\",\"NewVar\")"
    " - merge common subexpressions into a new variable\n"
    "reorder(mod_or_file,{func,2},[2,1])"
    " - reorder function arguments\n"
    "tupfun(mod_or_file,\"A,B\")"
    " - change function arguments into tuple\n"
    "upregex()"
    " - upgrade regexp from \"regexp\" module to \"re\" module usage\n"
  ];

ht(allrefac) ->
    ht(refac) ++ ht(renmov);

ht(server) ->
  [ "add(X)   - add a module, file, directory or a list of these"
    " to the database\n"
    "drop(X)  - drop (remove) like in add/1\n"
    "save(X)  - save from the database to physical file(s)\n"
    "ls()     - list database contents\n"
    "ls(FM)   - list forms in the file or module\n"
    "cat(FM)  - prints the contents of a file or module from the database\n"
    "cat(FM,RM)  - prints the definition of a macro or record\n"
    "cat(FM,F,A) - prints the definition of a function\n"
    "backup() - creates a backup from the current state of the graph\n"
    "backup(CommitLog) - creates a backup as ri:backup/0,\n"
    "but here the user can attach a commit log to the backup file\n"
    "ls_backups() - returns a lists of backups, that has been created before with\n"
    "ri:backup/0 or ri:backup/1\n"
    "backup_info(Backup) - returns information about the given backup\n"
    "restore(Backup) - restores the given backup\n"
    "undo()   - undo the transformation (rollback, only once!)\n"
    "clean() - removes all backups that belongs to the actual graph\n"
    "reset()  - reset the database to an empty state, but valid schema\n"
    "graph(T) - assume no options and call graph/2\n"
    "graph(Atom,Opt) - assume \".dot\" extension and call graph/2\n"
    "graph(File,Opt) - draw the database graph with the given options\n"
    "svg/0,1,2 - calls graph and generates SVG output\n"
    "create_graph(Name) - creates a graph with the given name\n"
    "rename_graph(OldName, NewName) - renames a graph that has the given OldName,\n"
    "with the given NewName\n"
    "ls_graphs() - returns a list of the created graphs\n"
    "actual_graph() - returns the actual graph's name\n"
    "load_graph(Name) - loads the given graph\n"
    "delete_graph(Name) - removes the given graph\n"
    "delete_all_graphs() - removes all graphs\n"
    "gn(Type,Idx) - returns the data of a graph node\n"
    "getcfg() - display the current settings that can be set via setcfg/3\n"
    "setcfg(AppDirs,IncDirs,OutDir) - save settings\n"
  ];

ht(regexp) ->
  [ "You can substitute regexp in place "
    "of positions and posranges like so:\n"
    "* a plain regexp matches precisely a posrange or "
    "marks the start of a position;\n"
    "* use plain angle brackets '<' and '>' to highlight"
    " part of a regexp;\n"
    "* to get angle bracket characters instead,"
    " escape them like \\< and \\>;\n"
    "* the nth match can be selected instead of the first one "
    "by substituting the tuple {\"regexp\",index1} for \"regexp\".\n"
  ].

%%% ============================================================================
%%% Refactorings

extfun_h() ->
    message("extfun(ModFile,Range_of_body,NewFunc)").

%% @doc Extract function refactoring
extfun(File, Range, Name)
  when is_atom(Name), (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)) ->
    transformr(extract_fun, File, Range, [{name, Name}]);
extfun(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, atom())"),
    usage.
% ri:extfun(mod_or_file,"A+B",f).


merge_h() ->
    message("merge(ModFile,Range_of_expression,NewVar)").

%% @doc Merge common subexpressions refactoring
merge(File, Range, Varname=[C|_]) when is_integer(C),
        (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)) ->
    transformr(merge, File, Range, [{varname, Varname}]);
merge(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, string())"),
    usage.

% ri:merge(mod_or_file,"1+2","V").

inlfun_h() ->
    message("inlfun(ModFile,Pos_of_fun_application)").

%% @doc Inline function refactoring
inlfun(File, Pos) when (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)) ->
    transformp(inline_fun, File, Pos, []);
inlfun(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()})"),
    usage.

% ri:inlfun(mod_or_file,"f[(]1").

inlmac_h() ->
    message("inlmac(ModFile,Pos_of_macro_use)").

%% @doc Inline macro refactoring
inlmac(File, Pos) when (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)) ->
    transformp(inline_mac, File, Pos, []);
inlmac(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()})"),
    usage.
% ri:inlmac(mod_or_file,"?Mac").

tupfun_h() ->
    message("tupfun(ModFile,Range_of_arguments)").

%% @doc Tuple function arguments refactoring
tupfun(File, Range) when (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)) ->
    transformr(tuple_funpar, File, Range, []);
tupfun(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()})"),
    usage.
%%@todo :wishlist Begin,End -> name/arity first last
% ri:tupfun(mod_or_file,"A,B").

%%Not recommended
%reorder(File, Fun, Arity, Order) ->
%    reorder(File, {Fun,Arity}, Order).

reorder_h() ->
    message("reorder(ModFile,{Fun,Arity},PermutationList)").
%@todo :wishlist check Order

%% @doc Reorder function arguments refactoring
reorder(File, {Fun,Arity}, Order=[I|_])
  when (is_atom(File) or is_list(File)), is_atom(Fun), is_integer(Arity), is_integer(I) ->
    transform_catch(reorder_funpar, File,
              [{function, Fun}, {arity, Arity}, {order, Order}]);
reorder(_,_,_)->
    message("::(modfile(), {atom(), natural()}, [positive()])"),
    usage.
% ri:reorder(mod_or_file,{f,2},[2,1]).

reorder(File)
  when (is_atom(File) or is_list(File)) ->
    transform_catch(reorder_funpar, File, []);
reorder(_)->
    message("::(modfile())"),
    usage.
% ri:reorder(mod_or_file).




genspec_h() ->
    message("genspec(ModFile,{Fun,Arity})").

%% @doc Generate function specification refactoring
genspec(File, {Fun,Arity})
  when (is_atom(File) or is_list(File)), is_atom(Fun), is_integer(Arity) ->
    transform_catch(genspec, File,
              [{function, Fun}, {arity, Arity}]);
genspec(_,_)->
    message("::(modfile(), {atom(), natural()})"),
    usage.






expfun_h() ->
    message("expfun(ModFile,Pos_of_funexpr)").

%% @doc Expand implicit fun expression refactoring
expfun(File, Pos) when (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)) ->
    transformp(expand_funexpr, File, Pos, []);
expfun(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()})"),
    usage.
% ri:expfun(mod_or_file, "fun g").

movfun_h() ->
    message("movfun(Source,Target,FunctionList=[{FunctionName,Arity}|_])").

%% @doc Move function refactoring
movfun(Source, Target, Fun={A,I})
  when is_atom(A), is_integer(I) ->
    movfun(Source, Target, [Fun]);
% ri:movfun(mod_or_file,b,{f,2}).

%%@todo :wishlist check Funlist
movfun(Source, Target, Funlist=[{A,I}|_]) when
        (is_atom(Source) or is_list(Source)),
        (is_atom(Target) or is_list(Target)),
        is_atom(A), is_integer(I) ->
    transform2(move_fun, Source, Target, [{funlist, Funlist}]);
movfun(_,_,_)->
    message("::(modfile(), modfile(), [{atom(), natural()}])"),
    usage.
% ri:movfun(mod_or_file,b,[{f,2},{g,0}]).

movrec_h() ->
    message("movrec(Source,Target,RecordList=[Record|_])").

%% @doc Move record refactoring
movrec(Source, Target, Rec) when
        is_atom(Rec) ->
    movrec(Source, Target, [Rec]);
% ri:movrec(mod_or_file,b,rec).

%%@todo :wishlist check Reclist
movrec(Source, Target, Reclist=[A|_]) when
        (is_atom(Source) or is_list(Source)),
        (is_atom(Target) or is_list(Target)),
        is_atom(A) ->
    transform2(move_rec, Source, Target, [{reclist, Reclist}]);
movrec(_,_,_)->
    message("::(modfile(), modfile(), [atom()])"),
    usage.
% ri:movrec(mod_or_file,b,[r1,r2]).

movmac_h() ->
    message("movmac(Source,Target,MacroList=[Macro|_])").

%% @doc Move macro refactoring
movmac(Source, Target, Mac=[C|_]) when
        is_integer(C) ->
    movmac(Source, Target, [Mac]);
% ri:movmac(mod_or_file,b,"Mac").

%%@todo :wishlist check Maclist
movmac(Source, Target, Maclist=[[C|_]|_]) when
        (is_atom(Source) or is_list(Source)),
        (is_atom(Target) or is_list(Target)),
        is_integer(C) ->
    transform2(move_mac, Source, Target, [{maclist, Maclist}]);
movmac(_,_,_)->
    message("::(modfile(), modfile(), [string()])"),
    usage.
% ri:movmac(mod_or_file,b,["Mac1","Mac2"]).

genfun_h() ->
    message("genfun(ModFile,Range_of_body,NewVar)").

%% @doc Generalize function by new argument refactoring
genfun(File, Range, Newname=[C|_]) when
        (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)),
        is_integer(C) ->
    transformr(gen, File, Range, [{varname, Newname}]);
genfun(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, string())"),
    usage.

% ri:genfun(mod_or_file, "[+]\\(2", "Two").

%%Not recommended
%renfun(File, Fun, Arity, Newname) ->
%    renfun(File, {Fun,Arity}, Newname).

renfun_h() ->
    message("renfun(ModFile,{FunctionName,Arity},NewFun)").

%% @doc Rename function refactoring
renfun(File, {Fun,Arity}, Newname) when
        (is_atom(File) or is_list(File)),
        is_atom(Fun), is_integer(Arity), is_atom(Newname) ->
    transform_catch(rename_fun, File,
              [{function, Fun}, {arity, Arity}, {name, Newname}]);
renfun(_,_,_)->
    message("::(modfile(), {atom(), natural()}, atom())"),
    usage.


% todo Uncomment the functions when they are ready.
% todo Do not use (half-)Hungarian names.
% rec_syner_h() ->
%     message("rec_syner(ModFile, [Records])").
%
% %% @doc Change the record syntax from tuple syntax to record syntax.
% rec_syner(File, RecordsList) when
%       (is_atom(File) or is_list(File)),
%       is_list(RecordsList) ->
%     transform_catch(recordos, File,  [{reclist, RecordsList}]).

renvar_h() ->
    message("renvar(ModFile,Pos_of_variable,NewVar)").

%% @doc Rename variable refactoring
renvar(File, Pos, Newname=[C|_]) when
        (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)),
        is_integer(C) ->
    transformp(rename_var, File, Pos, [{varname, Newname}]);
renvar(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()}, string())"),
    usage.

% ri:renvar(mod_or_file, "X=", "V").

renuvars_h() ->
    message("renuvars(ModFile)").

%% @doc Rename unused variables refactoring
renuvars(Module) when
	(is_atom(Module) or is_list(Module))
	-> transform_catch(rename_unused_vars,Module, []);
renuvars(_)  ->
    message("::(modfile())"),
    usage.

%% @doc Rename overloaded functions refactoring
renofuns_h() ->
    message("renofuns(ModFile, OldName, NewName)").
renofuns(File, Oldname, Newname) when
        (is_atom(File) or is_list(File)),
        is_atom(Oldname),
	is_atom(Newname) ->
		transform_catch(rename_overloaded_funs, File,
          	[{funname, Oldname}, {name, Newname}]);
%%	  is_tuple(Oldname) ->
%%		(Atom, Pos) = Oldname,
%%		transform_catch(rename_overloaded_funs, File,
%%        	[{Atom, Oldname}, {name, Newname}])
%%	end.
renofuns(_,_,_) ->
    message("::(modfile(), atom(), atom())"),
    usage.

renmod_h() ->
    message("renmod(OldModFile,NewMod)").
%% @doc Rename module refactoring
renmod(File, Newname) when
        (is_atom(File) or is_list(File)),
        is_atom(Newname) ->
    transform_catch(rename_mod, File, [{name, Newname}]);
renmod(_,_)->
    message("::(modfile(), atom())"),
    usage.

% ri:renmod(mod_or_file, newmod).

renhrl_h() ->
    message("renhrl(OldHrl,NewHrl)").

%% @doc Rename header refactoring
renhrl(File, Newname=[C|_]) when
        (is_atom(File) or is_list(File)), % note: can't be an atom...
        is_integer(C) ->
    transform_catch(rename_header, File, [{name, Newname}]);
renhrl(_,_)->
    message("::(modfile(), string())"),
    usage.

% ri:renhrl("a.hrl", "b.hrl").

renrec_h() ->
    message("renrec(ModFile,OldRecord,NewRecord)").

%% @doc Rename record refactoring
renrec(File,Record,NewName) when
        (is_atom(File) or is_list(File)),
        is_atom(Record), is_atom(NewName) ->
    transform_catch(rename_rec, File,
              [{record, Record}, {name, NewName}]);
renrec(_,_,_)->
    message("::(modfile(), atom(), atom())"),
    usage.


% ri:renrec(mod_or_file,recname,newrecname).

renfld_h() ->
    message("renfld(ModFile,Record,OldField,NewField)").

%% @doc Rename record field refactoring
renfld(File,Record,Field,NewName) when
        (is_atom(File) or is_list(File)),
        is_atom(Record), is_atom(Field), is_atom(NewName) ->
    transform_catch(rename_recfield, File,
              [{record,Record}, {recfield,Field}, {name,NewName}]);
renfld(_,_,_,_)->
    message("::(modfile(), atom(), atom(), atom())"),
    usage.


% ri:renfld(mod_or_file,recname,field1,newfield1).

elimvar_h() ->
    message("elimvar(ModFile,Pos_of_variable)").

%% @doc Eliminate variable by inlining refactoring
elimvar(File, Pos) when
        (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)) ->
    transformp(elim_var, File, Pos, []);
elimvar(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()})"),
    usage.


% ri:elimvar(mod_or_file,"X=").

%%Not recommended
%intrec(File, A, B, Newname, Fields) ->
%    intrec(File, {A,B}, Newname, Fields).

intrec_h() ->
    message("intrec(ModFile,Range_of_tuple,NewRecord,Fields=[field|_])").

%% @doc Introduce record in place of a tuple refactoring
intrec(File, URange, Newname, AFields=[A|_])
  when is_atom(A) ->
    SList = lists:map(fun atom_to_list/1, AFields),
    Fields = string:join(SList," "),
    intrec(File, URange, Newname, Fields);
% ri:intrec(mod_or_file, "{X,Y}", newrec, [f1, f2]).

intrec(File, Range, Newname, Fields=[C|_]) when
        (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)),
        is_atom(Newname), is_integer(C) ->
    transformr(introduce_rec, File, Range,
               [{name,Newname}, {text,Fields}]);
intrec(_,_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, atom(), [atom()])"),
    usage.


% ri:intrec(mod_or_file, "{X,Y}", rec, "f1 f2"]).

renmac_h() ->
    message("renmac(ModFile,OldMacro,NewMacro)").

%% @doc Rename macro refactoring
renmac(File, Macro, Newname) when
        (is_atom(File) or is_list(File)),
        (is_atom(Macro) or is_list(Macro)),
        (is_atom(Newname) or is_list(Newname)) ->
    transform_catch(rename_mac, File, [{macro,Macro}, {macname,Newname}]);
renmac(_,_,_)->
    message("::(modfile(), atom() | string(), atom() | string())"),
    usage.
% ri:renmac(mod_or_file,"Macname","NewMecname").

%intimp(File) ->
%    transform_catch(introduce_import, File, []).

upregex_h() ->
    message("upregex()").

%% @doc Upgrade regular expression syntax refactoring
upregex() ->
    catch_referr(fun()->
        transform(upgrade_regexp,[])
    end).

appfuncl_h()->
    message("appfuncl(Clusters)").

%% @doc Apply function clustering results refactoring
appfuncl(Clusters)->
    catch_referr(fun()->
                         transform(apply_funcluster,
                                   [{funclusters,Clusters}])
                 end).

metricmode_h()->
    message("metricmode(on), metricmode(off), metricmode(show)").

%% @doc Set the metric mode on/of/shows bad smells
metricmode()->
    refanal_metrics_helper:metricmode().
metricmode(on) ->
    refanal_metrics_helper:metricmode(on, ri);
metricmode(off) ->
    refanal_metrics_helper:metricmode(off);
metricmode(show) ->
    case refanal_metrics_helper:metricmode() of
          true -> refanal_metrics_helper:all_metric_errors(ri), ok;
         false -> refanal_metrics_helper:metricmode(on, ri),
		  metricmode(show),
		  ok
    end;
metricmode(_)->
    message("::on | off | show"),
    usage.

metric_h()->
    message("metric(Query)").

%% @doc Run a metric query
metric(Query) when is_atom(Query)->
    metric(atom_to_list(Query));
metric(Query) when is_list(Query)->
    transform_(metric_query,
               [{querys,Query}],
               fun(Results)->
                   [Result] = ?MISC:pgetu([result],Results),
                   message(Result),
                   ok
               end);
metric(_)->
    message("::(atom() | string())"),
    usage.

cluster_h()->
    message(["cluster()\n"
             "cluster(Algorithm,EntityType)"]).

cluster()->
    cluster_([]).

cluster(Algorithm,EntityType)->
    cluster_([{algorithm,Algorithm},{entity,EntityType}]).

cluster_(Args)->
    transform_(clustering,
               Args,
               fun(_)->result end).

q_h()->
    message2(["q(SemanticQuery)\n"
              "q(ModFile,SemanticQuery)\n"
              "q(ModFile,PositionRegexp,SemanticQuery)\n"
              "q(SemanticQuery,Options)\n"
              "q(ModFile,SemanticQuery,Options)\n"
              "q(ModFile,PositionRegexp,SemanticQuery,Options)\n"
              "Example usage:\n"
              " ri:q(\"mods.funs.name\").\n"
              " ri:q(mod1, \"f\\\\(X, Y\\\\)\", \"@fun.var\").\n"
              " ri:q(\"mods.funs\",[linenum,{out,\"result.txt\"}]).\n"]).

%% @doc Run a semantic query
q(Q) when is_atom(Q)->
    q(atom_to_list(Q));
q(Q=[C|_]) when is_integer(C)->
    q_(Q, [], addqnone());
q(_)->
    message("::(atom() | string())"),
    usage.

%% @doc Run a semantic query starting from the given file
q(ModFile,Q) when (is_atom(ModFile) or is_list(ModFile)), is_atom(Q)->
    q(ModFile,atom_to_list(Q));
q(ModFile,Q=[C|_]) when (is_atom(ModFile) or is_list(ModFile)), is_integer(C)->
    q_(Q,[],addqmod(ModFile));

q(Q, Options=[O|_]) when is_atom(Q), (is_atom(O) orelse is_tuple(O))->
    q(atom_to_list(Q),Options);
q(Q=[C|_], Options=[O|_]) when is_integer(C), (is_atom(O) orelse is_tuple(O))->
    q_(Q,Options,addqnone());
q(_, _)->
    message("::(modfile() | atom() | string(), atom() | string() | proplist())"),
    usage.

%% @doc Run a semantic query starting from the given position
q(ModFile,Pos=[A|_],Q)
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(A), is_atom(Q)->
    q(ModFile,Pos,atom_to_list(Q));
q(ModFile,Pos=[A|_],Q=[B|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(A), is_integer(B)->
    q_(Q,[],addqmodpos(ModFile,Pos));

q(ModFile,Q,Options=[O|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_atom(Q), (is_atom(O) orelse is_tuple(O)) ->
    q(ModFile,atom_to_list(Q),Options);
q(ModFile,Q=[C|_], Options=[O|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(C), (is_atom(O) orelse is_tuple(O)) ->
    q_(Q,Options,addqmod(ModFile));
q(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()} | atom() | string(), atom() | string() | proplist())"),
    usage.

%% @doc Run a semantic query starting from the given position with options
q(ModFile,Pos=[A|_],Q,Options=[O|_]) when
        (is_atom(ModFile) or is_list(ModFile)),
        is_integer(A), is_atom(Q),
        (is_atom(O) orelse is_tuple(O)) ->
    q(ModFile,Pos,atom_to_list(Q),Options);
q(ModFile,Pos=[A|_],Q=[B|_], Options=[O|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(A), is_integer(B),
       (is_atom(O) orelse is_tuple(O)) ->
    q_(Q,Options,addqmodpos(ModFile,Pos));
q(ModFile, _Pos={position, P}, Q=[B|_], Options=[O|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(P), is_integer(B),
       (is_atom(O) orelse is_tuple(O)) ->
    q_(Q,Options,addqmodpos(ModFile,P));
q(_,_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, atom() | string(), proplist())"),
    usage.

addqnone()->
    fun()->
        []
    end.

addqmod(ModFile)->
    fun()->
        [{file,mod2filec(ModFile)}]
    end.

addqmodpos(ModFile,Pos)->
    fun()->
        File = mod2filec(ModFile),
        [{file,File},
        {position,to_pos(File,Pos)}]
    end.

q_(Query=[Ch|_],Options,StartFun)
  when is_integer(Ch), is_list(Options), is_function(StartFun,0) ->
    catch_referr(
      fun()->
              Start = StartFun(),
              DispOpt =
                  [case E of
                       {out,_} ->
                           [];
                       {linenum,true}->
                           [{positions,linecol}];
                       _ ->
                           message("warning: unknown option '"++io_lib:print(E)++"'"),
                           []
                   end || E <- proplists:unfold(Options)],
              Args = [{display_opt,lists:append(DispOpt)},
                      {start_opt,Start},
                      {querystr,Query}],
              transform_(semantic_query,Args,
                  fun(Results)->
                    [Result] = ?MISC:pgetu([result],Results),
                    case proplists:lookup_all(out,Options) of
                        []->
                            io:put_chars(Result);
                        [{out,FileName}]->
                            {ok,IODev} = file:open(FileName, [write]),
                            io:put_chars(IODev, Result),
                            ok = file:close(IODev)
                    end,
                    ok
                  end)
      end).

%%% ============================================================================
%%% Others

%cluster_agglom() -> %%@todo :wishlist

%cluster_genetic() -> %%@todo :wishlist

%%% ============================================================================
%%% Server management

%% @private
get_filenode(ModFile) ->
    File = mod2file(ModFile),
    ?Query:exec1(?File:find(File),?RefError(file_not_present,[File])).

%% @private
get_modnode(ModFile) ->
    Mod = file2mod(ModFile),
    ?Query:exec1(?Mod:find(Mod),?RefError(mod_not_found,[Mod])).

cat_h() ->
    message(["cat(ModFile)\n",
             "cat(ModFile,RecMac)\n"
             "cat(ModFile,FunName,Arity)"]).

%% @doc Display the contents of the given file or module
cat(ModFile) when
        (is_atom(ModFile) or is_list(ModFile)) ->
    catch_referr(fun() ->
        message(?Syn:tree_text(get_filenode(ModFile))) end);
cat(_)->
    message("::(modfile())"),
    usage.

%% @doc Display the definition of the given record or macro
cat(ModFile, {FunName,Arity}) when
        (is_atom(ModFile) or is_list(ModFile)),
        is_atom(FunName), is_integer(Arity)->
    cat(ModFile, FunName, Arity);
cat(ModFile, RecMac) when
        (is_atom(ModFile) or is_list(ModFile)),
        (is_atom(RecMac) or is_list(RecMac)) ->
    catch_referr(fun() ->
        message2(recmactext(ModFile, RecMac))
    end);
cat(_,_)->
    message("::(modfile(), {atom(), natural()} | atom() | string())"),
    usage.

%% @doc Display a function definition
cat(ModFile, FunName, Arity) when
        (is_atom(ModFile) or is_list(ModFile)),
        is_atom(FunName), is_integer(Arity)->
    catch_referr(fun() ->
        message2(funtext(ModFile, FunName, Arity))
    end);
cat(_,_,_)->
    message("::(modfile(), atom(), natural())"),
    usage.

%% @private
recmactext(ModFile,RecMac)->
    File = get_filenode(ModFile),
    QE   = fun(L)-> ?Query:exec([File], ?Query:seq(L)) end,
    Obj  =
        case QE([?Rec:find(RecMac),?Rec:form()]) of
             [] ->
                 M = QE([?Macro:find(RecMac)]),
                 case (M==[]) and (is_atom(RecMac)) of
                     true  -> QE([?Macro:find(io_lib:write(RecMac))]);
                     false -> M
                 end;
             R  -> R
        end,
    case Obj of
        []  -> throw(?RefError(mac_not_found,[RecMac]));
        [O] -> ?Syn:tree_text(O)
    end.

%% @private
funtext(ModFile, FunName, Arity) ->
    Mod = get_modnode(ModFile),
    Fun = ?Query:exec1([Mod],
                       ?Query:seq([?Fun:find(FunName,Arity),
                                   ?Fun:definition()]),
                       ?RefError(fun_not_found,[FunName,Arity])),
    ?Syn:tree_text(Fun).

%% -----------------------------------------------------------------------------

getcfg_h() ->
    message("getcfg()").

%% @doc Display environment variables (deprecated)
%% @todo delete
getcfg() ->
    Config = ui({showconfig}),
    message(?MISC:any_to_string(Config)). % @todo

setcfg_h() ->
    message("setcfg(AppDirs,IncDirs,OutDir)").

%% @doc Configure directories (deprecated)
%% @todo delete
setcfg(AppDirs,IncDirs,OutDir) ->
    Success = ui({saveconfig,AppDirs,IncDirs,OutDir}),
    message(?MISC:any_to_string(Success)). % @todo

envs_h()->
    message("envs()").

%% @doc Lists all environment nodes
envs()->
    {ok, Envs} = ui({get_envs}),
    case Envs of
        [] ->
            message("no environment variable set");
        L ->
            message2(lists:map(fun show_env/1, L))
    end.

env_h()->
    message(["env(Variable)\n"
             "env(Variable,SubKey)"]).
%% @doc Lists a specific environment node
env(Name)->
    message2(show_env({Name,?Syn:get_env(Name)})).

%#env{name=env_var, value=[{EnvName, EnvVal}]}

addenv_h()->
    message("addenv(Variable,NewValue)"
            "addenv(Variable,SubKey,NewValue)").

addenv(EnvName, EnvVal) when is_atom(EnvName)->
    ?Syn:add_env(EnvName, EnvVal).

%% @doc Adds a new subkey to a proplist environment node
addenv(Name, EnvName, EnvVal) when is_atom(EnvName)->
    addenv(Name, atom_to_list(EnvName), EnvVal);
addenv(Name, EnvName, EnvVal) when is_atom(EnvVal)->
    addenv(Name, EnvName, atom_to_list(EnvVal));
addenv(Name, EnvName, EnvVal)->
    case ?Syn:env_type(Name) of
        proplist ->
            [_|_] = ?Syn:add_env(Name, {EnvName, EnvVal}),
            ok;
        atomic ->
            message("error: type of '"++
                    ?MISC:any_to_string(Name)++"' is not a proplist")
    end.

delenv_h()->
    message(["delenv(Variable)\n"
             "delenv(Variable,SubKey)\n"
             "delenv(Variable,SubKey,OldValue)"]).

%% @doc Deletes a subkey from a proplist environment node
delenv(Name, EnvName) when is_atom(EnvName)->
    delenv(Name,atom_to_list(EnvName));
delenv(Name, EnvName)->
    case ?Syn:env_type(Name) of
        proplist ->
            case ?Syn:del_env_sub(Name,EnvName) of
                [] -> message("key not found"), fail;
                [_|_] -> ok
            end;
        atomic ->
            message("error: type of '"++
                    ?MISC:any_to_string(Name)++"' is not a proplist"),
            fail
    end.

%% @doc Deletes a specific key-value pair from a proplist environmnent node
delenv(Name, EnvName, EnvVal) when is_atom(EnvName)->
    delenv(Name, atom_to_list(EnvName), EnvVal);
delenv(Name, EnvName, EnvVal) when is_atom(EnvVal)->
    delenv(Name, EnvName, atom_to_list(EnvVal));
delenv(Name, EnvName, EnvVal)->
    case ?Syn:env_type(Name) of
        proplist ->
            case ?Syn:del_env_val(Name,{EnvName,EnvVal}) of
                [] -> message("error: key-value pair not found"), fail;
                [_|_] -> ok
            end;
        atomic ->
            message("error: type of '"++
                    ?MISC:any_to_string(Name)++"' is not a proplist"),
            fail
    end.

%% @doc Lists the value for a subkey of a proplist environment node
env(Name, EnvName) when is_atom(EnvName)->
    env(Name, atom_to_list(EnvName));
env(Name, EnvName)->
    case ?Syn:env_type(Name) of
        proplist ->
            case ?Syn:get_env(Name,EnvName) of
                L=[_|_] ->
                    message2(show_env({Name,[{EnvName,EV} || EV <- L]}));
                [] ->
                    message("error: Environment variable sub-key not found"),
                    fail
            end;
        atomic ->
            message("error: type of '"++
                    ?MISC:any_to_string(Name)++"' is not a proplist"),
            fail
    end.

%#env{name=output}
%#env{name=backup}

setenv_h()->
    message("setenv(Variable,Value)").

%% @doc Sets the value of an environment node
%% @todo setenv/3
setenv(Name, Value)->
    case ?Syn:env_type(Name) of
        atomic ->
            ?Syn:set_env(Name,Value),
            ok;
        proplist ->
            message("Please use addenv/3, delenv/2 and delenv/3 instead."),
            fail
    end.

%% @doc Deletes an environment node
delenv(Name)->
    case ?Syn:del_env(Name) of
        [] -> message("not found"), fail;
        [_|_] -> ok
    end.

show_env({_Name,[]})->
    "error: Environment variable not found\n";
show_env({Name,Values})->
    case ?Syn:env_type(Name) of
        proplist ->
            [io_lib:format("~p:~n", [Name]),
             [io_lib:format(" ~p = ~p~n",[K,V]) || {K,V} <- Values]];
        atomic ->
            [io_lib:format("~p = ~p~n", [Name,Value]) || Value <- Values]
    end.

%% @doc Returns the names of the files
%% that are described in the named environment.
%% @equiv refcore_syntax:get_env(Name)
%% @todo delete
dirs_by_env(Name) ->
    [(?Graph:data(Dir))#env.value ||
        Dir <- ?Graph:path(?Graph:root(), [{env, {name, '==', Name}}])].

%% -----------------------------------------------------------------------------

%% @doc Generates html for a file or directory or a list of these.
generate(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun generate_/1,L);

generate(X) -> generate_(X).

generate_(File=[C|_]) when is_integer(C) ->
    case io_lib:deep_char_list(File) of
        true ->
            ui({generate_dir,File}),
			ok;
        false ->
            message("Error: bad file argument given"),
            error
    end;

generate_(_)->
    message("::(file() | [file()])"),
    usage.

%% @doc Adds a custom query for a specific node type.
addQuery(Type,Query) -> referl_htmlserver:addQuery(Type,Query).

%% -----------------------------------------------------------------------------

add_h() ->
    message2(["add(ModFileDirList)\n"
              "add(AppBase, App)"]).

%% @doc Adds an application to the database.
add(AppBase, App) ->
    AppBaseS = ?MISC:to_list(AppBase),
    AppS     = ?MISC:to_list(App),
    Bases = [AB || AB <- dirs_by_env(appbase),
                   {match, _} <- [re:run(AB, AppBaseS)]],
    case dir_to_load(Bases, AppBaseS, AppS) of
        {no_dir, Msg} ->
            Msg;
        Dir ->
            add_src_dir(Dir)
    end.

-spec dir_to_load([string()], string(), string())
            -> string() | {no_dir, Cause :: atom()}.
%% Returns name of the directory to load as a string or {no_dir, Cause}.
%% Preference is given to an exactly matching directory name,
%% and a higher version number.
dir_to_load([], AppBaseS, _) ->
    message("Application base " ++ AppBaseS ++ " not found"),
    {no_dir, missing_appbase};
dir_to_load(Bases, AppBaseS, AppS) ->
    AppNotFound = "Application " ++ AppS ++ " not found under " ++ AppBaseS,
    try
        Bases =/= [] orelse
            throw(empty_bases),
        DirMatches = lists:concat([get_dirs(AppS, Base) || Base <- Bases]),
        DirMatches =/= [] orelse
            throw(no_matching_dir),
        ExactMatchDirs = [Dir || {exact, Dir} <- DirMatches],
        ExactMatchDirs == [] orelse
            throw({has_exact_matches, ExactMatchDirs}),
        PrefixMatchDirs = [Dir || {prefix, Dir} <- DirMatches],
        PrefixMatchDirs =/= [] orelse
            throw({no_matches_at_all, DirMatches}),
        lists:last(PrefixMatchDirs)
    catch
        {has_exact_matches, ExactMatchDirs2} ->
            lists:last(ExactMatchDirs2);
        no_matching_dir ->
            message(AppNotFound),
            {no_dir, not_found};
        {no_matches_at_all, DirMatches2} ->
            message(AppNotFound),
            io:format("Applications under :~n" ++ AppBaseS),
            [io:format("    ~s~n", [Dir]) || Dir <- DirMatches2],
            {no_dir, not_found}
    end.


%% Tries to find a directory for the application name `AppS'.
%% This succeeds if either there is a directory called `AppS' (with an optional version number),
%% or there is at least one directory whose name begins with `AppS'.
%% In these cases, a non-empty list is returned with elements {exact, Dir}
%% and {prefix, Dir}, respectively.
get_dirs(AppS, Base) ->
    BaseApp    = filename:join(Base, AppS),
    IsExactDir = filelib:is_dir(BaseApp),
    VsnDirs    = filelib:wildcard(filename:join(Base, AppS ++ "-*")),
    PrefixDirs = filelib:wildcard(filename:join(Base, AppS ++ "*")),
    case {IsExactDir, VsnDirs, PrefixDirs} of
        {true, _, _} ->
            [{exact, BaseApp}];
        {false, [_|_], _} ->
            [{exact, filename:join(Base, Dir)} || Dir <- VsnDirs];
        _ ->
            [{prefix, filename:join(Base, Dir)} || Dir <- PrefixDirs]
    end.

add_src_dir(Dir) ->
    SrcDir = filename:join(Dir, "src"),
    message("Adding: " ++ SrcDir),
    with_db_save(add(SrcDir)).

%% Saves the database whether or not an error was encountered.
with_db_save(Result) ->
    ?Graph:save(database),
    Result.

%% Returns whether the result of a transformation is (partly) successful.
% result_is_ok(deny)                -> false;
% result_is_ok(abort)               -> false;
% result_is_ok(error)               -> false;
% result_is_ok({error, _})          -> false;
% result_is_ok(Xs) when is_list(Xs) -> lists:any(fun result_is_ok/1, Xs);
% result_is_ok(_)                   -> true.

%% @doc Add either a module, a file, a directory or a list of these
add(L=[H|_]) when not is_integer(H) ->
    with_db_save(lists:foreach(fun add_/1,L));
add(X) ->
    with_db_save(catch_referr(
      fun()->
              case add_(X) of
                  error -> error;
                  _     -> ok
              end
      end)).

add_(Mod) when is_atom(Mod) ->
    try
        add_(mod2file(Mod,true))
    catch
        ?RefError(mod_not_found,[Mod]) ->
            N = atom_to_list(Mod),
            add_if_exists(N++".erl") orelse
            add_if_exists(N++".beam") orelse
            begin
                message("Error: no matching .erl or .beam"),
                error
            end
    end;
add_(File=[C|_]) when is_integer(C) ->
    case io_lib:deep_char_list(File) of
        true ->
            add_file(File);
        false ->
            message("Error: bad file argument given"),
            error
    end;
add_(_)->
    message("::(modfile() | [modfile()])"),
    usage.

add_if_exists(F)->
    filelib:is_regular(F) andalso
        error =/= add_(F).

print_files(error) ->
    error;
print_files({ok, Infos}) ->
    GoodFiles = [FileName           || {FileName, [{present,true}]} <- Infos],
    ErrInfos  = [{FileName, Errors} || {FileName, [{error,Errors}]} <- Infos],
    message(?MISC:any_to_string({{ok, GoodFiles}, {error, ErrInfos}})).

%% @private
add_file(File) ->
%    Added = ui({add_dir,File}),
%    print_files(Added).
    case lists:suffix(".hrl", File) of
        true ->
            add_hrl(File);
        false ->
            add_erlbeam(File)
    end.

add_erlbeam(File) ->
    ui({add_dir,File}).

add_hrl_h() ->
    message("add_hrl(HeaderFileName)").

add_hrl(File=[C|_]) when is_integer(C) ->
    ui({add,File});
add_hrl(_) ->
    message("::(string())"),
    usage.

%ri:add([a,b]).
%ri:add("a.erl").

drop_h() ->
    message("drop(ModFileDirList)").

%% @doc Drop either a module, a file, a directory or a list of these
drop(L=[H|_]) when not is_integer(H) ->
    with_db_save(lists:foreach(fun drop/1,L));
drop(Mod) when is_atom(Mod) ->
    with_db_save(catch_referr(fun()->
        drop(mod2file(Mod,true))
    end));
drop(File) when is_list(File) ->
    with_db_save(drop_file(File));
drop(_)->
    message("::(modfile() | [modfile()])"),
    usage.

%% @private
drop_file(File) ->
    Dropped = ui({drop,File}),
    print_files(Dropped).
%ri:drop(a).

save_h() ->
    message2(["save(ModFileDirList)\n"]).

%% @doc Saves a file (debug)
%% @todo Is this still needed?
save(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun save/1,L);
save(Mod) when is_atom(Mod) ->
    catch_referr(fun()->
        save(mod2file(Mod,true))
    end);
save(File) when is_list(File) ->
%    recurse_erl(File, fun save_file/1).
%save_file(File) ->
    catch_referrsave(fun()->
        ?FileMan:save_file(get_filenode(File))
    end).


lsl_h() ->
    message2(["lsl()\n"]). %@todo

lsl() ->
    print_files(ui({status_info,[]})).

ls_h() ->
    message2(["ls()\n"
              "ls(ModFile)\n" ]).

%% @doc Lists modules contained in the database
ls() ->
    Files = ui({filelist}),
    print_files(Files).

%% @doc Lists includes, records, macros and functions in a file or moule
ls(ModFile) when
        (is_atom(ModFile) or is_list(ModFile))->
    catch_referr(fun() -> ls_(ModFile) end);
ls(_)->
    message("::(modfile())"),
    usage.

ls_(ModFile) ->
    FN  = get_filenode(ModFile),
    GetFunName = fun(F)-> ?MISC:fun_text([?Fun:name(F),?Fun:arity(F)]) end,
    GetRecName = fun(R)-> atom_to_list(?Rec:name(R)) end,
    Gets = [{"includes",?File:includes(),fun ?File:path/1},
            {"records", ?File:records(), GetRecName},
            {"macros",  ?File:macros(),  fun ?Macro:name/1},
            {"funcs",   ?Query:seq([[form],[fundef]]), GetFunName}],
    Text = [ begin Res = ?Query:exec(FN,Q) -- [FN],
                   case Res of
                       "" -> "";
                       _  -> S ++ ":" ++ [ " " ++ D(E) || E <- Res ] ++ "\n"
                   end
             end
           || {S,Q,D} <- Gets ],
    message2(Text).

%% @doc Creates a backup from the current state of the graph.
%% @see ri:backup_info/1
%% @see ri:ls_backups/0
backup() ->
    backup("").

%% @doc Creates a backup as {@link ri:backup/0},
%% but here the user can attach a commit log to the backup file.
%% @see ri:backup_info/1
%% @see ri:ls_backups/0
backup(CommitLog) when is_list(CommitLog) ->
    call_fun({backup, CommitLog});

backup(_) ->
    message("::(string())"),
    usage.

backup_h() ->
    message(["backup()\n",
             "backup(CommitLog)"]).

%% @equiv ri:backup()
checkpoint() ->
    backup().

%% @equiv ri:backup(CommitLog)
checkpoint(CommitLog) ->
    backup(CommitLog).

checkpoint_h() ->
   message(["checkpoint()\n",
            "checkpoint(CommitLog)"]).

%% @doc Returns a lists of backups, that has been created before with
%% {@link ri:backup/0} / {@link ri:checkpoint/0} or
%% {@link ri:backup/1} / {@link ri:checkpoint/1}.
%% @see ri:backup_info/1
ls_backups() ->
    Backups = call_fun({ls_backups}),
    case Backups of
        ok ->
            message3([]);
        _Else ->
            message3(Backups)
    end.

ls_backups_h() ->
    message("ls_backups()").

%% @equiv ri:ls_backups()
ls_checkpoints() ->
    ls_backups().

ls_checkpoints_h() ->
    message("ls_checkpoints()").

%% @spec backup_info(atom() | integer() | string()) -> any()
%% @doc Returns information about the given backup. That information is
%% the time of creation, and the commit-log (if specified earlier).
%% @see ri:ls_backups/0
backup_info(Backup) when is_atom(Backup) or is_list(Backup) or is_integer(Backup) ->
    case call_fun({backup_info, Backup}) of
        {ok, Infos} ->
            BackupName   = element(1, Infos),
            CreationTime = element(2, Infos),
            CommitLog    = element(3, Infos),

            io:format("Informations about ~w:~n", [BackupName]),
            case CommitLog of
                [] ->
                    ok;
                _Else ->
                    io:format("Commit-log: ~s~n", [CommitLog])
            end,
            io:format("Time of creation: ~s~n~n", [CreationTime]);
        Else ->
            Else
    end;

backup_info(_) ->
    message("::(atom() | string() | integer())"),
    usage.

backup_info_h() ->
    message(["backup_info(BackupName)\n",
             "backup_info(CheckpointNumber)"]).

%% equiv ri:backup_info(Backup)
checkpoint_info(Backup) ->
    backup_info(Backup).

checkpoint_info_h() ->
    message(["checkpoint_info(BackupName)\n",
             "checkpoint_info(CheckpointNumber)"]).

%% @spec restore(atom() | list() | integer()) -> any()
%% @doc Restores the given backup.
%% @see ri:backup/1
restore(Backup) when is_atom(Backup) or is_list(Backup) or is_integer(Backup) ->
    call_fun({restore, Backup});

restore(_) ->
    message("::(atom() | string() | integer())"),
    usage.

restore_h() ->
    message(["restore(BackupName)\n",
             "restore(CheckpointNumber)"]).

%% @doc undoes the previous transformation.
undo() ->
    call_fun({undo, []}).

undo_h() ->
    message("undo()").

%% @doc Removes all backups that belongs to the actual graph.
clean() ->
    call_fun(clean).

clean_h() ->
    message("clean()").

%% @spec create_graph(atom()) -> any()
%% @doc Creates a graph with the given name.
%% @see rename_graph/2
%% @see load_graph/1
%% @see delete_graph/1
create_graph(Name) when is_atom(Name) ->
    call_fun({create_graph, Name});

create_graph(_) ->
    message("::(atom())"),
    usage.

create_graph_h() ->
    message("create_graph(Name)").

%% @spec rename_graph(atom(), atom()) -> any()
%% @doc Renames a graph that has the given OldName, with the given NewName.
rename_graph(OldName, NewName) when is_atom(OldName) and is_atom(NewName) ->
    call_fun({rename_graph, OldName, NewName});

rename_graph(_, _) ->
    message("::(atom(), atom())"),
    usage.

rename_graph_h() ->
    message("rename_graph(OldName, NewName)").

%% @doc Returns a list of the created graphs.
%% @see ri:create_graph/1
%% @see ri:actual_graph/0
ls_graphs() ->
    Graphs = ui({ls_graphs}),
    message3(Graphs).

ls_graphs_h() ->
    message("ls_graphs()").

%% spec actual_graph() -> atom()
%% @doc Returns the actual graph's name.
%% @see ri:ls_graphs/0
actual_graph() ->
    ActGraph = ui({actual_graph}),
    message3(ActGraph).

actual_graph_h() ->
    message("actual_graph()").

%% @spec load_graph(atom()) -> any()
%% @doc Loads the given graph.
%% @see ri:create_graph/1
load_graph(Name) when is_atom(Name) ->
    call_fun({load_graph, Name});

load_graph(_) ->
    message("::(atom())"),
    usage.

load_graph_h() ->
    message("load_graph(Name)").

%% @spec delete_graph(atom()) -> any()
%% @doc Removes the given graph.
%% @see ri:delete_all_graphs/0
delete_graph(Name) when is_atom(Name) ->
    call_fun({delete_graph, Name});

delete_graph(_) ->
    message("::(atom())"),
    usage.

delete_graph_h() ->
    message("delete_graph(Name)").

%% @doc Removes all graphs.
%% @see ri:delete_graph/1
delete_all_graphs() ->
    call_fun({delete_all_graphs}).

delete_all_graphs_h() ->
    message("delete_all_graphs()").

call_fun(FunName) when is_atom(FunName) ->
    call_fun({FunName});

call_fun(FunAndParams) ->
    case ui(FunAndParams) of
        {ok, []} ->
            ok;
        Else ->
            Else
    end.


graph_h() ->
    message2([
        "graph()\n"
        "graph(TargetNameFile)\n"
        "graph(TargetFile,FilterList)\n" ]).

%% @doc Draws a graph with default name from database contents (debug)
graph() ->
    graph(?DEF_GRAPH_NAME).

%% @doc Draws a graph from database contents, saves result in `Target' (debug)
graph(Target) ->
    graph(Target,[]).

%% @doc Draws a graph from filtered database contents (debug)
graph(Target, Filter) when is_atom(Target) ->
    graph(atom_to_list(Target)++".dot",Filter);

graph(Target, Filter) ->
    Success = ui({draw, Target, Filter}),
    message(?MISC:any_to_string(Success)). % @todo

svg_h() ->
    message2([
        "svg()\n"
        "svg(TargetNameFile)\n"
        "svg(Target,Options)\n" ]).

%% @doc Draws a graph converted to SVG with default name (debug)
svg() ->
    svg(?DEF_GRAPH_NAME).

%% @doc Draws a graph converted to SVG from database contents (debug)
svg(Target) ->
    svg(Target, []).

%% @doc Draws a graph converted to SVG from filtered database contents (debug)
svg(Target, Opts) when is_atom(Target) ->
    svg(atom_to_list(Target)++".svg",Opts);
svg(Target, Opts) when is_list(Target) ->
    DotName = filename:rootname(Target) ++ ".dot",
    io:format("Making ~s...~n", [DotName]),
    _Success1 = ui({draw,DotName, Opts}), %%TODO: tooltip
    io:format("Calling Graphviz...~n"),
    Res = os:cmd("dot -Tsvg "++DotName++" -o"++Target),
    case Res of
        [] -> ok;
        _  -> {error,Res}
    end;
svg(_, _)->
    message("::(atom() | string(), proplist())"),
    usage.

gn_h() ->
    message("gn(TypeAtom,Index)").

%% @doc Prints out data of a graph node (debug)
gn(TypeAtom,Index) when is_atom(TypeAtom), is_integer(Index) ->
    message(io_lib:print(?Graph:data({'$gn',TypeAtom,Index})));
gn(_,_)->
    message("::(atom(), integer())"),
    usage.

stop_h()->
    message("stop()").

%% @doc Stops the RefactorErl server.
stop()->
    Success = ui({stop}),
    message(?MISC:any_to_string(Success)). % @todo

reset_h() ->
    message("reset()").

%% @doc Clears database contents and resets its schema
reset()  ->
	referl_htmlserver:reset(),
    metricmode(off),
    Success = ui({reset}),
    message(?MISC:any_to_string(Success)). % @todo


anal_message() ->
    refanal_message:analyse().
anal_dyn() ->
    refanal_dynfun:analyse().
clean_dyn() ->
    refanal_dynfun:clean().

%%% ============================================================================
%%% Regexp helpers

%% @doc gets a posrange in a file from a regexp
to_posrange(File,Range) ->
    rgxpos(File,Range).

%% @doc gets a position in a file from a regexp
to_pos(_,Pos) when is_integer(Pos) ->
    Pos;
to_pos(File,Pos) ->
    {P,_}=rgxpos(File,Pos),
    P.

%% @doc gets a position or a posrange in a file from a regexp
%% @private
%% @todo :wishlist improve rgxpos/3 to be context sensitive and indexless
%% @todo :wishlist check Rgx
rgxpos(File,Pos) ->
    case ?Query:exec(?File:find(File)) of
        [FileNode] -> Text = lists:flatten(?Syn:tree_text(FileNode)),
                      rgxpos_text(Text,Pos);
        _          -> throw(?RefError(file_not_present,[File]))
    end.

%% @doc gets a position or a posrange in text from a regexp
rgxpos_text(_,{Beg,End}) when is_integer(Beg), is_integer(End) ->
    {Beg,End};
rgxpos_text(Text,Rgx=[C|_]) when is_integer(C) ->
    rgxpos_text(Text,{Rgx,1});
rgxpos_text(Text,{Rgx=[C|_],Idx}) when is_integer(Idx), is_integer(C) ->
    case getparen(Rgx) of
        invalid ->
            throw(?LocalError(invalid_regexp,[Rgx]));
        {{P,Q},RRgx} ->
            Matches  =
                case tryrgx(matches,[Text,RRgx]) of
                    [] -> throw(?LocalError(unmatched_regexp,[Rgx]));
                    M  -> M
                end,
            {Beg,Len} =
              try
                lists:nth(Idx,Matches)
              catch
                error:function_clause ->
                    throw(?LocalError(unbound_idx_regexp,[Rgx,Idx]))
              end,
            Match = lists:sublist(Text,Beg,Len),
            Pre = [$^] ++ lists:sublist(RRgx,1,Q),
            Suf = lists:nthtail(P-1,RRgx) ++ [$$],
            Tails = length(tryrgx(sub,[Match,Pre,""])),
            Inits = length(tryrgx(sub,[Match,Suf,""])),
            {Beg+Inits, Beg+Len-1-Tails}
%%@todo !!! better separation!!!
    end;
rgxpos_text(_,Pos) -> throw(?LocalError(invalid_pos,[Pos])).

tryrgx(RegFun,Params=[_,RRgx|_]) ->
    case apply(regexp,RegFun,Params) of
        {match,M} -> M;
        {ok,R,1}  -> R;
        _         -> throw(?LocalError(invalid_regexp,[RRgx]))
    end.

%% @private
%% @doc Gets the location of escaped parenthesis in text
%% Starts up the state machine for getparen/4.
getparen(L=[_|_]) ->
    getparen([],1,[],L).

%% @doc Gets the location of escaped parenthesis in text
%%
%% Example invocations: <ol>
%%  <li>getparen("abc\\tdef")   = {{1,8},"abc\\tdef"}</li>
%%  <li>getparen("abc&lt;de&gt;f")    = {{4,5},"abcdef"}</li>
%%  <li>getparen("a&lt;b\\&lt;cd&lt;ef") = {{2,5},"ab&lt;cdef"}</li>
%%  <li>getparen("a&lt;b\ncd&lt;ef")  = {{2,5},"ab\ncdef"}</li>
%%  <li>getparen("a&lt;bcdef&lt;")    = {{2,6},"abcdef"}</li>
%%  <li>getparen("ab&lt;cdef")     = {{3,6},"abcdef"}</li>
%%  <li>getparen("abcde&lt;f")     = {{1,5},"abcdef"}</li>
%%  <li>getparen("ab&lt;c&lt;d&lt;")     = invalid</li>
%%  </ol>
%%
getparen([], I,R,[$<|T]) ->
    getparen([I],I,R,T);
getparen([],I,R,[$>|T]) ->
    getparen([1,I-1],I,R,T);
getparen([P],I,R,[$>|T]) ->
    getparen([P,I-1],I,R,T);
getparen(S,  I,R,[$\\,C |T]) when (C==$<) or (C==$>) ->
    getparen(S,I+1,R++[C],T);
getparen(S,  I,R,[$\\,C |T]) when is_integer(C) ->
    getparen(S,I+2,R++[$\\,C],T);
getparen(S,  I,R,[C|T])      when is_integer(C), C/=$\\, C/=$<, C/=$> ->
    getparen(S,I+1,R++[C],T);
getparen([], I,R,[])-> {{1,I-1},R};
getparen([P],I,R,[])-> {{P,I-1},R};
getparen([P,Q],_,R,[])-> {{P,Q},R};
getparen(_,_,_,_)-> invalid.

%%% ============================================================================
%%% Other helpers
%% @doc transformation helper for source+dest refactorings
transform2(Ref, Source, Dest, Args) ->
    catch_referr(fun()->
        DestTup = case Ref of
            move_fun ->
                {name,    file2modc(Dest)};
            A when (A==move_rec) or (A==move_mac) ->
                {filename,mod2filec(Dest)}
        end,
        transform_inject(Ref, Source, fun(A)->A end, [DestTup|Args])
     end).

%% @doc transformation helper for refactorings needing a position
transformp(Ref,Source,Pos,Args) ->
    catch_referr(fun()->
        Fun=fun(A)->
                {file,File} = proplists:lookup(file,A),
                [{position,to_pos(File,Pos)} | A] end,
        transform_inject(Ref,Source,Fun,Args)
     end).

%% @doc transformation helper for refactorings needing a posrange
transformr(Ref,Source,Range,Args) ->
    catch_referr(fun()->
        Fun=fun(A)->
                {file,File} = proplists:lookup(file,A),
                [{posrange,to_posrange(File,Range)} | A] end,
        transform_inject(Ref,Source,Fun,Args)
     end).

%% @private
%% @doc transformation helper that prints exceptions
transform_catch(Ref, Source, Args) ->
    catch_referr(fun()->
        transform_inject(Ref, Source, fun(X)->X end, Args)
    end).

%% @private
%% @doc transformation helper that adds both file and module information
transform_inject(Ref, Source, Fun, Arg) when is_function(Fun,1) ->
    Args = [{file,  mod2filec(Source)},
            {module,file2modc(Source)}|Arg],
    transform(Ref, Fun(Args)).

%% @private
catch_referrsave(Fun) when is_function(Fun,0) ->
     catch_referr(fun() ->
         catch_save(Fun)
     end).

%% @private
%% @doc prints out refactorerl exceptions as textual errors
catch_referr(Fun) when is_function(Fun,0) ->
    try
        Fun() %{ok,}
    catch
        E={M,_,_} when is_atom(M) -> %(M==?MODULE) or (M==?Error) ->
            message(?Error:error_text(E)),
            error; %{error,E}
        %Error -> ?d(Error)
        _ -> ok
    end.

%% @private
%% @doc prints out save_file exceptions as textual errors
catch_save(Fun) when is_function(Fun,0) ->
    R = try
            Fun()
        catch
            unsafe_save -> {error,"unsafe save denied"}
        end,
    case R of
        {error,Error} ->
            message(Error);
        ok -> ok
    end.

transform_h()->
    message("transform(Refac,Args)").

%% @doc transformation helper that catches messages synchronously
%% exported for debugging purposes
transform(Refac, Args) when is_atom(Refac), is_list(Args) ->
    transform_(Refac, Args, fun(_)->success end).

%% @doc transformation helper that catches messages synchronously
transform_(Refac, Args0, Fun) when is_function(Fun,1),
  is_atom(Refac), is_list(Args0) ->
    Args = [{ask_missing,true} | Args0],
    case ui({transform, Refac, Args}) of
        {ok, {result, R}} ->
            Fun(R);
        {ok, {abort, {_,A}}} ->
            message(A),
            deny;
        {ok, _} ->
            ok;
        {error, {_,E}} ->
            message("Fatal: " ++ E),
            error;
        _ ->
            ok
    end.

%% @doc handle synchronous UI/transform calls
trap(Pre, Action, Done)
  when is_function(Pre,0), is_function(Action,1),
       is_function(Done,1) ->

    State  = Pre(),
    Answer =
        try
            Action(State)
        catch
            E ->
                Done(E),
                throw(E);
              error:E ->
                Done({error,E}),
                erlang:error(E)
        end,
   Done(Answer).

%%% ----------------------------------------------------------------------------
%%% File and module related helpers

%% @private
%% @doc looks up the module name of a loaded and error-free file
file2modc(Source) ->
    _ = guard_ready(Source),
    file2mod(Source).

%% @private
%% @doc looks up the module name of a loaded file
file2mod([]) ->
    throw(?LocalError(no_file,[]));
file2mod(Mod) when is_atom(Mod) ->
    Mod;
file2mod(Fil) when is_list(Fil) ->
    File = filename:absname(Fil),
    FileNode = ?Query:exec1(?File:find(File),
                            ?RefError(file_not_present, [File])),
    ModNode  = ?Query:exec1([FileNode], ?File:module(),
                            ?RefError(file_not_module,  [File])),
    ?Mod:name(ModNode).

%% @private
%% @doc looks up the filename for a loaded and error-free module
mod2filec(Source) ->
    guard_ready(mod2file(Source)).

%% @private
%% @doc looks up the filename for a loaded module
mod2file([],_Amb) ->
    throw(?LocalError(no_file,[]));
mod2file(File,_Amb) when is_list(File) ->
    filename:absname(File);
mod2file(Mod,Amb) when is_atom(Mod), is_boolean(Amb) ->
    FileNodes =
        ?Query:exec(?Query:seq([
            ?Mod:find(Mod),
            ?Mod:file() ])),
    case FileNodes of
        [File] ->
            ?File:path(File);
        [] ->
            throw(?RefError(mod_not_found, [Mod]));
        [_|_] ->
            case Amb of
                false ->
                    throw(?RefError(ambiguous_mod, [Mod]));
                true ->
                    [?File:path(File) || File <- FileNodes]
            end;
        X      -> throw(?LocalError(internal_unexpected, [X]))
    end.

%% @private
%% @doc returns only a single filename, otherwise throws ambiguous_mod
mod2file(X) -> mod2file(X,false).

%% @private
%% @doc ensures that the given file is error-free
guard_ready([])   ->
    throw(?LocalError(no_file,[]));
guard_ready(File) ->
    case state(File) of
        ok      -> File;
        invalid -> throw(?LocalError(not_ready,[File]));
        _       -> throw(?RefError(file_not_present, [File]))
    end.

%% @private
%% @doc gets the status of a file or module
state(Source) ->
    trap(
      fun() ->
              mod2file(Source)
      end,
      fun(F) ->
              {ok,[{F,Prop}]} = ui({status,F}),
              case ?MISC:pget([present,error],Prop) of
                  [[true],[]]    ->
                      ok;
                  [[false],[]] ->
                      missing;
                  [[true],[true]] ->
                      invalid
              end
      end,
      fun(X) -> X end).

%%% ----------------------------------------------------------------------------
%%% Elementary helpers

build_h()->
    message("build()").

%% @doc Builds RefactorErl.
build() ->
    os:putenv("COMPILE_CPP", "true"),
    build_source().

%% @doc Builds RefactorErl without compiling the CPP code.
build(no_cpp) ->
    os:putenv("COMPILE_CPP", "false"),
    build_source().
    
build_source() ->
    referl_gen_build:run(tool_dir(), tool, [debug_info]).

%% @private
%% @doc Returns the `tool' directory of RefactorErl.
tool_dir() ->
    filename:join([code:lib_dir(referl_core), "..", ".."]).

%% @spec errors() -> ok | 'errors!'
%% @doc Prints a list of functions that have cross reference problems:
%%      unused, deprecated, or most importantly, undefined functions.
%%      Ideally, this function should print nothing, and return `ok'.
errors() ->
    errors(all).

%% @spec errors(all | loaded) -> ok | 'errors!'
%% @doc  A helper for `errors/0',
%%       this function can print the errors of all known modules (`all'),
%%       or only those that are loaded (`loaded').
errors(Opt) ->
    {ok, [{script, _, Actions}]} =
        file:consult(filename:join(tool_dir(), "refactorerl.script")),
    Mods = [Mod ||  {apply, {application, load,
                             [{application, _App, Opts}]}} <- Actions,
                    {modules, Mods} <- Opts,
                    Mod <- Mods],
%    io:format("Checking modules:~n    ~p~n", [Mods]),
    Problems = [{St, Funs} ||   Mod <- Mods,
                                lists:prefix("ref", atom_to_list(Mod)),
                                {St, Funs} <- xref:m(Mod),
                                Funs =/= []],
    P1 = print_problems(unused, "Unused functions:", Problems, Mods, Opt),
    P2 = print_problems(deprecated, "Deprecated functions:", Problems, Mods, Opt),
    P3 = print_problems(undefined, "Missing functions:", Problems, Mods, Opt),
    case {P1, P2, P3} of
        {ok, ok, ok} -> ok;
        _            -> 'errors!'
    end.

print_problems(ProblemAtom, Descr, Problems, Mods, Opt) ->
    case lists:usort(lists:append([Funs ||  {A, Funs} <- Problems,
                                            A =:= ProblemAtom])) of
        [] ->
            ok;
        ProblemFuns ->
            io:format("~s~n", [Descr]),
            [io:format("  ~p:~p/~p~n", [M,F,A]) || {M,F,A} <- ProblemFuns],
            PrCalled = [{M,F,A} || {_, {M,F,A}} <- ProblemFuns],
            [begin
                io:format("  ~p:~p/~p~n", [PM,PF,PA]),
                [io:format("      <- ~p:~p/~p~n", [M,F,A]) ||
                    {{M,F,A},{M2,F2,A2}} <- ProblemFuns,
                    {M2,F2,A2} =:= {PM,PF,PA},
                    Opt =:= all orelse lists:member(M, Mods)]
             end || {PM,PF,PA} <- lists:usort(PrCalled),
                    Opt =:= all orelse lists:member(PM, Mods)],
            errors
    end.


%% @doc Runs the unit tests.
test() ->
    %ri:metricmode(off),
    reftest_refact:run().

%% @doc Runs all test cases.
%% @todo Run unit tests and other regression indicators
testall() ->
    %ri:metricmode(off),
    {reftest_lib:run(),
     reftest_refact:run()}.

%% @doc Runs the test cases of a transformation.
%% The name of the transformation can be abbreviated up to ambiguity.
%% @todo Run on a given entity
test(Params) when is_list(Params) ->
    reftest_refact:run(Params);
test(Mod) when is_atom(Mod) ->
    {ok, TestDirFiles} = file:list_dir(filename:join(["..", "test"])),
    ModList = atom_to_list(Mod),
    case [F || F <- lists:usort(TestDirFiles), lists:prefix(ModList, F)] of
        [FileName|_] -> reftest_refact:run([{test, [list_to_atom(FileName)]}]);
        []           -> list_files(TestDirFiles)%;
%        FileNames    -> list_files(FileNames)
    end;
test(_)->
    message("::(atom() | proplist())"),
    usage.

%% @doc Runs a specific test case of a transformation.
%% If the name of the test case is numeric, it can be given as an integer.
%% The name of the transformation can be abbreviated up to ambiguity.
test(Mod, Case) when is_atom(Mod), is_integer(Case) ->
    CaseList = integer_to_list(Case),
    CaseAtom =
        if
            Case < 10 -> list_to_atom([$0|CaseList]);
            true      -> list_to_atom(CaseList)
        end,
    test(Mod, CaseAtom);
test(Mod, Case) when is_atom(Mod), is_atom(Case) ->
    {ok, TestDirFiles} = file:list_dir(filename:join(["..", "test"])),
    ModList = atom_to_list(Mod),
    CaseList = atom_to_list(Case),
    case [F || F <- lists:usort(TestDirFiles), lists:prefix(ModList, F)] of
        [ModName|_] ->
            {ok, TestDir2} = file:list_dir(filename:join(["..", "test", ModName])),
            case lists:member(CaseList, TestDir2) of
                false ->
                    list_files(TestDir2);
                true ->
                    reftest_refact:run([{test, [{list_to_atom(ModName), Case}]}])
            end;
        []         -> list_files(TestDirFiles)%;
        %%FileNames  -> list_files(FileNames)
    end;
test(_,_)->
    message("::(atom(), atom() | integer())"),
    usage.


%% @doc Lists the files of a directory.
list_files(AllFiles) ->
    io:format("Possible parameters:~n"),
    Files = lists:usort(AllFiles) -- [".svn", "DESC"],
    [io:format("    ~s~n", [File]) || File <- Files],
    missing_dir.


%% @doc Returns the list of differences after files have been loaded.
%%      This function does NOT consider forms with load errors.
%% @todo Parallelising this operation does not seem to bring any speedup.
cat_errors() ->
    % todo [file] should have an interface function
    Files = [File || File <- ?Query:exec([file]),
                     not has_errors(File),
                     File =/= error],
    io:format("~B files ", [length(Files)]),
    {TimeDiff, Results} =
        timer:tc(fun() -> pmap(fun cat_errors_with_display/1, Files) end, []),
    {H,M,S} = calendar:seconds_to_time(TimeDiff div 1000000),
    io:format("~nchecked in ~B hours, ~B minutes, ~B seconds~n", [H,M,S]),
    case [{File, Err} || {File, Err} <- Results, Err =/= ok] of
        []     -> no_cat_errors;
        Errors ->
            {ErrFiles, _} = lists:unzip(Errors),
            ErrFileNames = [(?Graph:data(File))#file.path || File <- ErrFiles, File =/= error],
            io:format("Errors found in the following files:~n    ~p~n", [ErrFileNames]),
            Errors
    end.

has_errors(File) ->
    Types = [(?ESG:data(Form))#form.type || Form <- ?Query:exec(File, ?File:real_forms())],
    lists:member(error, Types).


%% @doc Returns the list of differences between the original and the database
%% version of a file.
cat_errors(File) ->
    ?FileMan:create_scanner(),
    #file{path=Path, eol=Eol} = ?ESG:data(File),
    {FileText, _EOL} = ?FileMan:file_text(Path),
    FileHashWForms = [{Hash, tokens_to_text(Tokens, Eol)}
                        || {Hash, Tokens} <- ?FileMan:tokenize(FileText)],
    GForms = ?Query:exec(File, ?File:real_forms()),

    GFileText = ?Syn:flat_text(File),
    case {GFileText == FileText, nows(GFileText) == nows(FileText)} of
        {true, true} ->
            ok;
        {false, true} ->
            ws_diff;
        {false, false} ->
            case length(GForms) == length(FileHashWForms) of
                false -> {not_matching_form_count, length(GForms), length(FileHashWForms)};
                true ->
                    MatchingForms =
                        [{{GForm, (?ESG:data(GForm))#form.hash}, {{FHash, FText}, Idx}}
                         || {GForm, {{FHash, FText}, Idx}} <-
                          lists:zip(GForms, ?MISC:index_list(FileHashWForms))],
                    case [{Idx, GForm, FText}
                            ||  {{GForm, Hash}, {{FHash, FText}, Idx}} <- MatchingForms,
                                Hash =/= FHash] of
                        BadForms = [_|_] ->
                            {changed_forms, BadForms};
                        [] ->
                            MForms =
                                [{Idx, GForm, flat_text(GForm), FText}
                                    || {{GForm, _Hash}, {{_FHash, FText}, Idx}} <- MatchingForms],
                            AllDiffForms =
                                [{Idx, GForm} || {Idx, GForm, GFText, FText} <- MForms,
                                                 not is_similar(GFText, FText)],
                            WSDiffForms =
                                [{Idx, GForm} || {Idx, GForm, GFText, FText} <- MForms,
                                                 not is_similar(nows(GFText), nows(FText))],
                            DiffForms = AllDiffForms -- WSDiffForms,

                            case {DiffForms, WSDiffForms} of
                                {[], []} ->
                                    ok;
                                _ ->
                                    {{diffs, DiffForms}, {ws_diffs, WSDiffForms}}
                            end
                    end
            end
    end.

nows(Text) ->
    re:replace(Text, "[ \n\t\r]+", " ", [global, {return, list}]).

is_similar(invalid_children, _) ->
    false;
is_similar(_, invalid_children) ->
    false;
is_similar(Txt1, Txt2) ->
    Txt1 == Txt2.


tokens_to_text(Tokens, Eol) ->
    ?FileMan:orig_text(Eol, lists:flatten([Pre ++ Text ++ Post ||
     #token{prews=Pre, text=Text, postws=Post} <- Tokens])).

%% @todo Move to ?Syn.
flat_text(Form) ->
    try
        lists:flatten(?Syn:tree_text(Form))
    catch
        throw:{invalid_children, _, _} -> invalid_children
    end.




cat_errors_with_display(File) ->
    Ret = cat_errors(File),
    ErrOrSuccStr =
        case Ret of
            ok -> "o";
            _  -> "X"
        end,
    io:put_chars(ErrOrSuccStr),
    {File, Ret}.


pmap(Fun, Xs) ->
    S = self(),
    [spawn(fun() -> pmap_f(S, Fun, X) end) || X <- Xs],
    pmap_gather(length(Xs)).

pmap_gather(0) ->
    [];
pmap_gather(N) ->
    receive
        {pmap, _, Ret} ->
            [Ret|pmap_gather(N-1)]
    end.

pmap_f(Parent, Fun, X) ->
    Parent ! {pmap, self(), (catch Fun(X))}.

%% -----------------------------------------------------------------------------

modsave(Mod) ->
    refcore_store_graph:save(Mod).

modload(Mod) ->
    refcore_store_graph:load(Mod).

%% -----------------------------------------------------------------------------

%% @doc Start yaws with default configuration.
start_yaws() ->
    catch_referr(fun() -> web_helper:start_yaws() end).

%% @doc Configure and start yaws web server.
start_yaws(Proplist) ->
    catch_referr(fun() -> web_helper:start_yaws(Proplist) end).

%% @doc Stop yaws web server.
stop_yaws() ->
    web_helper:stop_yaws().

%% -----------------------------------------------------------------------------

%% @doc Start nitrogen with default configuration.
start_nitrogen() ->
    catch_referr(fun() -> nitrogen_helper:start_nitrogen() end).

%% @doc Configure and start nitrogen.
start_nitrogen(Proplist) ->
    catch_referr(fun() -> nitrogen_helper:start_nitrogen(Proplist) end).

%% @doc Stop nitrogen.
stop_nitrogen() ->
    nitrogen_helper:stop_nitrogen().

%% -----------------------------------------------------------------------------

%% @spec draw_dep(Options::proplists()) -> any()
%% @doc Draws out the dependency graph (in either module or function
%% level) into a dot file. <br/>
%% See {@link print_dep/1} for Options.
draw_dep([]) -> message("Error: no option list given");
draw_dep(Options) ->
	try
	   case proplists:get_value(level, Options) of
		mod -> draw_dep_mod(proplists:delete(level, Options));
		func -> draw_dep_fun(proplists:delete(level, Options));
		_ -> message("Error: unknown dependency level given.")
	   end
	catch
		{error, Format, ArgList} ->
			io:format(Format, ArgList)
	end.


%% @spec print_dep(Options::proplists()) -> any()

%% @doc Prints out the dependency graph in either module or function level.
%%
%% Options:
%% ```
%% {level, Level}
%% Level = mod | func
%% '''
%%     Determines the examination level: module or function.
%%
%% ```
%% {type, Type}
%% Type = all | cycles
%% '''
%% Deteremines whether the printing or the drawing is on the entire graph, or just on the cycle subgraph
%% (if there is). Subgraph from a given function or module should be given with the <b>gnode</b> key.
%% ```
%% {gnode, Node | NodeList::lists()}
%% Node = node() | Name
%% Name = Module::atom() | Function::string()
%% '''
%% Subgraph from the given node on both levels.
%% A function can be specified (as "Module:Function/Arity" or
%% as a graph node) to only look at cycles touching that fun.
%% Multiple nodes/names can be given in a list.
%% Note, that if the node is not correctly given, in the type of the cycles ({type, cycles}),
%% the query will run on the whole graph.
%% ```
%% {dot, Dot::string()}
%% '''
%%    User defined .dot name. Can be given as a simple .dot file, then the result will be put into
%% the ./dep_files directory, or as an absolute path, when the user can define the exact place.
%% @end

print_dep([]) -> message("Error: no option list given");
print_dep(Options) ->
	try
	   case proplists:get_value(level, Options) of
		mod -> print_dep_mod(proplists:delete(level, Options));
		func -> print_dep_fun(proplists:delete(level, Options));
		_ -> message("Error: unknown dependency level given.")
	  end
	catch
		{error, Format, ArgList} ->
			io:format(Format, ArgList)
	end.

draw_dep_fun([]) -> message("Error: no type or gnode given");
draw_dep_fun(Options)	->
	Opts=proplists:delete(type, Options),
	case proplists:get_value(type, Options) of
		all -> refusr_cyclic_fun:draw(Opts);
		cycles -> refusr_cyclic_fun:draw_cycles(Opts);
		undefined -> case proplists:get_value(gnode, Options) of
				undefined -> 	message("Error: bad argument keys given");
				_ -> refusr_cyclic_fun:draw(Opts)
			     end;
		_ -> message("Error: bad type key given")
	end.

draw_dep_mod([]) -> message("Error: no type or gnode given");
draw_dep_mod(Options) ->
	Opts=proplists:delete(type, Options),
	case proplists:get_value(type, Options) of
		all -> refusr_cyclic_mod:draw(Opts);
		cycles -> refusr_cyclic_mod:draw_cycles(Opts);
		undefined -> case proplists:get_value(gnode, Options) of
				undefined -> 	message("Error: bad argument keys given");
				_ -> refusr_cyclic_mod:draw(Opts)
			     end;
		_ -> message("Error: bad type key given")
	end.

print_dep_fun([]) -> message("Error: no type or gnode given");
print_dep_fun(Options) ->
	case proplists:get_value(type, Options) of
		all -> refusr_cyclic_fun:check_cycle();
		cycles -> case proplists:get_value(gnode, Options) of
				undefined -> refusr_cyclic_fun:print_cycle();
				Node -> refusr_cyclic_fun:check_function(Node)
			     end;
		undefined -> case proplists:get_value(gnode, Options) of
				undefined -> 	message("Error: bad argument keys given");
				Node -> refusr_cyclic_fun:check_function(Node)
			     end;
		_ -> message("Error: bad type key given")
	end.

print_dep_mod([]) -> message("Error: no type or gnode given");
print_dep_mod(Options) ->
	case proplists:get_value(type, Options) of
		all -> refusr_cyclic_mod:check_cycle();
		cycles -> case proplists:get_value(gnode, Options) of
				undefined -> 	refusr_cyclic_mod:print_cycle();
				Node -> refusr_cyclic_mod:check_module(Node)
			  end;
		undefined -> case proplists:get_value(gnode, Options) of
				undefined -> 	message("Error: bad argument keys given");
				Node -> refusr_cyclic_mod:check_module(Node)
			     end;
		_ -> message("Error: bad type key given")
	end.

draw_dep_h()->
	message("draw_dep(Options::proplists())
		Dependency graph representation in module or function level. The result is a .dot file.


		The Options are the following
		- {level, Level}, Level = mod | func
		- {type, Type}, Type = all | cycles
		- {gnode, Node | NodeList::lists(Node)},
				 Node = node() | Name,
				 Name = Module::atom() | Function::string()
		- {dot, Dot::string()}

		Examples:
		- ri:draw_dep([{level, mod}, {gnode, erlang}]).
		- ri:draw_dep([{level, mod}, {gnode, erlang}, {dot, \"/home/working/dot/test.dot\" }]).
		- ri:draw_dep([{level, func}, {gnode, \"lists:hd/1\"}]).
		- ri:draw_dep([{level, mod},{gnode, {'$gn', module, 4}}]).
		- ri:draw_dep([{type, cycles}, {level, func}, {gnode, {'$gn', func, 36}}]).").

print_dep_h()->
	message("print_dep(Options::proplists())
		Prints out the dependency graph in either module or function level.


		The Options are the following
		- {level, Level}, Level = mod | func
		- {type, Type}, Type = all | cycles
		- {gnode, Node | NodeList::lists(Node)},
				 Node = node() | Name,
				 Name = Module::atom() | Function::string()

		Examples:
		- ri:print_dep([{level, func}, {type, all}]).
		- ri:print_dep([{level, mod}, {type, cycles}]).
		- ri:print_dep([{type, cycles}, {level, func}, {gnode, [{'$gn', func, 36}, {'$gn', func, 8}]}]).").

%%---------------------------------------------------------------------------
%% @spec dir_sort()-> [{Directory, [Modules]}] | {error, no_list_given} |
%%                    {error, no_modules_in_database}
%% Directory = string()
%% Modules = atom()
%% @doc  Directory sorting for all modules added to the database.
%% The modules are sorted in a list according to their directory in which they are in.
%% Those modules which don't have a directory are put into the "Other" category.
dir_sort()->
    refusr_dir_sort:sort().

%%---------------------------------------------------------------------------
%% @spec dir_sort(Options::proplists())-> [{string(), [Modules]}] | {error, no_list_given} |
%%                    {error, no_modules_in_database}
%% Modules = atom()
%% @doc Directory sorting for a given list of modules added to the database.
%% The modules are sorted in a list according to their directory in which they are in.
%% Those modules which don't have a directory are put into the "Other" category.
%% Modules can be given with their names or as nodes.
%% Can be done in a recursive way also (modules of subdirectories are also included).
%%
%% The Options are the following:
%% ```
%% {type, Type}
%% Type = mod | dir
%% '''
%% Filtering according to given modules (mod) or directories (dir).
%%
%% ```
%% {mod_list, ModList}
%% ModList = [node() | atom()]
%% '''
%% Directory filtering on these modules given as nodes or with their names.
%%
%% ```
%% {dir_list, DirList}
%% DirList = [string()]
%% '''
%% Module filtering on these directories (given with full path).
%%
%% ```
%% {recursive, Recursive}
%% Recursive = true | false
%% '''
%% Option working for only dir type filtering.
%% The sorting should be done in a recursive way, meaning that a directory (besides
%% its direct modules) will get the modules of its subdirectories in the result module list.

dir_sort(Options)->
	case proplists:get_value(type, Options) of
		undefined -> {error, no_type_key_given};
		mod -> refusr_dir_sort:get_dirs(proplists:get_value(mod_list, Options));
		dir -> refusr_dir_sort:get_mods(proplists:delete(type, Options));
		_ -> {error, wrong_type_key}
	end.

dir_sort_h()->
	message("
	dir_sort()
	Directory sorting for all modules added to the database.

	dir_sort(Options::proplists())
	Directory sorting for a given list of modules added to the database.

	- {type, Type}, Type = mod | dir
	- {mod_list, ModList}, ModList = [node() | atom()]
	- {dir_list, DirList}, DirList = [string()]
	- {recursive, Recursive}, Recursive = true | false)").


%%---------------------------------------------------------------------------
%% @spec fb_relations(Options::proplists())-> GetRel | IsRel | Cycle | Error | ok
%% GetRel = [{Path::string(), Path::string()}]
%% IsRel = true | false
%% Cycle = {ok, no_cyclic_dependency} | {NoCycles::integer(), ListCycles::list()}
%% Error = string() | tuple()
%%
%% @doc Interface for functionblock examiner module.<br/>
%% Options:
%% ```
%% {command, Command}
%% Command = get_rel | is_rel | check_cycle | draw | draw_cycle
%% '''
%%      - <b>get_rel</b> - Displays the relationship between the given functionblock list.
%% There is a relationship between fb1 block and fb2 block if a module of fb1 is dependent
%% from a module of fb2. The result is a tuplelist where a tuple represents a relation.<br/>
%%	- <b>is_rel</b> - Decides whether there is a connection between the two given functionblocks. <br/>
%%	- <b>check_cycle</b> - Checks for cycles in the dependencies between the given functionblock list.<br/>
%%	- <b>draw</b>- Prints out the entire graph or creates a subgraph drawing from the given functionblock list.
%%		 Output file is fb_relations.dot.<br/>
%% 	- <b>draw_cycle</b> - Prints out a subgraph which contains the cycle(s). Vertex list can be given.
%% 		Unless cycles exist, prints out the entire graph.
%% 		Output file is fb_rel_cycles.dot.<br/>
%%
%% ```
%% {fb_list, List}
%% List = [string()] | [{Basename::string(), [FunctionBlock::atom()]}]
%% '''
%% Chosen functionblock lists for further examinations. If no list given, then the it takes every functionblock list.
%% ```
%% {other, Other}
%% Other = bool()
%% '''
%% The Other parameter stands whether the category "Other" would be
%% taken into consideration (true) or not (false). The default value is true.
fb_relations([])->
	message("Error: no argument given");
fb_relations(Options)->
	try
		type_opt(proplists:get_value(command, Options),
			convert_list(proplists:delete(command, Options)))
	catch
		{error, Format, ArgList} ->
			io:format(Format, ArgList)
	end.

type_opt(get_rel, Options)->
	Other = case proplists:get_value(other, Options) of
		 false -> false;
		 _ -> true
		end,
	refusr_fb_relations:get_relations(convert_list(
					  proplists:delete(other, Options)), Other);

type_opt(is_rel, Options)->
	refusr_fb_relations:is_relation(Options);
type_opt(check_cycle, Options)->
	refusr_fb_relations:check_cycle(Options);
type_opt(draw, Options)->
	refusr_fb_relations:draw(Options);
type_opt(draw_cycle, Options)->
	%%io:format("~p~n", [Options]),
	refusr_fb_relations:draw_cycles(Options);
type_opt(undefined, Options)->
	type_opt(get_rel, Options);
type_opt(_, _)->
	message("Error: bad command argument given").

convert_list(undefined)-> [];
convert_list(List) when is_list(List)->List;
convert_list(_) -> message("Error: bad command argument given").


%% @spec fb_regexp(Options::proplists()) -> any()
%% @doc Interface for the function block regular expression filter.
%% The Options are the following:
%% ```
%% {type, Type}
%% Type = list | get_rel | cycle | draw
%% '''
%%      - <b>list</b> -Prints out every function block which matches the basic regular expression.<br/>
%%	- <b>get_rel</b> - Decides whether there is a connection between the two given functionblocks. <br/>
%%	- <b>cycle</b> - Checks for cycles in the dependencies between the given functionblock list.<br/>
%%	- <b>draw</b>- Prints out the entire graph or creates a subgraph drawing from the given functionblock list.
%%		 Output file is fb_relations.dot.<br/>
%%
%% ```
%% {regexp, Value}
%% Value = File::string() || [RegExp::string()]
%% '''
%% Unless this option (tuple) is given, the program works with a basic regular expression.
%% The basic rule: <i>&lt;functionblock&gt;/common/&lt;service&gt;/ebin</i>
%% or <i>&lt;functionblock&gt;/common/&lt;service&gt;/lib/ebin</i>.<br/>
%% and regular expression saved for these: ``^(/)[0-9a-zA-Z_./]+/common/[0-9a-zA-Z_.]+/(lib/)?(ebin)$''.
%%
%%      -<b>Value </b> - If the regular expression is given in a file then every single regexp has to be in a newline
%%	    in the file and must follow Perl syntax and semantics
%%          as the "<a href="http://www.erlang.org/doc/man/re.html"><tt>re</tt></a>" erlang module resembles so.<br/>
%%	    However, the user can give the regular expressions in a list as well.
%%          If there is an error with a regular expression in the file or in the list, it prints out the regexp,
%%          the error specification, and the position. <br/>
fb_regexp(Options)->
	try
	    refusr_fb_regexp:re(Options)
	catch
	   {error, Format, ArgList} ->
			io:format(Format, ArgList)
	end.

%% ------------------------------------------------------------------------------

%% @doc Inputs of the function are two lists. The elements of the first list
%% define architecture layers as group of modules. Every group of modules must
%% have a name. At default functions in a group can call functions from the
%% same group or from the group directly under own group. But the
%% second list defines additional permits, allowing function
%% calls between two architecture layers given by it's names. After this an
%% input can look like this: [{l1,[mod1,mod2]},{l2,[mod3,mod4,mod5]},{l3,[mod6]}],
%% [{l1,l2}]. The function checks wether in an architecture defined by the
%% input are functions, that insult the hierarchy or not. The output is
%% an empty list if function calls are allowed in the defined architecture
%% or list of disabled function calls in the following format:
%% {error|[{caller fun's mod name:caller fun's name/caller fun's arity,
%% called fun's module name:called fun's name/called fun's arity}, ..]}.
check_layered_arch(ModLists, ArchList) ->
    try
        {ModNameList,InsultCalls} = refusr_layer:show_insults(ModLists, ArchList),
        io:format("Modules of each interface:",[]),
        message(ModNameList),
        io:format("Insulting function calls: ~n",[]),
	    message(InsultCalls)
    catch
        {error,_} -> error;
        badarg ->
            io:format("Error: bad command argument given~n",[]),
            io:format("::([{atom(),[atom()|string()|node()]}],[{atom(),atom()}])~n",[]),
            usage
    end.

%% @doc Visualisation of layers, representing group of layers and insulting functions.
%% Check {@link check_layered_architecture/2} for detailed parameters. 
%% The result is a .dot file, called layered_arch.dot.
%% Same as calling ```show_layered_arch(LayerList, ArchList, "layered_arch.dot").'''
%% @end
show_layered_arch(ModLists, ArchList)->
    try
        refusr_layer:draw(ModLists,ArchList)
    catch
        {error,_} -> error;
        badarg ->
            io:format("Error: bad command argument given~n",[]),
            io:format("::([{atom(),[atom()|string()|node()]}],[{atom(),atom()}])~n",[]),
            usage
    end.
%% @doc Same as {@link show_layered_arch/2}, only the user can give the name of the
%%  generated .dot file.
%% @end
show_layered_arch(ModLists, ArchList, Filename)->
    try
        refusr_layer:draw(ModLists,ArchList, Filename)
    catch
        {error,_} -> error;
        badarg ->
            io:format("Error: bad command argument given~n",[]),
            io:format("::([{atom(),[atom()|string()|node()]}],[{atom(),atom()}],string())~n",[]),
            usage
    end.
    
%% ------------------------------------------------------------------------------

draw_gen_server(Module, ToFile) ->
    reflib_draw_gen_server:draw(Module, ToFile).

draw_gen_fsm(Module, ToFile) ->
    reflib_draw_gen_fsm:draw(Module, ToFile).

%% @doc Interface for the duplicate code analysis, that search for syntactically
%% identical or similar codes.<br/>
%% The Options are the following:<br/>
%% <ul>
%%     <li>{files, Files} - Define the files in which the search is carried out.<br/>
%%         Files = [ Filepath::string() | File::string() | RegExp::string() | Module::atom() ]<br/>
%%         There are four options to specify files:
%%         <ul><li>path of the file</li>
%%             <li>regexp and file, that contains regexps</li>
%%             <li>name of the module</li></ul>
%%         This property is optional. By default all files from the database.
%%     </li>
%%     <li>{minlen, integer()} - Define the minimal length of a clone.<br/>
%%         This property is optional. Default value: 10.</li>
%%     <li>{minnum, integer()} - Define the minimal number of clones in a clone group.<br/>
%%         This property is optional. Default value: 2.</li>
%%     <li>{overlap, integer()} - Define the scale of the overlap enabled.<br/>
%%         This property is optional. Default value: 0.</li>
%%     <li>{output, Filename::string()} - Specifies thee nam of the file in witch the results are saved.<br/>
%%         This property is optional. Default value: -.</li>
%%     <li>{name, Name::atom()} - Specifies the name of the search. Use this name to save the result.<br/>
%%         This property is optional. Default value refers to the parameters of the search.</li>
%% </ul>
%% @spec search_duplicates() -> any()
search_duplicates() -> search_duplicates([]).
%% @spec search_duplicates(Options::proplist()) -> any()
search_duplicates(Options) when is_list(Options)->
    try
        {Name,EFTICs} = refusr_dupcode:search_initial_clones(Options),
        %_FEFTICs = lists:map(
        %    fun(E1)->
        %        lists:map(
        %            fun(E2)->
        %                proplists:delete(nodes, E2)
        %            end, E1)
        %    end, EFTICs)
        case EFTICs of
            [] -> io:format("No duplicates found.~n",[]);
            _ ->
                io:format("~p clone groups found.~n",[length(EFTICs)]),
                io:format("Result saved to ~p...~n",[Name]),
                io:format("run ri:show_dupcode(~p) to see the result.~n",[Name])
        end,
        ok
    catch
        {error, no_list} ->
            io:format("Error: bad command argument given~n",[]),
            io:format("::{files, [string()]}~n",[]),
            error;
        {error, ni_len} ->
            io:format("Error: bad command argument given~n",[]),
            io:format("::{minlen, integer()}~n",[]),
            error;
        {error, ni_num} ->
            io:format("Error: bad command argument given~n",[]),
            io:format("::{minnum, integer()}~n",[]),
            error;
        {error, ni_ol} ->
            io:format("Error: bad command argument given~n",[]),
            io:format("::{overlap, integer()}~n",[]),
            error;
        E -> io:format("Error: ~p~n",[E]), error
    end;
search_duplicates(_) ->
    io:format("Error: bad command argument given~n",[]),
    io:format("::[{files, [string()]},{minlen, integer()},{minnum, integer()},{overlap, integer()}]~n",[]),
    error.

show_dupcode_group(Name, GroupNumber) when is_atom(Name) andalso 
                                           is_integer(GroupNumber)->
    case refusr_dupcode:get_result_dets(Name) of
        []-> io:format("No result with the name ~p.~n",[Name]);
        [{Name,{_,Result}}] when length(Result) >= GroupNumber 
                                 andalso GroupNumber > 0 ->
            Group = lists:nth(GroupNumber,Result),
            NoNode = [proplists:delete(nodes,Clone)||Clone<-Group],
            io:format("~p~n",[NoNode]),
            io:format("Contains ~p members.~n",[length(Group)]);
        _ -> io:format("No such group in the result.~n",[])
    end,
    ok.
show_dupcode(Name) when is_atom(Name)->
    case refusr_dupcode:get_result_dets(Name) of
        []-> io:format("No result with the name ~p.~n",[Name]);
        [{Name,{_,Result}}]->
            NoNode = [[proplists:delete(nodes,Clone)||Clone<-Group]||Group<-Result],
            Temp = lists:zip(lists:seq(1,length(Result)), NoNode),
            io:format("~p~n",[Temp]),
            io:format("Contains ~p clone groups...~n",[length(Result)]),
            io:format("run ri:show_dupcode_group(~p, GroupNumber::integer()) to see one of the clone groups.~n",[Name])
    end,
    ok.
stored_dupcode_results()->
    Result = refusr_dupcode:stored_results(),
    case Result of
        [] -> io:format("No stored dupcode found.",[]);
        _ -> 
            FResult = [[Name, 
                        [case Property of 
                             {files, Files} -> {NoFiles,_} = lists:unzip(Files), 
                                               {files, NoFiles}; 
                             _ -> Property 
                         end
                         ||Property<-Properties]]
                       ||[Name, Properties]<-Result],
            io:format("~p~n",[FResult])
    end,
    ok.
save_dupcode_result(Name, Filename) when is_atom(Name)->
    case refusr_dupcode:get_result_dets(Name) of
        []-> io:format("No result with the name ~p.~n",[Name]);
        [{Name,{_,Result}}]->
            Numbered = lists:zip(lists:seq(1,length(Result)), Result),
            file:write_file(Filename, io_lib:format("Name: ~p~n~p",[Name,Numbered]))
    end,
    ok.
