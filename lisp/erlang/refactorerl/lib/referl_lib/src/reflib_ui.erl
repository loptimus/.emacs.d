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

%%% @doc This module provides a bridge between user interfaces and system
%%% functionality. A user interface requests operations and receives results
%%% through this module. Requests are asychronously handled, the results are
%%% returned through a `gen_event' event manager. Events can occur
%%% independently of operations, a user interface probably wants to handle
%%% these as well.
%%%
%%% Operations requests are sent using the exported functions of this module.
%%% See the description of the functions below.
%%%
%%% == Message types ==
%%%
%%% To receive event messages, a standard `gen_event' callback module must be
%%% registered by calling {@link add_msg_handler/2}. The following event
%%% messages may be sent to the callback modules:
%%%
%%% <dl>
%%%
%%% <dt>{@type {status, Message::string()@}}</dt> <dd>A textual message that
%%% informs the user of the tool's status.</dd>
%%%
%%% <dt>{@type {error, Message::string()@}}</dt> <dd>A textual error message
%%% that describes an internal error situation of the tool.</dd>
%%%
%%% <dt>{@type {add, Files::[Path::path()]@}}</dt> <dd>The files specified by
%%% the `Files' list are added (or maybe re-added) to the database.</dd>
%%%
%%% <dt>{@type {drop, Files::[Path::path()]@}}</dt> <dd>The files specified by
%%% the `Files' list are removed from the database.</dd>
%%%
%%% <dt>{@type {invalid, Path::path()@}}</dt> <dd>The file specified by `Path'
%%% is added to the database, but it is invalid (contains errors).</dd>
%%%
%%% <dt>{@type {reload, Files::[Path::path()]@}}</dt> <dd>The files specified by
%%% the `Files' list are modified by the tool (and should be reloaded from disk
%%% by the UI).</dd>
%%%
%%% <dt>{@type {rename, {From::path(), To::path()@}@}}</dt> <dd>The file
%%% `From' is changed to have the name `To' (and this changes should be
%%% followed by the UI).</dd>
%%%
%%% <dt>{@type {showconfig, Config::[{Key::atom(), Value@}]@}}</dt> <dd>This
%%% message describes the currect runtime configuration of the tool. Currently
%%% used key values are `appbase', `include', and `output'. See also {@link
%%% saveconfig/3}.</dd>
%%%
%%% <dt>{@type {filelist, Files::[{Path::path(), Error::atom()@}]@}}</dt>
%%% <dd>This message contains the files that are currently stored in the
%%% database, and whether they haver errors or not.</dd>
%%%
%%% <dt>{@type {filepos,
%%% FilePos::[{Path::path(),SatartPos::{integer(), integer()@},
%%% EndPos::{integer(), integer()@}@}]@}}</dt> <dd>This message
%%% contains a list of starting and ending position and their containing
%%% file</dd>
%%%
%%% <dt>{@type {funlist, Functions::[{Name::atom(),
%%% Arity::integer()@}]@}}</dt> <dd>This message contains a list of functions.
%%% <small>TODO: This is rather specific to the move function refactoring, and
%%% should be improved.</small></dd>
%%%
%%% <dt>{@type {recordlist, Records::[Name::atom()]@}}</dt> <dd>This message
%%% contains a list of records. <small>TODO: this is rather specific to the
%%% move record refactoring, and should be improved.</small></dd>
%%%
%%% <dt>{@type {macrolist, Macros::[Name::atom()|string()]@}}</dt> <dd>This
%%% message contains a list of macros. <small>TODO: this is rather specific
%%% to the move macro refactoring, and should be improved.</small></dd>
%%%
%%% <dt>{@type {question, {Id::integer(), Details::proplist()@}@}}</dt>
%%% <dd>Contains a question that must be answered by {@link reply/2} or
%%% cancelled by {@link cancel/1}. <small>TODO: `Details' are to be
%%% specified.</small></dd>
%%%
%%% <dt>{@type {uifinished, Operation::atom()@}}</dt> <dd>The spawning of the
%%% specified operation is complete on the UI side.</dd>
%%%
%%% <dt>{@type {trfinished, ok@}}</dt> <dd>An operation is finished
%%% on the transform side.</dd>
%%%
%%% <dt>{@type {progress, {add|drop, Path::path(), Count::integer(),
%%% Max::integer()@}@}}</dt> <dd>Progress report of a file (re)loading or
%%% dropping operation. `Max' is the maximal number of steps, `Count' is the
%%% number of currently finished steps.</dd>
%%%
%%% </dl>
%%%
%%% ------------
%%% 2010.01.18 bkil
%%% status: I am constantly verifying if it compiles and passes
%%%  dialyzer (all via flymake), but never run it
%%% @todo actualize comments
%%% @todo finish filelist
%%% @todo transform
%%%
%%% A quick overview of the new message format.
%%% Unicast:
%%%  {ReqID, progress, todo()} |
%%%  {ReqID, reply, {error,Reason} | {ok,any()}}
%%%  {ReqID, question, {NewID,todo()}}
%%%  {ReqID, answer, todo()}
%%% Broadcast:
%%%  {B,statusinfo,StatusData}
%%%   where StatusData=
%%%    {shutdown,Reason} |
%%%    {reset,Reason} |
%%%    {change,FileChange}
%%%     where FileChange = [{Filename,[{rename,New}    |
%%%                                    {content,true}  |
%%%                                    {present,true|false}|
%%%                                    {error,Errors}  |
%%%                                    {lastmod,todo()}|
%%%                                    {type,todo()} ]}]
%%%      where Errors = [todo()]
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reflib_ui).
-vsn("$Rev: 1479$ ").

%%% ----------------------------------------------------------------------------
%%% Client exports

%%% Database control
-export([stop/1, reset/1, clean/1, add/2, drop/2, backup/2, ls_backups/1,
         backup_info/2, saveconfig/4]).
-export([create_graph/2, rename_graph/3, ls_graphs/1, actual_graph/1, restore/2,
         undo/2, load_graph/2, delete_graph/2, delete_all_graphs/1]).
-export([load_beam/4, add_dir/2, generate_dir/2, drop_dir/2]).

%%% Status queries
-export([status/2, showconfig/1, filelist/1, status_info/2]).
-export([get_envs/1]).

%%% Semantic queries
-export([draw/3, get_running_queries/1, kill_query/2]).

%%% Internal queries
-export([funlist/2, recordlist/2, macrolist/2]).

%%% Refactoring
-export([transform/3, reply/3, cancel/2]).

%%% Clustering
-export([cl_options/2, run_cl/4, cl_refresh/1]).

%%% Metrics
-export([metric_mode/2]).

%% Callbacks
-export([error_text/2]).

-include("lib.hrl").
-include_lib("kernel/include/file.hrl").


%%% ----------------------------------------------------------------------------
%%% UI protocol description

% each function invocation must return with exactly one of the following:
-define(NoReply,   noreply).
-define(Dat(X),    {ok,X}).
-define(OK,        ?Dat([])).
-define(ERR(R),    {error,R}).
-define(LErr(R),   ?LocalErr(R,[])).
-define(LocalErr(R,L), ?ERR(?LocalError(R,L))).
-define(RefErr(R,L),   ?ERR(?RefError(R,L))).

% the following can be sent anytime:
send_progress(MCB, Op, File, Percent, FormCount, FormMax, KBps) ->
    (MCB#msg_cb.unicast)(progress,{Op, File, Percent, FormCount, FormMax, KBps}).

send_change(MCB,Change) ->
    (MCB#msg_cb.broadcast)(statusinfo,[{change,Change}]).

send_shutdown(MCB) ->
    (MCB#msg_cb.broadcast)(statusinfo,[{shutdown,"manual shutdown initiated"}]).

send_reset(MCB) ->
    (MCB#msg_cb.broadcast)(statusinfo,[{reset,"manual restart initiated"}]).

% possible formats of changes
-define(PresentCorrect(File),
        {File, [{present,true}]}).
-define(PresentCorrectTM(File,Type,LastMod),
        {File, [{present,true},
                {type, Type}, {lastmod, LastMod}]}).
-define(PresentError(File,Errors),
        {File, [{error,Errors}]}). %,{present,true}
-define(PresentErrorTM(File,Type,LastMod,Errors),
        {File, [{present,true}, {error,Errors},
                {type, Type}, {lastmod, LastMod}]}).
-define(NotPresent(File),
        {File, [{present,false}]}).
-define(Modified(File),
        {File, [{content,true}]}).
-define(Renamed(OldPath,NewPath),
        {OldPath, [{rename,NewPath}]}).
-define(AddedCorrect(File),     ?PresentCorrect(File)).
-define(AddedError(File,Error), ?PresentError(File,Error)).
-define(Dropped(File),          ?NotPresent(File)).


%%% ----------------------------------------------------------------------------
%%% types

%%% @type path() = string(). The absolute path of a file.


%%% ============================================================================
%%% Error texts

error_text(wrangler_dir, []) ->
    ["Please set Wrangler's ",
     "installation directory, and restart RefactorErl"];
error_text(cl_ui_refresh, []) ->
    ["cl_ui refresh: something wrong"];
error_text(invalid_checkpoint_number, []) ->
    error_text(no_backup, []);
error_text(no_backup, []) ->
    ["Need a backup to do it"];
error_text(nothing_changed, []) ->
    ["Nothing has changed since the last backup"];
error_text(invalid_path_number, []) ->
    ["An invalid path number has been specified"];
error_text(undef, []) ->
    ["undefined function"];
error_text(noresult, []) ->
    ["no result available"];
error_text(trfail, []) ->
    ["initiation of the transformation failed"];
error_text(no_dups, []) ->
    ["Wrangler did not find any ",
     "duplicated code fragments"];
error_text(not_found, [Path])->
    ["No module or BEAM could be handled from \"", Path, "\""];
error_text(somebad, [])->
    ["Processing failed for some files, check the errors"];
error_text(graph_already_exist, [Name])->
    ["A graph already exists with the given name: \'", ?MISC:to_list(Name), "\'"];
error_text(graph_not_exist, [OldName, _NewName])->
    error_text(graph_not_exist, [OldName]);
error_text(graph_already_exist, [_OldName, NewName])->
    error_text(graph_already_exist, [NewName]);
error_text(graph_not_exist, [Name])->
    ["The graph you have specified does not exist: \'", ?MISC:to_list(Name), "\'"];
error_text(invalid_name, [_OldName, NewName])->
    error_text(invalid_name, [NewName]);
error_text(invalid_name, [Name])->
    ["The name you have specified is invalid: \'", ?MISC:to_list(Name), "\'"];
error_text(graph_is_in_use, [Name])->
    ["The graph you have specified is in use,",
     " and cannot be deleted now: \'", ?MISC:to_list(Name), "\'"];
error_text(graph_load_fail, [Name]) ->
    ["The graph \'" ++?MISC:to_list(Name) ++ "\' has been loaded, but with an empty database, because it may be crashed!"];
error_text(invalid_backup, [Backup]) ->
    ["The backup or the checkpoint number you have specified is invalid: \'",
      ?MISC:to_list(Backup), "\'"];
error_text(corrupted_backup, [Backup])->
    ["The given backup is corrupted and has been renamed (has a 'corrupted_' prefix): \'",
      ?MISC:to_list(Backup), "\'\n",
      "The database remains the same."].

%@todo refactor to ?LocalError
%error_message({no_include_file, File}) ->
%    ?MISC:format("Include file \"~s\" not found", [File]);
%error_message({some_proc, []})->
%    ?MISC:format("Not all files were processed", []);
%error_message({none_proc, []})->
%    ?MISC:format("No files were processed", []);
%error_message(Error) ->
%    ?MISC:format("Error: ~p", [Error]).



%%% ============================================================================
%%% Standard UI exports


%% @spec stop(#msg_cb{}) -> ok
%% @doc Stops the RefactorErl server.
stop(MCB) ->
    send_shutdown(MCB),
    init:stop(),
    ?OK.
% "RefactorErl server is shutting down..."

%% @spec status(#msg_cb{}, path()) -> ok
%% @doc Requests information about the status of `File'. The result is a
%% message of type `add', `invalid', or `drop'.
status(_MCB, FileName) when is_list(FileName) ->
    case ?Query:exec(?File:find(FileName)) of
        [File] ->
            case ?Query:exec(File, ?File:error_forms()) of
                [] -> ?Dat([?PresentCorrect(FileName)]);
                E  -> ?Dat([?PresentError(FileName,
                                          decode_error_form(File,E))])
            end;
        []  -> ?Dat([?NotPresent(FileName)])
    end.


%% @spec add(#msg_cb{}, path()) -> ok
%% @doc Adds `File' to the database.
%% @todo maybe introduce a `reload' or `update' message
add(MCB, FileName) when is_list(FileName) ->
    do_add_file(MCB,FileName).

%% @spec drop(#msg_cb{}, path()) -> ok
%% @doc Drops `File' from the database.
drop(MCB, FileName) when is_list(FileName) ->
    do_drop_file(MCB,FileName).

%% @spec add_dir(#msg_cb{}, path()) -> ok
%% @doc Adds recursively to the database starting from `File'.
add_dir(MCB,FileName) when is_list(FileName) ->
    add_flat(recurse_erl(MCB, FileName, fun add_filedir/2)).

%% @spec generate_dir(#msg_cb{}, path()) -> ok
%% @doc Generates html recursively starting from `File'.
generate_dir(MCB,FileName) when is_list(FileName) ->
    add_flat(recurse_erl(MCB,FileName, fun generate_filedir/2)).

%% @spec drop_dir(#msg_cb{}, path()) -> ok
%% @doc Drops recursively to the database starting from `File'.
drop_dir(MCB,FileName) when is_list(FileName) ->
    case filelib:is_regular(FileName) of
        true ->
            add_flat(recurse_erl(MCB,FileName, fun drop_filedir/2));
        false ->
            drop_filedir(MCB, FileName)
    end.

%% @spec load_beam(#msg_cb{}, path(), path(), boolean()) -> ok
%% @doc Loads a BEAM file compiled with `debug_info' and saves the result
load_beam(MCB,FileName,TargetDir,ToSave)
  when is_list(FileName), is_list(TargetDir), is_boolean(ToSave) ->
    case refcore_loadbeam:start(FileName,TargetDir,ToSave) of
        {error, Reason} ->
            send_change(MCB,[?AddedError(FileName,[])]), %@todo
            ?LErr(Reason);
        {ok, File} ->
            Add = [?File:path(FN) || FN <- ?Query:exec(File, ?File:includes())],
            send_change(MCB,[?AddedCorrect(F) || F <- Add]),
            ?OK
    end.

%% @spec draw(#msg_cb{}, path(),
%%            integer() | atom() | list(integer() | atom())) -> ok
%% @doc Creates a `.dot' drawing of the graph in the database, and saves it in
%% `File'. The contents of the graph are filtered according to `Filter'; the
%% value `1' means no filtering, then different numbers select different
%% filters. Also, filters might be given by atom keywords.
draw(_MCB, File, Type) ->
    Filter = convert_filter(Type),
    ok = ?DRAW_GRAPH:draw_graph(File, Filter),
    ?OK.

%% @spec showconfig(#msg_cb{}) -> ok
%% @doc Requests configuration information. The result is sent in a message of
%% type `showconfig'.
showconfig(_MCB) ->
    ?Dat([{Name, Value} ||
              Env <- ?Query:exec([env]),
              #env{name=Name, value=Value} <- [?Graph:data(Env)]]).

%% @spec saveconfig(#msg_cb{}, [path()], [path()], path() | original) -> ok
%% @doc Modifies the runtime configuration of the tool.
%% <ul>
%% <li>`App' is a list of directories that are searched for applications for
%%   `include_lib' directives</li>
%% <li>`Inc' is a list of directories that are searched for `include'
%%   directives</li>
%% <li>`Out' is the output directory of the tool. Its value can be `original',
%%   which means files should be overwritten, or a name of a directory, which
%%   means modified files should be saved there.</li>
%% </ul>
%% @todo If this option is still necessary, it should use ?Graph:save_envs/0.
saveconfig(_MCB, AppDirs, IncDirs, OutDir) ->
    ?Syn:del_envs(),
    [?Syn:create_env(appbase, Dir) || Dir <- AppDirs],
    [?Syn:create_env(include, Dir) || Dir <- IncDirs],
    ?Syn:create_env(output, OutDir),
    ?OK.
% "Configuration saved."

%% @spec filelist(#msg_cb{}) -> ok
%% @doc Requests a list of the files in the database. The result is sent in a
%% message of type `filelist'.
filelist(_MCB) ->
    Files = ?Query:exec([file]), %@todo
    FileStats =
        [ case ?Query:exec(FileNode, ?File:error_forms()) of
              [] -> ?PresentCorrect(FilePath);
              E  -> ?PresentError(FilePath,decode_error_form(FileNode,E))
          end || FileNode <- Files, FilePath <- [?File:path(FileNode)]],
    ?Dat(FileStats).

%% @spec reset(#msg_cb{}) -> ok
%% @doc Resets the database.
reset(MCB) ->
    send_reset(MCB),
    ?Graph:reset_schema(),
    ?Graph:save(database),
    ?OK.

clean(_MCB) ->
    ?Graph:clean(),
    ?OK.

%% @todo: doc
get_running_queries(_MCB)->
    ?Dat(?Transform:get_running_queries()).

%% @todo: doc
kill_query(_MCB, QueryId)->
    ?Dat(?Transform:kill_query(QueryId)).
%%% ============================================================================
%%% Transformation interface

-define(SQ,refusr_sq).
-define(MQ,refusr_metrics).
-define(CL,refcl_main).


%% @spec transform(#msg_cb{}, atom(), proplist()) -> ok
%% @doc Initiates a transformation and waits for it to end.
%% Returns the result of the said transformation.
%% `Args'  is a proplist that contains the arguments of the transformation.
%% If it is a real transformation (not a query/clustering),
%% saves the database upon its succesful execution.
%% @see reflib_transform
%% @see reflib_args
transform(MCB, Action, Args) ->
    Mod = action_module_name(Action),
    is_transformation(Mod) andalso
        ?Graph:save(before_transformation),
    transform2(MCB, Mod, Args).

transform2(MCB, Mod, Args) ->
    try
        TrRes = ?Transform:do(MCB, Mod, Args),
        is_transformation(Mod) andalso is_success(TrRes) andalso
            ?Graph:save(database),
        ?Dat(TrRes)
    catch
        throw:_ ->
            ?LErr(trfail);
        error:_ ->
            ?LErr(trfail)
    end.

%% Returns whether the module name designates a transformation
%% (as opposed to a query or clustering).
is_transformation(?MQ) -> false;
is_transformation(?SQ) -> false;
is_transformation(?CL) -> false;
is_transformation(_)   -> true.

%% Returns whether the transformation result indicates success.
is_success({abort, _}) -> false;
is_success({deny, _})  -> false;
is_success(_)          -> true.

%% Returns the module name for an action (refactoring, query or clustering).
action_module_name(metric_query)   -> ?MQ;
action_module_name(semantic_query) -> ?SQ;
action_module_name(clustering)     -> ?CL;
action_module_name(Refac)          -> list_to_atom("reftr_"++atom_to_list(Refac)).


%% @spec reply(#msg_cb{}, integer(), term()) -> ok
%% @doc Provides a reply to a previously asked question.
%% @see reflib_transform:reply/2
reply(_MCB, Id, Reply) ->
    noreply = ?Transform:reply(Id, Reply),
    ?NoReply.

%% @spec cancel(#msg_cb{}, integer()) -> ok
%% @doc Cancels a previously asked question.
%% @see reflib_transform:cancel/1
cancel(_MCB, Id) ->
    noreply = ?Transform:cancel(Id),
    ?NoReply.


%%% ----------------------------------------------------------------------------
%%% Movings

%% @spec funlist(#msg_cb{}, path()) -> ok
%% @doc Requests the list of functions defined in `File'. The result is
%% returned in a message of type `funlist'.
%% @todo error handling?!
funlist(_MCB, File) ->
    ?Dat([{?Fun:name(F), ?Fun:arity(F)} ||
             F <- ?Query:exec(
                     ?Query:seq([?File:find(File),
                                 ?File:module(),
                                 ?Mod:locals()]))]).

%% @spec recordlist(#msg_cb{}, path()) -> ok
%% @doc Requests the list of records defined in `File'. The result is
%% returned in a message of type `recordlist'.
recordlist(_MCB, File) ->
    ?Dat([?Rec:name(R) ||
             R <- ?Query:exec(
                     ?Query:seq(?File:find(File), ?File:records()))]).

%% @spec macrolist(#msg_cb{}, path()) -> ok
%% @doc Requests the list of macros defined in `File'. The result is
%% returned in a message of type `macrolist'.
macrolist(_MCB, File) ->
    ?Dat([?Macro:name(M) ||
             M <- ?Query:exec(
                     ?Query:seq(?File:find(File), ?File:macros()))]).

%%% ----------------------------------------------------------------------------
%%% Clustering

%% @spec cl_options(#msg_cb{}, atom()) -> ok
%% @doc Requests the options of the given clustering algorithm.
cl_options(_MCB, Alg) ->
    OptList = cl_ui:cl_options(Alg),
    Ls = [[A,B] || {A,B} <- OptList],
    ?Dat([Alg]++Ls).

%% @spec cl_refresh(#msg_cb{}) -> ok
%% @doc Refreshes the Emacs clustering interface.
cl_refresh(_MCB) ->
    case cl_ui:refresh() of
        {cl_ui, recreated} ->
            ?OK;
        _ ->
            ?LErr(cl_ui_refresh) %@todo
    end.
% "cl_ui refresh: cleaned"

%% @spec run_cl(#msg_cb{}, proplist(), atom(), atom()) -> ok
%% @doc Invokes {@link cl_ui:run/1}. See the options there.
run_cl(_MCB, Opt, Alg, Create) ->
    {Clustering, FittNum} = cl_ui:run({Opt, Alg, Create}),
    ?Dat([{result,Clustering},
          {flist,?MISC:format("~w",[FittNum])}]).
% "Clustering algorithm finished."

%%% ============================================================================
%%% Backup

backup(_MCB, CommitLog) ->
    ?Graph:backup(CommitLog).

ls_backups(_MCB) ->
    ?Dat(?Graph:ls_backups()).

backup_info(_MCB, Backup) ->
    Result = ?Graph:backup_info(Backup),
    case {is_tuple(Result), size(Result)} of
        {true, 3} ->
            ?Dat(Result);
        _ ->
            {Error, _} = Result,
            ?LocalErr(Error, [Backup])
    end.

restore(MCB, Backup) ->
    load(MCB, restore, [Backup]).

undo(MCB, _File) ->
    load(MCB, undo, []).

load(MCB, Fun, Args) ->
    send_reset(MCB),
    FilesBefore = files_with_lastmod(),
    try apply(?Graph, Fun, Args) of
        ok  ->
            FilesAfter = files_with_lastmod(),
            ModifiedFiles =
                [ begin
                      ok = ?FileMan:save_file(File),
                      ?File:path(File) end ||
                    {File, LastMod} <- FilesAfter,
                    lists:keysearch(File, 1, FilesBefore) =/= LastMod ],
            send_change(MCB,[?Modified(F) || F <- ModifiedFiles]),
            ?OK;
        {Error, _} ->
            ?LocalErr(Error, Args);
        Error ->
            ?LocalErr(Error, Args)
    catch
        throw:Msg -> ?LErr(Msg)
    end.


create_graph(_MCB, Name) ->
    graph_op(create_graph, [Name]).

rename_graph(_MCB, OldName, NewName) ->
    graph_op(rename_graph, [OldName, NewName]).

ls_graphs(_MCB) ->
    graph_op(ls_graphs, []).

actual_graph(_MCB) ->
    graph_op(actual_graph, []).

load_graph(MCB, Name) ->
    send_reset(MCB),
    graph_op(load_graph, [Name]).

delete_graph(_MCB, Name) ->
    graph_op(delete_graph, [Name]).

delete_all_graphs(MCB) ->
    send_reset(MCB),
    graph_op(delete_all_graphs, []).

graph_op(FunAtom, Args) ->
    case apply(?Graph, FunAtom, Args) of
        ok ->
            ?OK;
        Result when is_list(Result) or is_atom(Result) ->
            ?Dat(Result);
        Error when is_tuple(Error) ->
            ?LocalErr(element(1, Error), Args);
        Error ->
            ?LocalErr(Error, Args)
    end.

%%% ----------------------------------------------------------------------------
%%% Attributes of the error form

%% @spec decode_error_form(#file{}, ErrList) -> ok
%% @doc This function can collect information about error forms.
decode_error_form(FileNode, ErrorForms) when is_list(ErrorForms) ->
    [decode_error_form(FileNode, ErrorForm) || ErrorForm <- ErrorForms];
decode_error_form(FileNode, ErrorForm) ->
    #form{tag=Tag} = ?Graph:data(ErrorForm),
    case Tag of
        Err = {no_include_file, _MissingInc} ->
            [Err];
        {include_error,_Includer, Err = {no_include_file,_Included}} ->
            [Err];
        {1, ErrTxt} when is_list(ErrTxt) ->
            ParseErrorMsg =
                re:replace(ErrTxt, "^[^:]+:([0-9:\\-]+) (Parse error before .+)",
                           "\\1 \\2", [{return, list}]),
            case ParseErrorMsg == ErrTxt of
                false -> [ParseErrorMsg];
                true  -> decode_possible_token_error(FileNode, Tag)
            end;
        _ ->
            decode_possible_token_error(FileNode, Tag)
    end.

decode_possible_token_error(FileNode, Tag) ->
    Token = paarse(Tag),
    {Position, Text} =
        case Token of
            [] ->
                N = ?File:length(FileNode),
                {{N,N}, "EOF"};
            _ ->
               {?Token:pos(Token), ?Token:text(Token)}
        end,
    [{nexttokentext, Text}, {position, Position}].


%%% ----------------------------------------------------------------------------
%%% Handling file status information

%% @spec status_info(#msg_cb{}, [string()]) -> ok
%%
%% @doc This function collets information about files loaded in the
%%      RefactorErl database.
%%      Parameter of the function: `[]' means that the function find all
%%      files form the database.
%%      When the parameter is a list of the file paths or it is one file path
%%      in a list, the function is collecting information about these files
%%      only.
%%      The message of the function is a `FileStatusList' that is a
%%      [proplist()] contains information about the file(s).
%%
%%      Elements of the `FileStatusList':
%%
%%      {file,string()}          The path and the name of the file
%%      {error,Errors}           The file contains errors: list @todo
%%      {type,atom()}            The type of the file:
%%                               `none'|`module'|`include'
%%      {lastmod,int()|atom()}   The last modification of th file. The
%%                               default value is `undefined'
%%      {present,boolean()}      Status of the file:  @todo
status_info(_MCB, FileList) ->
    ?Dat(file_status_info(FileList)).


%% @spec get_envs(#msg_cb{}) -> {ok, [{atom(), [any()]}]}
%% @doc Returns the environment settings (appbase, output etc.) as a proplist.
get_envs(_MCB) ->
    ?Dat(?Syn:get_envs()).

%%% ============================================================================
%%% Private implementation

%% @doc Parser for the error messages
paarse({_,Mesg})->
    case
        re:run(Mesg, "{{.+},{.+}}", [{capture, first}]) of
        {match, [{F, L}]} ->
            SToken = string:substr(Mesg, F+1, L),
            {ok, STerm, _}= erl_scan:string(SToken++"."),
            {ok, Tken} = erl_parse:parse_term(STerm),
            {_,Token} = Tken, Token;
        _ -> []
    end;
paarse(_) ->
    [].

%%% ----------------------------------------------------------------------------

add_filedir(MCB,File) ->
    case ?MISC:is_erl(File) of
        true  ->
            do_add_file(MCB,File);
        false ->
            true = ?MISC:is_beam(File),
            do_load_beam(MCB,File)
    end.

generate_filedir(_,File) ->
    case ?MISC:is_erl(File) of
        true  ->
            {referl_htmlserver:generate_call(File),ok};
        false ->
            {ok,error}
    end.

drop_filedir(MCB,File)->
    do_drop_file(MCB,File).

do_add_file(MCB,FileName)->
    case ?FileMan:add_file(FileName,
                           [update, {progress, progress(MCB,add)}]) of
        {error, Reason} ->
            send_change(MCB,[?AddedError(FileName,[])]), %@todo
            ?LErr(Reason);
        {file, File} ->
            Nodes = ?Query:exec(File, ?File:includes()),
            Paths = [?File:path(FN) || FN <- Nodes],
            send_change(MCB,[?AddedCorrect(F) || F <- Paths]),
            ?Dat(Nodes)
    end.

do_drop_file(MCB,FileName)->
    case ?Query:exec(?File:find(FileName)) of
        [File] ->
            Drop =
                [?File:path(FN) || FN <- ?Query:exec(File, ?File:included())],
            ?FileMan:drop_file(File, [{progress, progress(MCB,drop)}]),
			referl_htmlserver:drop(FileName),
            send_change(MCB,[?Dropped(F) || F <- Drop]),
            ?OK;
        [] ->
            ?RefErr(file_not_present,[FileName])
    end.

%% @doc Traverse a complete directory recursively while doing the action
%% specified on all "*.erl" and "*.beam" files each folder contains.
recurse_erl(MCB,Start=[_|_], Action) when is_function(Action,2) ->
    Files = erl_beam(MCB,Start),
    _Result = [Action(MCB,F) || F <- Files].
% @todo
    %% case Result /= [] of
    %%     true ->
    %%         case lists:any(fun(X)->X end, Result) of
    %%             false ->
    %%                 message(status, "~s", [error_message({none_proc,[]})]);
    %%             true ->
    %%                 case lists:any(fun(X)->not X end, Result) of
    %%                     true ->
    %%                         message(status, "~s",
    %%                                 [error_message({some_proc,[]})]);
    %%                     false ->
    %%                         ok
    %%                 end
    %%         end;
    %%     false ->
    %%         ok
    %% end.

%% @doc Traverse a complete directory recursively to collect
%% all "*.erl" and "*.beam" files each folder contains.
erl_beam(_MCB,Start) ->
    Files = ?MISC:find_files(
           filename:absname(Start),
           ?MISC:'or'(fun ?MISC:is_erl/1, fun ?MISC:is_beam/1)),
    case Files of
    [] ->
        throw(?LocalError(not_found,[Start]));
    _ ->
        FileMod = [{filename:rootname(filename:basename(F)), F}
               || F <- Files],
        {Erl,Beam0} = lists:partition(
                fun({_M,F})-> ?MISC:is_erl(F) end, FileMod),
        {ErlMod,_ErlFile} = lists:unzip(Erl),
        Beam = ?MISC:pdel(ErlMod, Beam0),
        All = Erl ++ Beam,
        [F || {_,F} <- All]
    end.

do_load_beam(MCB,File) ->
    load_beam(MCB,File,filename:dirname(File),false).

add_flat(Results0)->
    Results = lists:flatten(Results0),
    case lists:all(fun({ok,_})->true; % ?OK, ?Dat
                      (_)->false end,
                   Results) of
        true->
            Unpack = [N || {ok,N} <- Results], % ?OK, ?Dat
            ?Dat(lists:flatten(Unpack));
        false->
            ?LErr(somebad)
    end.

%%% ----------------------------------------------------------------------------

progress(MCB,Op) ->
    fun
        (File, Percent, FormCount, FormMax, KBps) ->
            send_progress(MCB, Op, File, Percent, FormCount, FormMax, KBps)
    end.

%% @doc Filters can be given either by their atomic abbreviations
%%      or by numeric codes.
convert_filter([]) -> all;
convert_filter(Types) when is_list(Types) -> [convert_filter(T) || T <- Types];
convert_filter(Type) when is_atom(Type) -> Type;
convert_filter(2) -> sem;
convert_filter(3) -> ctx;
convert_filter(4) -> syn;
convert_filter(5) -> synlex;
convert_filter(6) -> lex;
convert_filter(7) -> inp;
convert_filter(8) -> not_lex;
convert_filter(_) -> all.


files_with_lastmod() ->
    [{File, LastMod}
     || File <- ?Query:exec([file]),
        LastMod <- [(?ESG:data(File))#file.lastmod]].

file_status_info([])->
    Files = ?Query:exec([file]),
    [file_stat(FileName, FileNode) ||
        FileNode <- Files, FileName <- [?File:path(FileNode)]];

file_status_info(FileList)-> %@todo
    lists:map(
      fun(FileName) ->
              case ?Query:exec(?File:find(FileName)) of
                  [] ->
                      ?NotPresent(FileName);
                  [FileNode] ->
                      file_stat(FileName,FileNode)
              end
      end,
      FileList).

file_stat(Path,FileNode)->
    Type = ?File:type(FileNode),
    #file{lastmod=LastMod} = ?Graph:data(FileNode),
    case ?Query:exec(FileNode, [{form, {type, '==', error}}]) of
        [] -> ?PresentCorrectTM(Path, Type, LastMod);
        E  -> ?PresentErrorTM(Path, Type, LastMod,
                              decode_error_form(FileNode,E))
    end.

%%% ============================================================================

metric_mode(_MCB, Enable) ->
    case Enable of
        true ->
            refanal_metrics_helper:metricmode(on);
        false ->
            refanal_metrics_helper:metricmode(off)
    end,
    ?OK.
