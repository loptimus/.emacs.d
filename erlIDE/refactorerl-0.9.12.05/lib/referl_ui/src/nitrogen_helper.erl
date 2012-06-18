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

%%% @author Viktoria Fordos <v@fviktoria.hu>

-module(nitrogen_helper).
-vsn("$Rev: 8382 $ ").
-export([svg/2,start_nitrogen/0, start_nitrogen/1, stop_nitrogen/0]).
-export([get_nitrogen_index/0, get_nitrogen_root/0, get_nitrogen_apps_dir/0,
         get_nitrogen_site_dir/0]).
-export([get_images_root/0,get_file_browser_server_root/0,get_file_browser_loaded_files_root/0,
         get_loaded_files_list/1]).
-export([delete_from_qtab/2,query_list_from_qtab/1, update_prev_query_comment/2]).
-export([execute_query/1,execute_system_query/2,get_running_queries/0,
         get_running_queries/1, kill_query/1, set_alias/2, update_tab_if_needed/6]).
-export([get_appbases/0, del_appbase/1,add_appbase/1]).
-export([add_possible_warning/0, get_possible_warning/0]).
-export([add_to_db/2, drop_from_db/2, need_to_update/0]).
-export([do_autocomplete/1]).
-export([generate_dependency_graph/2, print_dependency_graph/2,
		 delete_dependency_graphs_files/1,
         get_loaded_directories/0, generate_fb_graph/2]).
-export([get_source/1, get_database_hash/0]).
-export([whitespaces_to_html/1, get_restricted_mode/0]).
-export([stop_running_queries/1,make_ui_request/2]).
-export([save_skeleton/3, list_skeletons/0, evaluate_skeleton/3, evaluate_skeleton/4, 
         delete_skeleton/1, skeleton_call_format/1, update_skeleton/3,
		 update_prev_skeleton_comment/2,determine_sq_request_type/1]).


-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").

-define(TAB, filename:join(mnesia:system_info(directory),"query_table_v5")).
-define(NROOT,"nitrogen").
-define(NSITE,?NROOT++"/site").
-define(NAPPS,?NROOT++"/apps").
-define(FB_TAB, filename:join(mnesia:system_info(directory),"file_browser_table")).
-define(RQ_TAB, filename:join(mnesia:system_info(directory),"running_queries_table")).
-define(S_TAB, filename:join(mnesia:system_info(directory),"skeleton_table_v2")).


-define(SPARAM_START, $$).
-define(SPARAM_END, $$).

-define(NITRO_ERROR, nitrogen_helper_error).

%%% @type safequery() = {SQueryString, File, Position}
%%%   SQueryString = string()
%%%   File = string()
%%%   Position = integer().

%%% ============================================================================
%%% Start, configure and stop nitrogen framework

%% @doc Starts nitrogen framework
start_nitrogen(["from_script", YPath, YName, YPort, YListen,BrowserRoot, ImgRoot, RestrictedMode]) ->
    start_nitrogen(YPath, YName, YPort, YListen, BrowserRoot, ImgRoot, RestrictedMode);

start_nitrogen(PropList) ->
    YPort = proplists:get_value(yaws_port,PropList,"8001"),
    YListen = proplists:get_value(yaws_listen,PropList,"127.0.0.1"),
    YName = proplists:get_value(yaws_name,PropList,"localhost"),
    YPath = proplists:get_value(yaws_path,PropList,"no_path"),
    BRoot = proplists:get_value(browser_root,PropList,"no_path"),
    IRoot = proplists:get_value(images_dir,PropList,"no_path"),
    ImgRoot=case IRoot of
            "no_path" -> mnesia:system_info(directory);
            Dir -> Dir
            end,
	RestrictedMode = proplists:get_value(restricted_mode,PropList,false),
    start_nitrogen(YPath, YName, YPort, YListen, BRoot, ImgRoot, RestrictedMode).

%% @doc Starts nitrogen framework
start_nitrogen() ->
    start_nitrogen([]).

start_nitrogen(YPath, YName, YPort, YListen, BrowserRoot, ImgRoot, RestrictedMode)->
    try    
        load_needed(),
        init_fbtab(),
        init_rqtab(),
        set_file_browser_docroot(BrowserRoot),
        set_image_root(ImgRoot),
		set_restricted_mode(RestrictedMode),
        PropList=make_proplist(YPath, YName, YPort, YListen),
        
        application:start(nprocreg),
        web_helper:start_yaws(PropList)
    catch
        RefErr ->
            io:format(?Error:error_text(RefErr) ++ "~n"),
            io:format("~p~n",[RefErr]),
            exit("Cannot start nitrogen.")
    end.

%% @doc Stop nitrogen framework
stop_nitrogen()->
    close_qtab(),
    application:stop(yaws).

%%% ============================================================================
%%% Getters for nitrogen framework

%% @doc Returns nitrogen's docroot path
%% @spec get_nitrogen_index()-> string()
get_nitrogen_index()->
    filename:join([get_base(),?NSITE,"static"]).

%% @doc Returns nitrogen's root path
%% @spec get_nitrogen_root()-> string()
get_nitrogen_root()->
    filename:join([get_base(),?NROOT]).

%% @doc Returns nitrogen's apps path
%% @spec get_nitrogen_apps_dir()-> string()
get_nitrogen_apps_dir()->
    filename:join([get_base(),?NAPPS]).

%% @doc Returns nitrogen's site path
%% @spec get_nitrogen_site_dir()-> string()
get_nitrogen_site_dir()->
    filename:join([get_base(),?NSITE]).

%% @doc Returns the root dir(s) of browsable files in server
%% @spec get_file_browser_server_root()-> [string()]
get_file_browser_server_root()->
    [{_,Result}]=find_in_fbtab("server_docroot"),
    Result.

%% @doc Returns the root dir(s) of images
%% @spec get_images_root()-> [string()]
get_images_root()->
    [{_,Result}]=find_in_fbtab("images_docroot"),
    Result.

%% @doc Returns the restricted mode flag.
%% @spec get_restricted_mode() -> atom()
get_restricted_mode() ->
	[{_,Result}]=find_in_fbtab("restricted_mode"),
	list_to_existing_atom(Result).

%% @doc Returns the root dir(s) of browsable files
%% which previously loaded to database
%% @spec get_file_browser_loaded_files_root()-> [string()]
get_file_browser_loaded_files_root()->
    [{_,Result}]=find_in_fbtab("db_docroot"),
    Result.

%% @doc Returns a list of filenames, where file's parent is the given argument,
%% and file's been loaded to database 
%% @spec get_loaded_files_list(string())-> [string()]
get_loaded_files_list(Parent)->
    {result,FileList}=nitrogen_helper:execute_system_query("files",
                                         {query_display_opt,
                                          [{positions, scalar}],
                                          needed_pattern,$:}),
    lists:foldl(fun(File, List)->
                        case lists:prefix(Parent, File) of
                            true -> List ++ [File];
                            false -> List 
                        end 
                end, [], lists:map(fun(F)->lists:sublist(F,length(F)-1) end,
                                   FileList)).

%%% ============================================================================
%%% Operations on 'query_table_v5' dets table

%% @doc Returns previous queries from the given user in the query_table_v5
%% @spec query_list_from_qtab( string() )->[] | [ Item :: term() ]
%% Item = {SafeQuery, Query, Alias, Users}
%%   SafeQuery = safequery()
%%   Alias = string()
%%   Users = [ string() ]
query_list_from_qtab(Usr) ->
    L = find_in_qtab_by_pattern({'_','_','_','_','_','_','_'}),
    L2 = case Usr of
            "all" -> L;
            _ -> lists:filter(
                    fun({_SafeQuery,_Q,_A,_C,_R,Users,_H}) ->
                        lists:member(Usr,Users)
                    end, L)
        end,
    lists:keysort(3,lists:map(
                    fun ({SafeQuery,Q,A,C,_R,U,_H})->
                            {SafeQuery,Q,A,C,U} 
                    end, L2)).

update_prev_query_comment(SafeQuery, NewComment)->
	[{_SafeQuery,Query,Alias,_Comment,Result,UserList,Hash}] = find_in_qtab(SafeQuery),
	insert_to_qtab(SafeQuery,Query,Alias,NewComment,Result,UserList,Hash).

%% @doc Deletes the given query from the given user in the query_table_v5
%% @spec delete_from_qtab({SQueryString, File, Positon},string()) -> ok
delete_from_qtab(SafeQuery,admin) ->
    [{SafeQuery,_Query,_Alias,_Comment,_Result,_UserList,_Hash}] = find_in_qtab(SafeQuery),
    delete_from_qtab(SafeQuery);

delete_from_qtab(SafeQuery,Usr) ->
    [{SafeQuery,Query,Alias,Comment,Result,UserList,Hash}] = find_in_qtab(SafeQuery),
    UserList2 = lists:delete(Usr,UserList),
    case UserList2 of
        [] -> delete_from_qtab(SafeQuery);
        _ -> insert_to_qtab(SafeQuery,Query,Alias,Comment,Result,UserList2,Hash)
    end.

%% @doc Initialize query_table_v5
%% @spec init_qtab() -> ok
%% @deprecated
init_qtab() ->
    dets:open_file(?TAB,[]).

%% @doc Close query_table_v5
%% @spec close_qtab() -> ok
%% @deprecated
close_qtab() ->
    dets:close(?TAB).

%%% ============================================================================
%%% Semantic Queries

%% @doc Returns the given query's result
%% and administrate the action if necessary
%% @spec execute_query({QueryType, Query, User} | 
%%                      {QueryType, SafeQuery, User} |
%%                      {QueryType, Query, Alias, User} | 
%%                      {QueryType, SafeQuery, Alias, User} |
%%                      noquery) -> {Result, Warning, Error}
%%   QueryType = 'query' | prev_query | unknown
%%   Query = string()
%%   User = string()
%%   Alias = string()
%%   SafeQuery = safequery()
%%   Result = {result, no_result | [term()]}
%%   Warning = {warning, no_warning | string()}
%%   Error = {error, no_error | string()}
execute_query({_,[],_}) ->
    execute_query(noquery);

execute_query({_,[],_,_}) ->
    execute_query(noquery);

execute_query({'query',Q=[C|_],User}) when is_integer(C)->
    execute_query({'query',{Q, undefined, undefined},User});

execute_query({'query',{Query, File, Pos},User}) ->
    init_qtab(),
    SafeQuery=lists:filter(query_filter_fun(), Query),
    case find_in_qtab({SafeQuery, File, Pos}) of
        [_] -> execute_query({prev_query,{SafeQuery, File, Pos},User});
        [] -> execute_query({'query',{Query, File, Pos}, Query, User})
    end;

execute_query({'query',Q=[C|_], Alias, User}) when is_integer(C)->
    execute_query({'query',{Q, undefined, undefined},Alias, User});

execute_query({'query',{Query, File, Pos}, Alias, User}) ->
    init_qtab(),
    SafeQuery=lists:filter(query_filter_fun(), Query),
    StartOpt=case {File, Pos} of
                 {[C|_],P} when is_integer(C) andalso is_integer(P)-> 
                     [{file, File},{position, Pos}];
                 _ ->[]
             end,
    case calculate_result(Query,[{positions,scalar}, {output,other}], 
                          StartOpt,[{save_query_id, true}, {user, User}]) of
        {result, Result} ->
            [R0] = ?MISC:pgetu([result],Result),
            R = io_lib:format("~p",[R0]),
            Hash = get_database_hash(),
            update_tab_if_needed({SafeQuery, File, Pos},Query,User,Alias,R,Hash),
            {{result,R0},{warning,get_possible_warning()},{error,no_error}};
        E ->
            {{result, no_result},{warning,no_warning},{error,error_handler(E)}}
    end;

execute_query({prev_query,SQ={_Query, node, Node},U}) ->
    init_qtab(),
    [{_SQ,Q,A,_C,R,_U, H}] = find_in_qtab(SQ),
    case is_database_changed(H) of
        true -> 
			try 
				?ESG:data(Node),
				Fun=main:nodeQueryFun(Q,{Node,Node},referl_htmlgen:getType(Node),U),
				Fun()
			catch
				_:_ -> {error,Q,"Starting node does not exist anymore."}
	        end;
        false ->
            update_tab_if_needed(SQ,Q,U,A,R,H),
            {R,Q,referl_htmlgen:getType(Node),Node}
    end;

execute_query({prev_query,SQ={_Query, File, Pos},U}) ->
    init_qtab(),
    [{_SQ,Q,A,_C,R,_U, H}] = find_in_qtab(SQ),
    case is_database_changed(H) of
        true -> execute_query({'query', {Q, File, Pos}, A, U});
        false ->
            update_tab_if_needed(SQ,Q,U,A,R,H),
            {{result,list_to_term(lists:flatten(R))},
             {warning,get_possible_warning()},
             {error,no_error}}
    end;

execute_query({unknown,QData={Query, File, Pos},User})->
    case determine_sq_request_type(Query) of
        sem_query -> execute_query({'query',{Query, File, Pos},User});
        skeleton -> try_parse_skeleton(QData, User)%;
        %_ -> ErrorStr="Error: Type could not determined.",
        %    {{result, no_result},{warning,no_warning},{error,ErrorStr}}
    end;

execute_query(noquery) ->
    [{result,[]}].

%% @doc Returns the given query's result in list
%% @spec execute_system_query(string(),
%%                            {query_display_opt,
%%                            proplist(),
%%                            needed_pattern,
%%                            char()}) -> [] | [string()]
execute_system_query(Query,{query_display_opt,Option,needed_pattern,Pattern})->
    {result,R}=calculate_result(Query,Option),
    [Result] = ?MISC:pgetu([result],R),
    get_result_list(Result,Pattern).


%% @doc Set the given name as alias for the given query
%% @spec set_alias(safequery(), string) -> ok | {error, string()}

set_alias(SafeQuery, Alias=[I|_]) when is_integer(I)->
    case find_in_qtab(SafeQuery) of
                [] -> {error, "Query was not found."};
                [{_SQ,Q,_A,C,R,Us,H}] ->insert_to_qtab(SafeQuery,Q,Alias,C,R,Us,H),
                                      ok
    end.

%%% ============================================================================
%%% Running queries
%% @doc Returns the list of the currently running queries.
%% @spec get_running_queries()-> [] | [{QueryId, QueryString}]
%%  QueryId = number()
%%  QueryString = string()
get_running_queries()->
    RunningQueries=case make_ui_request({get_running_queries}) of
        {error, _ } -> [];
        {ok, R} ->  R
    end,
    [{QId,proplists:get_value(querystr,Args)} || 
     {QId, _Pid, _Mod, Args} <- RunningQueries].


%% @doc Returns the list of the currently running queries and marks queries
%% which belong to the given user.
%% @spec get_running_queries(User)-> [] | [{QueryId, QueryString, Ownership}]
%%  User = string()
%%  QueryId = number()
%%  QueryString = string()
%%  Ownership = own | extraneous
get_running_queries(User=[C|_]) when is_integer(C)->
    lists:foldl(fun ({_,Usr,QueryId, QStr},AccIn) when Usr =:=User->
                        AccIn++[{ QueryId, QStr, own}];
                   ({_,_,QueryId, QStr},AccIn) ->
                       AccIn++[{ QueryId, QStr, extraneous}]
                end
               , [], get_rqtab_elements());

get_running_queries(_)->[].


%% @doc Kills the query which belongs to the given query id.
%% @spec kill_query(number()) -> not_found | ok
kill_query(QueryId) when is_integer(QueryId)->
    case make_ui_request({kill_query,QueryId}) of
        {error, _ } -> not_found;
        {ok, R} ->  delete_from_rqtab({'_','_',QueryId,'_'}),
                    R
    end;

kill_query(_)->
    not_found.
%%% ============================================================================
%%% Autocomplete semantic queries

%% @doc Returns the possible completions of the given query string.
%% @spec do_autocomplete(string()) -> [] | [{CompletedQS, Completion}]
%% CompletedQS = string()
%% Completion = string()
do_autocomplete(QueryString) when is_atom(QueryString) ->
    do_autocomplete(atom_to_list(QueryString));

do_autocomplete(QueryString)->
    List1=
    try
        List=refusr_ac:run(filtered_query(QueryString)),
        [{lists:flatten(InCompleteEnt++Completion), 
          lists:flatten(QueryString++Completion)}||
          {InCompleteEnt,Completion=[C|_]}<-List, is_list(InCompleteEnt),is_integer(C)]
    catch _:_ -> []
    end,
    List2=
    try
        do_autocomplete_skeleton(QueryString)
    catch _:_ -> []
    end,
    lists:keysort(2, List1++List2).
%%% ============================================================================
%%% Helper functions for Nitrogen framework

%% @doc Turns whitespaces into HTML tags.
%% @spec whitespaces_to_html(string()) -> string()
whitespaces_to_html(String) ->
lists:map(fun (E)->
                if 
                    E == 10 -> "<br/>";
                    E == 9  -> "&nbsp;&nbsp;";
                    E == 32 -> "&nbsp;";
                    true -> E
                end
          end, String).

%% @doc Returns file contents of the given filename
%% @spec get_source(string())-> {data, string()} | {error, string()}
get_source(FileName)->
    case file:open(FileName,[read]) of
        {ok,_IODevice} ->
            case file:read_file(FileName) of
                {ok, Data} -> {data,Data};
                {error,Reason} -> {error, "Error: " ++ Reason}
            end;
        {error,Reason} ->
            {error, io_lib:format("Error: ~p",[Reason])}
    end.

%% @doc Kill all of the running queries which belong to the given user
%% @stop_running_queries(string()|atom())-> ok
stop_running_queries(User)->
    [kill_query(QueryID) || {_,_,QueryID,_} <- find_in_rqtab_by_user(User)],
    ok.
%%% ============================================================================
%%% Enviroment Nodes

%% @doc Returns all of the appbase nodes
%% @spec get_appbases() -> [] | [string()]
get_appbases()->
    ?Syn:get_env(appbase).

%% @doc Deletes the given appbase node
%% @spec del_appbase(string()) -> ok | not_found
del_appbase(Path)->
    case ?Syn:del_env_val(appbase,Path) of
        [ok] -> ok;
        [] -> not_found
    end.

%% @doc Adds the given appbase node
%% @spec add_appbase(string()) -> ok | {error, string()}
add_appbase(Path=[C|_]) when is_integer(C) ->
    case filelib:is_dir(Path) of
        true -> refcore_syntax:add_env(appbase, Path);
        false -> {error, Path++" is not a directory."}
    end.

%%% ============================================================================
%%% Database's operations

%% @doc Add the given file, or directory to database
%% @spec add_to_db(string(), fun()) -> {result, ok} | {error, string()}
add_to_db(File=[C|_], PrinterFun) when is_integer(C) andalso is_function(PrinterFun)->
    case io_lib:deep_char_list(File) of
        true ->
            case make_ui_request({add_dir,File},[{remote_printer_fun, PrinterFun}]) of
                {error, {_Code, Msg}} ->{error, "Error: " ++ Msg};
                R -> {result, R}
            end;
        false ->
            {error,"Error: bad file argument given"}
    end;
add_to_db(_,_)->
    {error, "Bad arguments given"}.

%% @doc Drop the given file from database
%% @spec drop_from_db(string(), PrinterFun::fun())-> {result, ok} | {error, string()}
drop_from_db(File=[C|_], PrinterFun) when is_integer(C) andalso is_function(PrinterFun)->
    case make_ui_request({drop,File},[{remote_printer_fun, PrinterFun}]) of
        {error, {_Code, Msg}} ->{error, "Error: " ++ Msg};
        R -> {result, R}
    end;
drop_from_db(_,_)->
    {error, "Bad arguments given"}.

%% @doc Updates different root directory lists
%% @spec need_to_update() -> true
need_to_update() ->
    OldServerDocRoot = get_file_browser_server_root(),
    case OldServerDocRoot == get_file_browser_loaded_files_root() of
        true -> set_file_browser_docroot("no_path");
        false -> set_file_browser_docroot(OldServerDocRoot)
    end,
    true.

%% @doc Returns warning message if database contains files with error forms
%% @spec get_possible_warning() -> no_warning | string()
get_possible_warning() ->
    case is_error_in_database() of
        false -> no_warning;
        true ->   error_handler(warning)
    end.


%% @doc Returns the list of errors if database contains any file with error form
%% @spec add_possible_warning() -> no_warning | {warning, Warning}
%% Warning = {FilePath :: string(), 
%%            StartPos :: integer(), 
%%            Length :: integer(),
%%            ErrorMessage :: string()}
add_possible_warning() ->
    case get_error_forms_in_database() of
        [] -> no_warning;
        Errors ->   error_handler({warning, Errors})
    end.

%%% ============================================================================
%%% Dependency graph
%% @doc Converts to SVG from the given .dot file
%% @spec svg(string(), lists()) -> ok | {error, string()}
svg(Source=[A|_], Target=[B|_]) when is_list(Source) andalso is_list(Target)
  andalso is_integer(A) andalso is_integer(B)->
    case file:read_file_info(Source) of
        {ok, _ } -> ok;
        {error,E} -> throw(?RefError(file_not_exists,[Source,E]))
    end,
    Res = os:cmd("dot -Tsvg "++Source++" -o"++Target),
    case Res of
        [] -> ok;
        _  -> throw(?RefError(os_error,[Res]))
    end.

%% @doc Generates dependecy graph in module or function level with the given options
%% @spec generate_dependency_graph(proplist(),string()) -> 
%% {error, string()} | 
%% {string(), string()}
generate_dependency_graph(Options,User) when is_atom(User)->
    generate_dependency_graph(Options,atom_to_list(User));

generate_dependency_graph(Options,User) when is_list(User)->
    try
    FileName=User++".dot",
    [{_,TargetDir}]=find_in_fbtab("images_docroot"),
    DotName=filename:join([TargetDir,FileName]),
    XOptions=Options++[{dot, DotName}],
    case proplists:get_value(level, XOptions) of
        mod -> generate_dep_graph_mod_level(proplists:delete(level, XOptions));
        func -> generate_dep_graph_fun_level(proplists:delete(level, XOptions))
    end,
    SvgName=filename:join([TargetDir,User++".svg"]),
    svg(DotName,SvgName),
    {DotName, SvgName}
    catch
       {error, written}->{error, "An error occurred while processing the request."};
       {error, ErrM=[I|_]} when is_integer(I)->{error, ErrM};
       {error, ErrM} when is_atom(ErrM)->{error, io_lib:format("~p", [ErrM])};
       {error, ErrMStr=[I|_], ErrMParams} when is_integer(I) andalso is_list(ErrMParams)->
           {error, io_lib:format(ErrMStr, ErrMParams)};
       Err={M_,_,_} when is_atom(M_) ->
           {error, ?Error:error_text(Err)};
       Err->{error, io_lib:format("~p", [Err])}
   end;

generate_dependency_graph(_Options,_User) ->
    {error, "Bad argument was given"}.

%% @doc Generates dependecy graph in module or function level with the
%% given options
%% @spec print_dependency_graph(proplist(),string()) -> 
%% {error, string()} | 
%% {string(), string()}
print_dependency_graph(Options,User) when is_atom(User)->
    generate_dependency_graph(Options,atom_to_list(User));

print_dependency_graph(Options,User) when is_list(User)->
    try
    case proplists:get_value(level, Options) of
        mod -> {generate_dep_graph_mod_level(proplists:delete(level, Options)),mod};
        func -> {generate_dep_graph_fun_level(proplists:delete(level, Options)),func}
    end
    catch
       {error, written}->{error, "An error occurred while processing the request."};
       {error, ErrM=[I|_]} when is_integer(I)->{error, ErrM};
       {error, ErrM} when is_atom(ErrM)->{error, io_lib:format("~p", [ErrM])};
       {error, ErrMStr=[I|_], ErrMParams} when is_integer(I) andalso is_list(ErrMParams)->
           {error, io_lib:format(ErrMStr, ErrMParams)};
       Err={M_,_,_} when is_atom(M_) ->
           {error, ?Error:error_text(Err)};
       Err->{error, io_lib:format("~p", [Err])}
   end;

print_dependency_graph(_Options,_User) ->
    {error, "Bad argument was given"}.

%% @doc Deletes all of the files which are associated to the user
%% @spec delete_dependency_graphs_files(string()) -> ok
delete_dependency_graphs_files(User) when is_atom(User)->
    delete_dependency_graphs_files(atom_to_list(User));

delete_dependency_graphs_files(User) when is_list(User)->
    [{_,TargetDir}]=find_in_fbtab("images_docroot"),
    case file:list_dir(TargetDir) of
        {ok,FileNames}->
            lists:foreach(fun(Elem) ->
                                 case lists:prefix(User++"_",Elem) 
                                 andalso (lists:suffix(".dot",Elem) 
                                 orelse lists:suffix(".svg",Elem)) of
                                    true -> 
                                        file:delete(
                                            filename:join([TargetDir, Elem]));
                                    false -> ok
                                 end
                          end, FileNames);
        _ -> ok
    end.

%% @doc Returns the list of the loaded directories
%% @spec get_loaded_directories() -> [] | [string()]
get_loaded_directories()->
    case refusr_dir_sort:sort() of
        {error, _}-> [];
        Dirs -> [Dir || {Dir, _Mods}<-Dirs, is_list(Dir), Dir/="Other"]
    end.

%% @doc Generates dependency graph in functionblock level with the given options
%% @spec generate_fb_graph(proplist(), string()) -> 
%% {DotName, SvgName} | {error, ErrorMessage}
%% DotName = string()
%% SvgName = string()
%% ErrorMessage = string()
generate_fb_graph(Subjects, User) when is_list(Subjects) andalso is_list(User)->
    try
        FileName=User++".dot",
        [{_,TargetDir}]=find_in_fbtab("images_docroot"),
        DotName=filename:join([TargetDir,FileName]),
        Options=[{regexp, Subjects}, {type, draw}, {dot, DotName}],
        refusr_fb_regexp:re(Options),
        SvgName=filename:join([TargetDir,User++".svg"]),
        svg(DotName,SvgName),
        {DotName, SvgName}
    catch
       {error, written}->{error, "An error occurred while processing the request."};
       {error, ErrM=[I|_]} when is_integer(I)->{error, ErrM};
       {error, ErrM} when is_atom(ErrM)->{error, io_lib:format("~p", [ErrM])};
       {error, ErrMStr=[I|_], ErrMParams} when is_integer(I) andalso is_list(ErrMParams)->
           {error, io_lib:format(ErrMStr, ErrMParams)};
       Err={M_,_,_} when is_atom(M_) ->
           {error, ?Error:error_text(Err)};
       Err->{error, io_lib:format("~p", [Err])}
   end;

generate_fb_graph(_Options,_User) ->
    {error, "Bad argument was given"}.

%%% ============================================================================
%%% Skeletons
%% @doc Replaces the body and the owner of the skeleton, 
%% which is identified by the given name,
%% with the given new body and with the given owner.
%% @spec update_skeleton(Name::string(), NewBody::string(), Owner::string())->
%% ok | {error, ErrorMessage::string()}
update_skeleton(Name=[A|_], NewBody=[B|_], Owner=[C|_]) when is_integer(A) andalso
    is_integer(B) andalso is_integer(C) ->
    try
        check_skeleton_syntax(NewBody),
        [{_,_,_,_,Comment}] = find_in_stab_by_name(Name),
        save_safe_skeleton(Name, NewBody, Owner, Comment)
    catch
        {?NITRO_ERROR, ErrorMessage}->{error, ErrorMessage};
        _:_ -> {error, "Unknown error occured"}
    end.

%% @doc Updates the comment of a previously stored skeleton based on 
%% the given skeleton name.
%% @spec update_prev_skeleton_comment(Name::string(), NewComment::string())-> ok
update_prev_skeleton_comment(Name=[A|_], NewComment) when is_integer(A)
    andalso is_list(NewComment)->
    [{_,Body,Owner,_Card,_}] = find_in_stab_by_name(Name),
	save_safe_skeleton(Name, Body, Owner, NewComment).

%% @doc Deletes the skeleton which is identified by the given name
%% @spec delete_skeleton(Name::string())-> ok
delete_skeleton(Name)->
    delete_from_stab({Name, '_', '_', '_', '_'}).

%% @doc Composes a valid skeleton call based on the given skeleton name.
%% @spec skeleton_call_format(Name::string())->string()
skeleton_call_format(Name=[C|_]) when is_integer(C)->
    [{_,_,_,Card,_}] = find_in_stab_by_name(Name),
    ParamStr=case Card of
                 0 -> "";
                 1 -> " `_` ";
                 N -> string:copies(" `_` ,", N-1)++" `_` "
             end,
    Name++"("++ParamStr++").".

%% @doc Saves a new skeleton from the given parameters.
%% @spec save_skeleton(Name::string(), Body::string(), Owner::string())->
%% ok | {error, ErrorMessage::string()}
save_skeleton(Name=[A|_], Body=[B|_], Owner=[C|_]) when is_integer(A) andalso
    is_integer(B) andalso is_integer(C) ->
    case validate_skeleton(Name, Body) of
        ok -> save_safe_skeleton(Name, Body, Owner, "");
        Err={error, _} -> Err
    end.

%% @doc Returns a list of the data of the stored skeletons.
%% @spec list_skeletons()->[]|[Skeleton]
%% Skeleton = {Name, Body, Owner, ParamCardinality, Comment}
%% Name = string()
%% Body = string()
%% Owner = string()
%% ParamCardinality = integer()
%% Comment = string()
list_skeletons()->
    get_stab_elements().

%% @doc Evaluate the skeleton, which is identified by the given name, 
%% with the given actual parameters than try to execute it as a semantic query string.
%% @spec evaluate_skeleton(Name::string(), Parameters, User::string()) ->
%% {Result, Warning, Error}
%%   Result = {result, no_result | [term()]}
%%   Warning = {warning, no_warning | string()}
%%   Error = {error, no_error | string()}
%%   Parameters = []| [string()]
evaluate_skeleton(Name=[A|_], Parameters, User=[B|_]) when is_integer(A) 
  andalso is_integer(B) andalso is_list(Parameters)->
    try
        case find_in_stab_by_name(Name) of
            [] ->throw({?NITRO_ERROR, "Skeleton was not found."});
            [Result={_, Body, _, _, _}] -> 
                check_skeleton_params_match(Result, Parameters),
                Query=convert_to_sq(Body, Parameters),
                execute_query({'query', Query, User})
        end
    catch
        {?NITRO_ERROR, ErrorMessage}->{{result, no_result},
                                        {warning,no_warning},
                                        {error,ErrorMessage}};
        {_, _} -> {{result, no_result},
                    {warning,no_warning},
                    {error,"Unknown error occured"}}
    end.

evaluate_skeleton(Name=[A|_], Parameters, _User, onlyconvert) when is_integer(A) 
  andalso is_list(Parameters)->
    try
        case find_in_stab_by_name(Name) of
            [] ->throw({?NITRO_ERROR, "Skeleton was not found."});
            [Result={_, Body, _, _, _}] -> 
                check_skeleton_params_match(Result, Parameters),
                convert_to_sq(Body, Parameters)
        end
    catch
        {_, _} -> error
    end.
%%% ============================================================================
%%% helper functions
do_autocomplete_skeleton(Call)->
    lists:foldl(fun({Name,_,_,_,_}, Acc)->
                          FSkel = skeleton_call_format(Name),
                          case lists:prefix(Call, FSkel) of
                              true-> Acc++[{FSkel, FSkel}];
                              false -> Acc
                          end
                end , [],lists:keysort(1, list_skeletons())).
    
determine_sq_request_type(Call=[C|_]) when is_integer(C)->
    try
        check_skeleton_call_syntax(Call),
        skeleton_name_registered(Call),
        skeleton
    catch
        {_, _} -> sem_query
    end.

try_parse_skeleton({Call, _File, _Pos}, User)->
    {ok, Elements, _}=erl_scan:string(Call),
    {_,_, SkelName}=hd(Elements),
    %ParamsList=lists:sublist(Elements, 3, length(Elements)-4),
    %Parameters=[atom_to_list(element(3, Item)) || 
    %              Item <- ParamsList, is_tuple(Item), element(1, Item)==atom],
    ParamsList=re:split(Call,"[``]",[{return,list}]),
    Parameters=case length(ParamsList) of
                   1 ->[]; %skeleton can contain 0 parameter, too
                   Length -> lists:sublist(ParamsList, 2, Length-2)
               end,
    evaluate_skeleton(atom_to_list(SkelName), Parameters, User).

check_skeleton_params_match({_,_,_,ParamCardinality,_}, Parameters) when is_list(Parameters)->
    case length(Parameters)==ParamCardinality of
        true-> ok;
        false -> throw({?NITRO_ERROR, 
                        "Cardinality of the actual parameters does not match the cardinality of the formal parameters."})
    end.

convert_to_sq(Body, []) -> Body;

convert_to_sq(Body=[A|_], [HParam|Params]) when is_integer(A) 
  andalso is_list(HParam) andalso is_list(Params)->
    Pattern="(["++[?SPARAM_START]++"][^"++[?SPARAM_END]++"]*["++[?SPARAM_END]++"])",
    {ok, Mp}=re:compile(Pattern,[]),
    NewBody=re:replace(Body,Mp,HParam,[{return, list}]),
    convert_to_sq(NewBody, Params).

save_safe_skeleton(Name=[A|_], Body=[B|_], Owner=[C|_], Comment) when is_integer(A) andalso
    is_integer(B) andalso is_integer(C) andalso is_list(Comment)->
    ParamCardinality=countSkeletonParameters(Body),
    insert_to_stab(Name, Body, Owner, ParamCardinality, Comment).

validate_skeleton(Name=[A|_], Body=[B|_]) when is_integer(A) andalso is_integer(B)->
    try
        check_skeleton_syntax(Body),
        has_unique_skeleton_name(Name),
        ok
    catch
        {?NITRO_ERROR, ErrorMessage}->{error, ErrorMessage};
        {_, _} -> {error, "Unknown error occured"}
    end;

validate_skeleton(_,_)->
    {error, "Bad args"}.

%This clause should be used when the start symbol of and the end symbol of the skeleton's parameters are not equal to each other.
%countSkeletonParameters(Body=[A|_]) when is_integer(A) andalso ?SPARAM_START/=?SPARAM_END ->
%    StartSymbols=lists:filter(fun(E)->E==?SPARAM_START end, Body),
%    length(StartSymbols);

countSkeletonParameters(Body=[A|_]) when is_integer(A) andalso ?SPARAM_START==?SPARAM_END ->
    StartSymbols=lists:filter(fun(E)->E==?SPARAM_START end, Body),
    length(StartSymbols) div 2.

%This clause should be used when the start symbol of and the end symbol of the skeleton's parameters are not equal to each other.
%check_skeleton_syntax(Body=[A|_]) when is_integer(A) andalso ?SPARAM_START/=?SPARAM_END->
%    StartSymbols=lists:filter(fun(E)->E==?SPARAM_START end, Body),
%    EndSymbols=lists:filter(fun(E)->E==?SPARAM_END end, Body),
%    case length(StartSymbols)==length(EndSymbols) of
%        true-> ok;
%        false -> throw({?NITRO_ERROR,
%                        "Cardinality of the start symbol does not match the cardinality of the end symbol."})
%    end;

check_skeleton_syntax(Body=[A|_]) when is_integer(A) andalso ?SPARAM_START==?SPARAM_END->
    Symbols=lists:filter(fun(E)->E==?SPARAM_START end, Body),
    case even(length(Symbols)) of
        true-> ok;
        false -> throw({?NITRO_ERROR,
                        "Cardinality of the start symbol does not match the cardinality of the end symbol."})
    end.

skeleton_name_registered(Call)->
    Name=lists:takewhile(fun( $( )->false; (_)-> true end, Call),
    case length(find_in_stab_by_name(Name)) of
        0 -> throw({?NITRO_ERROR, Name++" does not exist as a skeleton name."});
        _ -> ok
    end.

skeleton_call_pattern()->
    Name="[^()`\t\n .]+",
    Open="[(]",
    Close="[)]",
    Dot="\.",
    NullParam="[\t\n ]*",
    Param="([\t\n ]*`[^`]*`[\t\n ]*)",
    Params=Param++"(,"++Param++")*",
    ParamList="("++NullParam++"|"++Params++")",
    Name++Open++ParamList++Close++Dot.

check_skeleton_call_syntax(Call=[C|_]) when is_integer(C)->
    {ok, Mp}=re:compile(skeleton_call_pattern(),[]),
    case re:run(Call,Mp) of
        nomatch -> throw({?NITRO_ERROR, "Syntax error in skeleton."});
        _ -> ok
    end.

has_unique_skeleton_name(Name=[A|_]) when is_integer(A)->
    case length(find_in_stab_by_name(Name)) of
        0 -> ok;
        _ -> throw({?NITRO_ERROR, Name++" already exists as a skeleton name."})
    end.

even(A) when is_integer(A)->
    A rem 2 == 0.

generate_dep_graph_fun_level(Options)    ->
    Opts=proplists:delete(type, Options),
    case proplists:get_value(type, Options) of
        all -> refusr_cyclic_fun:draw(Opts);
        cycles -> refusr_cyclic_fun:draw_cycles(Opts);
        none -> case proplists:get_value(gnode, Options) of
                undefined -> throw(?RefError(missing_arg, ["Bad args given"]));
                _ -> refusr_cyclic_fun:draw(Opts)
                 end;
        _ -> throw(?RefError(missing_arg, ["Bad args given"]))
    end.

generate_dep_graph_mod_level(Options) ->
    Opts=proplists:delete(type, Options),
    case proplists:get_value(type, Options) of
        all -> refusr_cyclic_mod:draw(Opts);
        cycles -> refusr_cyclic_mod:draw_cycles(Opts);
        none -> case proplists:get_value(gnode, Options) of
                undefined ->     throw(?RefError(missing_arg, ["Bad args given"]));
                _ -> refusr_cyclic_mod:draw(Opts)
                 end;
        _ -> throw(?RefError(missing_arg, ["Bad args given"]))
    end.


get_result_list(QueryResult,NeededPattern)->
    ResultList=string:tokens(QueryResult," \t\n"),
    case NeededPattern of
        none -> {result,ResultList};
        _      -> {result, lists:filter(fun(E)->
                                             case string:chr(E,NeededPattern) of
                                                 0 -> false;
                                                 _ -> true 
                                             end 
                                      end,ResultList)}
    end.

set_image_root(ImgRoot=[C|_]) when is_integer(C) ->
    insert_to_fbtab("images_docroot",ImgRoot);

set_image_root(ImgRoot) when is_atom(ImgRoot) ->
    set_image_root(atom_to_list(ImgRoot)).

set_restricted_mode(RM=[C|_]) when is_integer(C) ->
    insert_to_fbtab("restricted_mode",RM);

set_restricted_mode(RM) when is_atom(RM) ->
    insert_to_fbtab("restricted_mode",atom_to_list(RM)).

set_file_browser_docroot(no_path) ->
    set_file_browser_docroot("no_path");

set_file_browser_docroot("no_path")->
    Root=calculate_file_browser_serverroot(),
    set_file_browser_docroot(Root, no_path);

set_file_browser_docroot(BrowserRoot) when is_atom(BrowserRoot) ->
    set_file_browser_docroot([atom_to_list(BrowserRoot)], path);

set_file_browser_docroot(BrowserRoot=[C|_]) when is_integer(C) ->
    set_file_browser_docroot([BrowserRoot],path);

set_file_browser_docroot(BrowserRoot=[C|_]) when is_list(C) ->
    set_file_browser_docroot(BrowserRoot,path);

set_file_browser_docroot(_)->
    throw(?RefErr0r(
          "Bad argument given at startup~n Bad parameter: broswer root~n")).

set_file_browser_docroot(BrowserRoot=[C|_], path) when is_integer(C)->
    set_file_browser_docroot([BrowserRoot], path);

set_file_browser_docroot(BrowserRoot=[C|_], path) when is_list(C)->
    insert_to_fbtab("server_docroot",BrowserRoot),
    insert_to_fbtab("db_docroot",calculate_file_browser_dbroot());

set_file_browser_docroot(_, path)  ->
    throw(?RefErr0r(
          "Bad argument given at startup~n Bad parameter: broswer root~n"));

set_file_browser_docroot(BrowserRoot, no_path)->
    insert_to_fbtab("server_docroot",BrowserRoot),
    insert_to_fbtab("db_docroot",BrowserRoot).

calculate_file_browser_serverroot()->
    calculate_file_browser_dbroot().

calculate_file_browser_dbroot()->
    %Gets filelist from UI router
    {result, FileList}=nitrogen_helper:execute_system_query("files",
                                         {query_display_opt,
                                          [{positions, scalar}],
                                          needed_pattern,$:}),
    %Gets top level directories
    RootDir=calculate_root(lists:map(fun(File)->
                                            lists:sublist(File,length(File)-1) 
                                    end,FileList)),
    
    case RootDir of
        [] -> case file:get_cwd() of
                  {ok,Dir} -> [Dir];
                  {error, Error}->
                      throw(?RefErr0r("Error during init: " ++ 
                                          io_lib:format("~p", [Error])))
              end;
        _  -> RootDir
    end.

calculate_root(FileList=[C|_]) when is_list(C)->
    lists:usort(lists:map(fun(E) -> filename:dirname(E) end, FileList));

calculate_root([])->[].


make_ui_request(UIArgs)->
    make_ui_request(UIArgs, []).

make_ui_request(UIArgs, Args)->
    %make request to UI router
    ReqID = ?UI:getid(),
    User = proplists:get_value(user, Args, nobody),
    QStr = proplists:get_value(querystr, Args, ""),
    case User of
        nobody -> ok;
        _ -> insert_to_rqtab(ReqID, User, no_id, QStr)
    end,
    ok = ?UI:request(ReqID,UIArgs),
    ui_loop(ReqID, Args).

ui_loop(ReqID, Args)->
    receive
        {ReqID, reply, R} -> delete_from_rqtab({ReqID, '_','_','_'}),
                             R;        
        {ReqID, query_id, QueryId}-> Result=find_in_rqtab_by_reqid(ReqID),
									 case Result of
										[{ReqID, User, _, QStr}]->insert_to_rqtab(ReqID, User, QueryId, QStr);
									    _ -> ok
                                     end,
                                     ui_loop(ReqID, Args);       
        {ReqID, progress, {add, _File, _, _Max}} ->
            ui_loop(ReqID, Args);
        {ReqID, progress, {drop, _File, _, _Max}} ->
            ui_loop(ReqID, Args);
        {ReqID, progress, {add, File, Percent, FormCount, FormMax, KBps}} ->
            remote_print({File, Percent, FormCount, FormMax, KBps}, Args),
            ui_loop(ReqID, Args);
        {ReqID, progress, {drop, File, _Percent, FormCount, FormMax, KBps}} ->
            remote_print({File, FormCount/FormMax, FormCount, FormMax, KBps},Args),
            ui_loop(ReqID, Args)
    end.


remote_print({File, Percent, FormCount, FormMax, KBps}, Args)->
    KBpsTxt=case KBps of
        0.0 -> "";
        0 -> "";
        _ -> ?MISC:format("~5.2f kB/s", [(0.0 + KBps)])
    end,
    Data=io_lib:format("File: ~s (~p / ~p) ~s", [File, FormCount, FormMax, KBpsTxt]),
    remote_print({lists:flatten(Data), Percent*100}, Args);

%remote_print(Data=[C|_], Args) when is_integer(C)->
%    RemotePrinterFun=proplists:get_value(remote_printer_fun, Args, fun(_)->ok end),
%    spawn(fun()->RemotePrinterFun(Data) end),
%    ok;

remote_print(Data, Args)->
    RemotePrinterFun=proplists:get_value(remote_printer_fun, Args, fun(_)->ok end),
    spawn(fun()->RemotePrinterFun(Data) end),
    ok.

get_base()->
    Path0 = filename:split(filename:dirname(code:which(nitrogen_helper))),
    Path =filename:join(lists:sublist(Path0,length(Path0)-1)),
    Path.

%% @doc Return the filter function,
%% which need to remove whitespaces from queries.
query_filter_fun()->
fun(E)->
        if 
            E == 10 -> false;
            E == 9  -> false;
            E == 32 -> false;
            true -> true
        end
end.

%% @doc Replaces group of whitespace in the given query with 1 blank char.
filtered_query(Query=[C|_]) when is_integer(C) ->
    {ok, Mp}=re:compile("(\s|\t|\n)+",[]),
    re:replace(Query,Mp," ",[{return, list}, global]).


load_needed() ->
    BasePath=get_base(),
    %Needed to start Nitrogen
    case code:add_path(filename:join([BasePath,?NAPPS,"nprocreg/ebin"])) of        
        true -> ok;
        {error,_} -> throw(?RefError(file_notdir,
                                     [filename:join([BasePath,
                                                     ?NAPPS,
                                                     "nprocreg/ebin"])]))
    end,
    case code:add_path(filename:join([BasePath,?NAPPS,"simple_bridge/ebin"])) of
        true -> ok;
        {error,_} -> throw(?RefError(file_notdir,
                                     [filename:join([BasePath,
                                                    ?NAPPS,
                                                    "simple_bridge/ebin"])]))
    end,
    case code:add_path(filename:join([BasePath,?NAPPS,"nitrogen/ebin"])) of
        true -> ok;
        {error,_} -> throw(?RefError(file_notdir,
                                    [filename:join([BasePath,
                                                    ?NAPPS,
                                                    "nitrogen/ebin"])]))
    end,
    case code:add_path(filename:join([BasePath,?NAPPS,"simple_bridge/include"])) of
        true -> ok;
        {error,_} -> throw(?RefError(file_notdir,
                                     [filename:join([BasePath,
                                                    ?NAPPS,
                                                    "simple_bridge/include"])]))
    end,
    case code:add_path(filename:join([BasePath,?NAPPS,"nitrogen/include"])) of
        true -> ok;
        {error,_} -> throw(?RefError(file_notdir,
                                    [filename:join([BasePath,
                                                    ?NAPPS,
                                                    "nitrogen/include"])]))
    end,
    case code:ensure_loaded(nprocreg) of
        {error,_} -> throw(?RefErr0r(nprocreg_not_loaded));
        _ -> ok
    end,
    
    %Nitrogen framework use this low-level module to load 
    %the requested page's main modul in some circumstances, 
    %so we should add the page's ebin folder
    {ok,LoadPath}=erl_prim_loader:get_path(),
    erl_prim_loader:set_path(LoadPath++[filename:join([BasePath,?NSITE,"ebin"])]),
    ok.

make_proplist(YPath, YName, YPort, YListen) ->
    try
    YawsPathProp = {yaws_path,YPath},
    YawsNameProp = {yaws_name,YName},
    YawsPortProp = {yaws_port, web_helper:convert_list_to_integer(YPort)},
    YawsListenProp = {yaws_listen, web_helper:string_to_ip(YListen)},
    YawsIndexFunModuleProp = {yaws_index_fun_module, nitrogen_helper},
    YawsIndexFunNameProp = {yaws_index_fun_name, get_nitrogen_index},
    YawsPostSize = {yaws_partial_post_size, 1024*1024},
    YawsAppMod = {yaws_app_mod,   [{"/", nitrogen_yaws}] },
    NitrogenProp = {nitrogen_prop,with_nitrogen},
    AllProp = [YawsNameProp,YawsPortProp,YawsListenProp,YawsIndexFunModuleProp,
               YawsIndexFunNameProp,YawsAppMod,NitrogenProp,YawsPostSize],
    case YPath of
        "no_path" -> AllProp;
        _ -> [YawsPathProp|AllProp]
    end
    catch
        RefErr ->
            io:format(?Error:error_text(RefErr) ++ "~n"),
            exit("Cannot start nitrogen.")
    end.


%% @doc Administrate query_table_v5: if User has not owned the query,
%% add User to owner list.
update_tab_if_needed(SafeQuery,Query,User,Alias,Result,Hash)->
    {Users, Comment} = case find_in_qtab(SafeQuery) of
                [] -> {[User], ""};
                [{_SQ,_Q,_A,C,_R,UL,_H}] ->{lists:usort([User|UL]), C}
    end,
    insert_to_qtab(SafeQuery,Query,Alias,Comment,Result,Users,Hash).

%% @doc Returns with the result of the given query
calculate_result(Q,DisplayOpt)->
    calculate_result(Q,DisplayOpt, []).

calculate_result(Q,DisplayOpt, StartOpt) when is_list(StartOpt)->
    calculate_result(Q,DisplayOpt, StartOpt, []).
calculate_result(Q,DisplayOpt, StartOpt, Options) 
  when is_list(StartOpt) andalso is_list(Options)->
    SendBackQueryId=proplists:get_value(save_query_id, Options, false),
    User=proplists:get_value(user, Options, nobody),
    Req = {transform,semantic_query,
           [{ask_missing,false},
            {send_back_query_id, SendBackQueryId},
            {querystr,Q},
            {display_opt,DisplayOpt},
            {start_opt,StartOpt}]},
    case make_ui_request(Req, [{user,User},{querystr,Q}]) of
        {ok, Result} -> Result;
        M -> M
    end.

get_database_hash() ->
    HashList = lists:filter(fun(X) -> X /= virtual end,
                            [(refcore_graph:data(Node))#form.hash
                            || Node <- refcore_graph:path(
                                 refcore_graph:root(), [file, form])]),
    erlang:phash2(HashList).

error_handler(warning) ->
    "Warning: the database contains file(s) with error(s).";

error_handler({warning, Errors}) ->
    {"Warning: the database contains file(s) with error(s).", Errors};

error_handler(E) ->
    case E of
        {abort, M} -> reflib_error:error_text(M);
        {error, M} -> "Fatal error: " ++ io_lib:format("~p",[M])
    end.

is_error_in_database() ->
    case ?Query:exec(?Query:seq([file],?File:error_forms())) of
        [] -> false;
        _ -> true
    end.

get_error_forms_in_database() ->
    case ?Query:exec(?Query:seq([file],?File:error_forms())) of
        [] -> [];
        ErrorForms -> get_errors(ErrorForms)
    end.

get_errors([])->[];

get_errors(Forms) when is_list(Forms)->
    ErrorFiles=?Query:exec(Forms,[{form, back}]),
    lists:foldl(fun({File, Form}, Acc)->
                       {Index, First}=case ?Syn:index(File, form, Form) of
                                 1 -> {1,true};
                                 Int -> {Int-1,false}
                             end,
                       {StartPos,Length} = case First of
                            true->[Token]=?Query:exec(Form, [{flex, last}]),
                                     case ?Token:pos(Token) of
                                         {EPos,_} ->Len=(?ESG:data(Form))#form.length,
                                                    {EPos-Len,Len};
                                         not_found -> {not_found, 0}
                                     end;
                            false->PrevForm=?Query:exec(File, ?File:form(Index)),
                                      [Token]=?Query:exec(PrevForm, [{flex, last}]),
                                      case ?Token:pos(Token) of
                                          {SPos, _} ->Len=(?ESG:data(Form))#form.length
                                                          +?Token:len(?Token:data(Token)),
                                                      {SPos+1,Len-1}; %% 2 ????
                                          not_found -> {not_found, 0}
                                      end
                            end,
                       Tag=(?Graph:data(Form))#form.tag,
                        case {ErrorMessage=make_error_message(Tag), StartPos} of
                            {"", _} -> Acc;
                            {_, not_found} -> Acc;
                            _ -> Acc++[{?File:path(File), 
                                        StartPos, 
                                        Length,
                                        ErrorMessage}]
                        end
                end, [], lists:zip(ErrorFiles, Forms));

get_errors(_)->[].

make_error_message(Tag)->
    try
    case Tag of
        {1,Mess} -> Mess;
        {no_include_file, Name} -> io_lib:format("Include file not found: ~p",
                                                 [Name]);
        {include_lib, application_not_specified, Name} ->
            io_lib:format("Application not specified -include lib: ~p", [Name]);
        {unknown_env_in_include, [Name]}->io_lib:format("Unknown env in include: ~p",
                                                        [Name]);
        {include_error, IncName, Reason} ->
            io_lib:format("Include error in ~p. Reason: ~p",[IncName,Reason]);
        _ -> ""
    end
    catch
        _Type : _Error ->
            ""
    end.

is_database_changed(Hash1) ->
    Hash1 /= get_database_hash().

list_to_term(String) ->
    {ok, T, _} = erl_scan:string(String++"."),
    case erl_parse:parse_term(T) of
        {ok, Term} -> Term;
        {error, Error} -> Error
    end.

%%% ============================================================================
%%% Low- level operations on 'skeleton_table' dets table
insert_to_stab(Name,Body, Owner, ParamCardinality, Comment) ->
    dets:open_file(?S_TAB,[]),
    dets:insert(?S_TAB,{Name, Body, Owner, ParamCardinality, Comment}),
    dets:close(?S_TAB).

find_in_stab_by_name(Name) ->
    dets:open_file(?S_TAB,[]),
    Result=dets:match_object(?S_TAB,{Name,'_','_','_','_'}),
    dets:close(?S_TAB),
    Result.

get_stab_elements()->
    dets:open_file(?S_TAB,[]),
    Result=dets:match_object(?S_TAB,{'_','_','_','_','_'}),
    dets:close(?S_TAB),
    Result.
    
delete_from_stab(RowPattern) ->
    dets:open_file(?S_TAB,[]),
    dets:match_delete(?S_TAB,RowPattern),
    dets:close(?S_TAB).

%%% ============================================================================
%%% Low- level operations on 'running_queries_table' dets table
init_rqtab()->
    dets:open_file(?RQ_TAB,[]),
    dets:delete_all_objects(?RQ_TAB),
    dets:close(?RQ_TAB).

insert_to_rqtab(ReqID,User, QueryId, QStr) ->
    dets:open_file(?RQ_TAB,[]),
    dets:insert(?RQ_TAB,{ReqID,User,QueryId, QStr}),
    dets:close(?RQ_TAB).

find_in_rqtab_by_reqid(ReqID) ->
    dets:open_file(?RQ_TAB,[]),
    Result=dets:match_object(?RQ_TAB,{ReqID,'_','_','_'}),
    dets:close(?RQ_TAB),
    Result.

find_in_rqtab_by_user(User) ->
    dets:open_file(?RQ_TAB,[]),
    Result=dets:match_object(?RQ_TAB,{'_',User,'_','_'}),
    dets:close(?RQ_TAB),
    Result.

get_rqtab_elements()->
    dets:open_file(?RQ_TAB,[]),
    Result=dets:match_object(?RQ_TAB,{'_','_','_','_'}),
    dets:close(?RQ_TAB),
    Result.
    
delete_from_rqtab(RowPattern) ->
    dets:open_file(?RQ_TAB,[]),
    dets:match_delete(?RQ_TAB,RowPattern),
    dets:close(?RQ_TAB).

%%% ============================================================================
%%% Low- level operations on 'file_browser_table' dets table

insert_to_fbtab(Key,Value) ->
    dets:open_file(?FB_TAB,[]),
    dets:insert(?FB_TAB,{Key,Value}),
    dets:close(?FB_TAB).

find_in_fbtab(Key) ->
    dets:open_file(?FB_TAB,[]),
    Result=dets:match_object(?FB_TAB,{Key,'_'}),
    dets:close(?FB_TAB),
    Result.

init_fbtab()->
    dets:open_file(?FB_TAB,[]),
    dets:delete_all_objects(?FB_TAB),
    dets:close(?FB_TAB).

%%% ============================================================================
%%% Low- level operations on 'query_table_v5' dets table
insert_to_qtab(SafeQuery=[C|_],Query, Alias, Comment, Result, User, Hash) when is_integer(C)->
    dets:open_file(?TAB,[]),
    insert_to_qtab({SafeQuery, undefined, undefined}, Query, Alias, Comment, Result, User, Hash),
    dets:close(?TAB);

insert_to_qtab(SafeQuery={_Query, _File, _Pos},Query, Alias, Comment, Result, User, Hash) ->
    dets:open_file(?TAB,[]),
    dets:insert(?TAB, {SafeQuery, Query, Alias, Comment, Result, User, Hash}),
    dets:close(?TAB).

find_in_qtab(SafeQuery) ->
    dets:open_file(?TAB,[]),
    Result=dets:lookup(?TAB,SafeQuery),
    dets:close(?TAB),
    Result.

find_in_qtab_by_pattern(Pattern)->
    dets:open_file(?TAB,[]),
    Result=dets:match_object(?TAB,Pattern),
    dets:close(?TAB),
    Result.

delete_from_qtab(SafeQuery) ->
    dets:open_file(?TAB,[]),
    dets:delete(?TAB,SafeQuery),
    dets:close(?TAB).

