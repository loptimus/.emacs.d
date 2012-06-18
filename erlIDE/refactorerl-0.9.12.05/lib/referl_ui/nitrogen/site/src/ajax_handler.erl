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
%% -*- mode: nitrogen -*-
-module (ajax_handler).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-vsn("$Rev: 8315 $ ").


main() ->
	case wf:role(auth_users) of
        true ->
			#template { file=nitrogen_helper:get_nitrogen_site_dir()++
								 "/templates/ajax_handler.html" };
        false ->
    		wf:header("WWW-Authenticate", "HTTP/1.1 403 Forbidden"),
    		wf:status_code(403),
    		"<strong>Access Denied!</strong>"
    end.

get_content()->
	case wf:q(filter) of
		undefined ->
	case wf:q(dir) of
		undefined->"";
		"__multi_dirs__"->
			case wf:q(mod) of
				undefined -> "";
				"db"->TopLevelDirs=
							getStartingFolders(nitrogen_helper:get_file_browser_loaded_files_root()),
					  LiList=get_li_list("",TopLevelDirs),
					  "<ul class='jqueryFileTree' style='display: none;'>"++
						  LiList++
						  "</ul>";
				"server"->TopLevelDirs=
							  getStartingFolders(nitrogen_helper:get_file_browser_server_root()),
						  LiList=get_li_list("",TopLevelDirs),
						  "<ul class='jqueryFileTree' style='display: none;'>"++
							  LiList++
							  "</ul>"
			end;
		 RootDirUrlEncoded->RootDir=wf_convert:url_decode(RootDirUrlEncoded),
                   case wf:q(mod) of 
					 "db"->LiList=
							get_li_list(RootDir,
										getStartingFolders(get_loaded_files_list(
										  RootDir),noslash)),
						   "<ul class='jqueryFileTree' style='display: none;'>"++
							   LiList++
							   "</ul>";
					 _ ->case listdir_sorted(RootDir) of
							 {ok,FileNames}->
								 LiList=get_li_list(RootDir,FileNames),
								 "<ul class='jqueryFileTree' style='display: none;'>"++
									 LiList++
									 "</ul>";
							{error, _}->""
				 		 end
				 end
	end;
		Filter ->
			case wf:q(mod) of
				undefined -> "";
				"db"->TopLevelDirs=
						  nitrogen_helper:get_file_browser_loaded_files_root(),
					  LiList=get_filtered_list("",Filter,TopLevelDirs,db),
					  "<ul class='jqueryFileTree' style='display: none;'>"++
						  LiList++
						  "</ul>";
				"server"->TopLevelDirs=
							  nitrogen_helper:get_file_browser_server_root(),
						  LiList=get_filtered_list("",Filter,TopLevelDirs,server),
						  "<ul class='jqueryFileTree' style='display: none;'>"++
							  LiList++
							  "</ul>"
			end
	end.

getStartingFolders(Folders,noslash) ->
	GroupedFolders=groupFiles(lists:map(fun(F) -> {F,hd(string:tokens(F, "/"))} end,Folders)),
	lists:map(fun([F|FS]) -> getLargestPrefix(F,F,FS) end,GroupedFolders).
getStartingFolders(Folders) ->
	GroupedFolders=groupFiles(lists:map(fun(F) -> {F,hd(string:tokens(F, "/"))} end,Folders)),
	lists:map(fun([F|FS]) -> getLargestPrefix("/"++F,"/"++F,FS) end,GroupedFolders).

getLargestPrefix(_,LastPrefix,[]) -> LastPrefix;
getLargestPrefix(Prefix,LastPrefix,Folders) ->
	GoodPrefix=lists:all(fun(F) -> lists:prefix(Prefix, F) end,Folders),
	if
		GoodPrefix==true -> 
			First=hd(Folders),
			NextPrefix=Prefix++"/"++myhd(string:tokens(lists:sublist(First,length(Prefix)+1,length(First)),"/")),
			getLargestPrefix(NextPrefix,Prefix,Folders);
		GoodPrefix==false -> LastPrefix
	end.

myhd([]) -> [];
myhd([E|_]) -> E.

groupFiles([]) -> [];
groupFiles([{Data,File}|ES]) ->
	groupFiles(ES,[File,Data],[],File).

groupFiles([],InnerSum,Sum,_) -> Sum++[InnerSum];
groupFiles([{Data,File}|ES],InnerSum,Sum,LastFile) when File==LastFile ->
	groupFiles(ES,InnerSum++[Data],Sum,File);
groupFiles([{Data,File}|ES],InnerSum,Sum,_) ->
	groupFiles(ES,[File,Data],Sum++[InnerSum],File).

get_loaded_files_list(Parent)->
    {result,FileList}=nitrogen_helper:execute_system_query("files",
                                         {query_display_opt,
                                          [{positions, scalar}],
                                          needed_pattern,$:}),
	if
		FileList==[] -> [];
		true ->
			Files=lists:map(fun(F)->lists:sublist(F,length(F)-1) end,FileList),
			Folders=lists:usort(lists:map(fun(File)->
						            case lists:prefix(Parent, File) of
						                true -> FolderSplit=string:tokens(lists:sublist(File,length(Parent)+1,length(File)), "/"),
												ConcatedFolder=lists:map(fun(X)-> X++"/" end, FolderSplit),
												if
													length(ConcatedFolder)==1 -> hd(ConcatedFolder);
													true -> lists:flatten(lists:reverse(lists:nthtail(1,lists:reverse(ConcatedFolder))))
												end;
						                false -> []
						            end 
				        		end, Files)),
			
			lists:map(fun(F)->lists:sublist(F,length(F)-1) end,[F || F <- Folders, F/=[]])
	end.

receive_ajax()->
	%if the request was different from 'selected_file', do nothing.
	case wf:q(dir) of
		undefined ->wf:session(selected_file,wf:q(selected_file)),
					"";
		_ -> ""
	end,
	case wf:q(selected_position) of
		undefined -> "";
		_ -> wf:session(selected_position, wf:q(selected_position)),
			 ""
	end.

get_li_list(RootDir,FileNames)->
	lists:foldr(fun(X, List) -> 
					Elem=case file:read_file_info(filename:join(RootDir,X)) of
							{ok, #file_info{type=directory}} -> 
								"<li class='directory collapsed'><a href='#' rel='"++
									filename:join(RootDir,X)++
									"/'>"++
									X++
									"</a></li>";
							{_, _} -> 
								R=filename:extension(X),
								Ext=if 
										hd(R) == 46 -> lists:nthtail(1,R);
									    true -> X
									end,
								OnlyErls=wf:q(checked)=="true",
								if
									(R==".erl") or (R==".hrl") or not (OnlyErls) ->
										"<li class='file ext_"++ 
										Ext ++
										"'><a href='#' rel='"++
										filename:join(RootDir,X)++
										"'>"++
										lists:last(string:tokens (X, "/"))++
										"</a></li>";
									true -> ""
								end
					end,
					Elem ++ List
				end, [], FileNames).

get_filtered_list(RootDir,Filter,FileNames,ListType)-> 
	lists:foldr(fun(X, List) -> 
					Elem=case file:read_file_info(filename:join(RootDir,X)) of
							{ok, #file_info{type=directory}} ->
								if
									ListType==db -> 
										get_filtered_list(X,Filter,nitrogen_helper:get_loaded_files_list(X),ListType);
									true ->
										{ok,Files}=listdir_sorted(filename:join(RootDir,X)),
										get_filtered_list(X,Filter,Files,ListType)
								end;
							{_, _} ->
								R=filename:extension(X),
								Matches=filter(Filter,lists:last(string:tokens (X, "/"))),
								OnlyErls=wf:q(checked)=="true",
								if
									(Matches) and ((R==".erl") or (R==".hrl") or not (OnlyErls)) -> 
										Ext=if 
												hd(R) == 46 -> lists:nthtail(1,R);
												true -> X
											end,
										"<li class='file ext_"++ 
											Ext ++
											"'><a href='#' rel='"++
											filename:join(RootDir,X)++
											"'>"++
											lists:last(string:tokens (X, "/"))++
											%filename:join(RootDir,X)++
											"</a></li>";
									true -> ""
								end
					end,
					Elem ++ List
				end, [], FileNames).


filter(Filter,File) ->
	Regexp=lists:flatten(lists:map(fun(X) -> 
	if 
		X==$* -> ".*";
		X==$. -> "\\.";
		X==$? -> ".";
		true -> X 
	end end,Filter)),
	try
	case re:run(File, Regexp, []) of
		{match,[{S,F}]} when (S==0) and (F==length(File)) -> true;
		_ -> false
	end
	catch
	_ -> false
	end.

listdir_sorted(RootDir) ->
	{ok,FileNames}=file:list_dir(RootDir),
	F1=mysort(lists:filter(
		fun(X) -> 
			L=file:read_file_info(filename:join(RootDir,X)),
			case L of {ok, #file_info{type=directory}} -> true; _ -> false end
		end,FileNames)),
	F2=mysort(lists:filter(
		fun(X) -> 
			L=file:read_file_info(filename:join(RootDir,X)),
			case L of {ok, #file_info{type=regular}} -> true; _ -> false end
		end,FileNames)),
	F3=mysort(lists:filter(
		fun(X) -> 
			L=file:read_file_info(filename:join(RootDir,X)),
			case L of {error, _} -> true; _ -> false end
		end,FileNames)),
	{ok,F1++F2++F3}.

mysort([]) ->
    [];
mysort([H | T]) -> 
    mysort([ X || X <- T, string:to_lower(X) < string:to_lower(H) ]) ++ [H] ++ mysort([ X || X <- T, string:to_lower(X) >= string:to_lower(H) ]).
