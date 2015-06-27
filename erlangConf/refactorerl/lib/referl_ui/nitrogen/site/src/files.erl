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

-module (files).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 8315 $ ").
-define(PRINTER_ID, nitrogen_printer).

%%% ============================================================================
%%% HTML output

main() ->
	start_progress_printer(),
	nitrogen_lib:main("files.html").

title() -> "RefactorErl Queries".

logout()->
	nitrogen_lib:logout_link(wf:user()).

get_server_root_dir()->
	TL=nitrogen_helper:get_file_browser_server_root(),
	if 
		length(TL) == 1 -> TL;
		true -> "__multi_dirs__"
	end.

get_db_root_dir()->
	TL=nitrogen_helper:get_file_browser_loaded_files_root(),
	if 
		length(TL) == 1 -> TL;
		true -> "__multi_dirs__"
	end.

env_nodes()->
	case wf:role(admin) of
		true -> Envs=nitrogen_helper:get_appbases(),
				#panel{id=envs_placeholder, body=envs_handler(Envs)};
		false -> "You don't have permission to access this page."
	end.

show_file_section()->
	#panel{id=show_sorce_panel, body=""}.

browser_type_select()->
	case wf:role(admin) of
	true ->
	Options=case wf:session(browser_type_dropdown) of
				undefined ->[#option{ text="Browse server", 
									  value="server", 
									  selected=true },
							 #option{ text="Browse loaded files", 
									  value="db", 
									  selected=false }];
				"server"  ->[#option{ text="Browse server", 
									  value="server", 
									  selected=true },
							 #option{ text="Browse loaded files", 
									  value="db", 
									  selected=false }];
				"db"      ->[#option{ text="Browse server", 
									  value="server", 
									  selected=false },
							 #option{ text="Browse loaded files", 
									  value="db", 
									  selected=true }]
			end,
	wf:set(browser_type_dropdown,"server"),
	InnerCell=[#panel{id=buttons_placeholder,
					  body=[#link{id=show_file_button, 
								  body=[#image{image="/images/show_source.png", 
											   alt="Show source", 
											   class=no_decoration}], 
								  postback=show_file,
								  title = "Show source",
								  class=no_decoration},
							#link{id=add_file_button, 
								  body=[#image{image="/images/database_add.png", 
											   alt="Add file", 
											   class=no_decoration}], 
								  postback={add_file,add},
								  title = "Add file to database",
								  class=no_decoration, 
								  actions=get_progress_event(
											"Adding file(s) to database ..")},
							#link{id=sync_file_button, 
								  body=[#image{image="/images/reload.png", 
											   alt="Reload", 
											   class=no_decoration}], 
								  postback={add_file,sync},
								  title = "Reload file to database",
								  class=no_decoration,
								  actions=get_progress_event(
											"Reloading file(s) to database ..")},
							#link{id=delete_file_button, 
								  body=[#image{image=
												  "/images/database_remove.png", 
											   alt="Drop file", 
											   class=no_decoration}], 
								  postback=delete_file, 
								  title = "Drop file from database",
								  class=no_decoration},
							#link{id=set_env_button, 
								  body=[#image{image="/images/set_env.png", 
											   alt="Add as an appbase node", 
											   class=no_decoration}], 
								  postback=add_env,
								  title = "Add as an appbase node",
								  class=no_decoration},
							#link{id=generate_button, 
								  body=[#image{image="/images/show_links.png", 
											   alt="Generate html", 
											   class=no_decoration}], 
								  postback=generate,
								  title = "Generate html",
								  class=no_decoration},#panel{id=delete_confirm}]}],
	Body=#table{rows=[#tablerow{
							cells=[#tablecell{body=[
												#dropdown{id=browser_type_dropdown,
														  options=Options,
														  html_encode=true,
														  postback=load_file_browser}]},
								   #tablecell{body=InnerCell}]}]},
	Body;
	false -> #br{}
	end.

message_panel()->
	MessageBox=[#panel { class=[flash_content, centered],
				        id=progress_message_panel,
					    body=""}, 
			   #panel { class=[flash_content, centered],
				        id=progress_panel,
					    body="<div id='progressbar'></div>" }],
	StatusRow = #tablerow{cells=[#tablecell{body=[#panel { class=[flash_content, 
																	centered, bold],
														  id=wait_message_panel, 
														  body="" }
												  ]++MessageBox}]},
	
	InnerPanel = #panel { id=wait_message_flash_inner, 
						  class=flash, 
						  actions=#show { target=wait_message_flash, 
										  effect=blind, 
										  speed=10 }, 
						  body=[#link { class=flash_close_button, 
										text="Close", 
										actions=#event { type=click, 
														 target=wait_message_flash, 
														 actions=
															 #hide{effect=blind,
																   speed=400 }}},
								#table{rows=[StatusRow]}]},
    WaitPanel=#panel { id=wait_message_flash, 
					   style="display:none;", 
					   body=InnerPanel},
	wf:wire(wait_message_flash_inner, #hide{}),
	wf:wire(wait_message_flash, #hide{}),
	
	case wf:session(flash_message) of
		undefined -> ok;
		Message   -> wf:session(flash_message,undefined),
					 wf:flash(Message)
	end,
	[WaitPanel,#panel{id=message_panel,body=[#flash{}]}].

file_browser()->
	case wf:role(admin) of
	true ->	
	Body=[#panel{id=file_browser_placeholder,
		   body=[#checkbox{id=ext_checkbox,text="Show only .erl and .hrl files",class="ext_checkbox"},
				"<br />Filter: ",#textbox{id=file_filter,text="",class="file_filter"},
				#panel{id=file_browser_db_panel,
						body="<div id='file_browser_db'></div>"},
				#panel{id=file_browser_server_panel,
						body="<div id='file_browser_server'></div>"}]}],
	case wf:session(browser_type_dropdown) of
		undefined -> showPanel("server");
		Panel -> showPanel(Panel)
	end,
	Body;
	false -> #br{}
	end.

%%% ============================================================================
%%% Handlers for postback events	

continue({continue, generate}, {Files,Fun}) ->
	Count=length(Files),
	CurCount=wf:session(gencount),
	wf:session(gencount,wf:session(gencount)+1),
	if
		CurCount==Count ->
			wf:wire(wait_message_flash_inner, #hide{}),
			wf:wire(wait_message_flash, #hide{}),
			wf:wire(wait_message_flash, #hide{effect=blind, speed=400}),
			wf:flash(wf:session(selected_file)++" successfully generated.");
		true ->
			Fun({"Generating: "++lists:nth(CurCount+1,Files)++" ("++integer_to_list(CurCount)++"/"++integer_to_list(Count)++")",round(wf:session(gencount)/Count*100)})
	end.

count(File,List) ->
	case lists:suffix("/", File) of
		true -> 
			lists:foldr(fun(X,S) -> X++S end,[],lists:map(fun(X) -> count(X,List) end,nitrogen_helper:get_loaded_files_list(File)));
		false ->
			[File]
	end.

event(generate)->
	File=wf:session(selected_file),
	case File of
		undefined -> 
			wf:wire(#alert{text="Please, select a file first!"});
		_ ->
			Files=count(File,[]),
			wf:wire(wait_message_flash_inner, #show{}),
			wf:wire(wait_message_flash, #show{}),
			PFun=start_progress_printer(),
			wf:session(gencount,1),
			PFun({"Generating: "++hd(Files)++" (1/"++integer_to_list(length(Files))++")",round(wf:session(gencount)/length(Files)*100)}),
			lists:map(fun(X) ->
				Fun=fun() -> referl_htmlserver:generate_call(X),{Files,PFun} end,
				wf:continue({continue, generate}, Fun, 60*60*1000) end,Files)
	end;	

event(delete_file)->
	wf:replace(delete_confirm,#table{id=delete_confirm,rows=[#tablerow{cells=#tablecell{colspan=2,body="Are you sure?"}},#tablerow{cells=[#tablecell{body=#button{text="Yes", postback=delete_confirm,actions=get_progress_event("Dropping file(s) from database ..")}},#tablecell{body=#button{text="No", postback=delete_cancel}}]}]});

event(delete_cancel)->
	wf:replace(delete_confirm,#panel{id=delete_confirm});

event(delete_confirm) ->
	File=wf:session(selected_file),
	case File of
		undefined -> wf:wire(#alert{text="Please, select a file first!"});
		_ ->wf:wire(wait_message_flash, #hide{effect=blind, speed=400}),
			case lists:suffix("/", File) of
				true -> 
					lists:map(fun(X)-> wf:session(selected_file,X),event(delete_confirm) end,nitrogen_helper:get_loaded_files_list(File)),
					wf:wire(wait_message_flash, #hide{effect=blind, speed=400}),
					need_to_update(
										 "Successfully dropped the folder "++
											 File++
											 " from database.");
				false ->
					PrinterFun=start_progress_printer(),
					case nitrogen_helper:drop_from_db(File, PrinterFun) of
						 {result, _}-> need_to_update(
										 "Successfully dropped "++
											 File++
											 " from database.");
						 {error, Error} ->wf:flash(Error)
					 end
			end
	end;

event({add_file,Event})->
	File=wf:session(selected_file),
	case File of
		undefined -> wf:wire(#alert{text="Please, select a file first!"});
		_ ->wf:wire(wait_message_flash, #hide{effect=blind, speed=400}),
			PrinterFun=start_progress_printer(),
			case nitrogen_helper:add_to_db(File, PrinterFun) of
				 {result, _}->	
					 case Event of
						 add -> need_to_update("Successfully added "++
												   File++
												   " to database.");
						 sync ->wf:flash("Successfully reloaded "++
											 File++
											 " to database.")
					 end;
				{error, Error} ->need_to_update(Error)
			 end
	end;

event(show_file)->
	File=wf:session(selected_file),
	case File of
		undefined -> wf:wire(#alert{text="Please, select file first!"});
		_ ->case nitrogen_helper:get_source(File) of
			{data, Data} ->
				wf:replace(show_sorce_panel,
						   #panel{id=show_sorce_panel,
								  body=[#p{body=File, class="query_title"},
										#textarea{id=source_code_textarea,
												  text=Data,
												  html_encode=true,
												  class="source_code_ta"}]}),
				wf:wire(#resize_textarea { targetId=source_code_textarea, 
										   maxWidth=80});
			{error, Error} ->
				wf:replace(show_sorce_panel,#panel{id=show_sorce_panel,
												   body=[#p{id=error_in_load,
															body=Error,
															class="error"}]})
			end
	end;
	
event(load_file_browser)->
	showPanel(wf:q(browser_type_dropdown));

event(show_envs)->
	HideEnvsLink=#link{id=hide_envs_link,
					   text="Hide appbase nodes", 
					   postback=hide_envs, 
					   class="env_link"},
	wf:replace(show_envs_link,HideEnvsLink),
	wf:wire(envs_table, #appear{speed=100});

event(hide_envs)->
	ShowEnvsLink=#link{id=show_envs_link, 
					   text="List all of the appbase nodes", 
					   postback=show_envs, 
					   class="env_link"},
	wf:replace(hide_envs_link,ShowEnvsLink),
	wf:wire(envs_table, #fade{speed=100});

event({del_env, Path})->
	case nitrogen_helper:del_appbase(Path) of
		ok -> wf:flash("Successfully deleted "++
						Path++
						" from the values of the appbase node.");
		not_found -> wf:flash(Path++" is not set as an appbase node.")
	end,
	wf:replace(envs_placeholder,env_nodes());

event(add_env)->
	File=wf:session(selected_file),
	case File of
		undefined -> wf:wire(#alert{text="Please, select file first!"});
		_ -> case nitrogen_helper:add_appbase(File) of
			 	ok -> wf:flash(File++" is added as an appbase node successfully.");
				{error,Error} -> wf:flash("Error: "++Error)
			 end
	end,
	wf:replace(envs_placeholder,env_nodes());

event(logout) ->
	nitrogen_lib:logout(wf:user()).

%%% ============================================================================
%%% Hepler functions
showPanel("server")->
	wf:wire(file_browser_db_panel, #hide {}),
	wf:wire(file_browser_server_panel, #show {}),
	wf:wire(sync_file_button, #hide {}),
	wf:wire(delete_file_button, #hide {}),
	wf:wire(add_file_button, #show {}),
	wf:wire(show_file_button, #show {}),
	wf:wire(generate_button, #hide {});

showPanel("db")->
	wf:wire(file_browser_server_panel, #hide {}),
	wf:wire(file_browser_db_panel, #show {}),
	wf:wire(sync_file_button, #show {}),
	wf:wire(delete_file_button, #show {}),
	wf:wire(add_file_button, #hide {}),
	wf:wire(show_file_button, #show {}),
	wf:wire(generate_button, #show {}).

envs_handler(Envs) when is_list(Envs)->
	[#link{id=show_envs_link, 
		   text="List all of the 'appbase' environment nodes", 
		   postback=show_envs, 
		   class="env_link"},
	 #table{id=envs_table,
		   class="query_result_table",
		   rows=lists:foldl(fun(Path,Acc)->
								Cell1=#tablecell{body=#p{body=Path},
												align="left",
												valign="middle",
												class=["query_result_tablecell",
														"text_85"]},
								Link=#link {text="X",
											class="delete_query_a", 
											title = "Delete the value of the appbase node", 
											postback={del_env,
													  Path}},
								Cell2=#tablecell{body=Link,
												align="left",
												valign="middle",
												class="query_result_tablecell"},
								Acc++[#tablerow{cells=[Cell1,Cell2]}]
							end, [], Envs),
		 style="display:none;"}];

envs_handler(_)->"".

need_to_update(Message)->
	case nitrogen_helper:need_to_update() of
		false -> ok;
		true -> wf:session(flash_message,Message),
				wf:session(browser_type_dropdown,wf:q(browser_type_dropdown)),
				wf:redirect("/files")
	end.

get_progress_event(Label)->
	Event = #event { target=wait_message_panel, type=click },
	Actions=[#show{target=wait_message_flash_inner, speed=0},
			 #show{target=spinner_progress, speed=0},
			 #show {target=wait_message_flash, effect=blind, speed=10 },
			 #set{value=[Label]}],
	Event#event{actions=Actions}.

start_progress_printer()->
	{ok,Pid}=wf:comet(fun() -> progress_loop() end, ?PRINTER_ID),
	fun(Data)-> Pid!Data end.

progress_loop()->
    receive 
        {Data=[C|_], Percent} when is_integer(C)->
			wf:wire(#script{script="$('#progressbar').progressbar({ value: "
									++io_lib:format("~p",[round(Percent)])++" });"}),
			Panel=#panel { class=flash_content,
					 id=progress_message_panel,
					  body="<p>"++Data++"</p>" }  ,          
			wf:replace(progress_message_panel, Panel),
            wf:flush(),
			progress_loop();

		 _->progress_loop()
    end.
