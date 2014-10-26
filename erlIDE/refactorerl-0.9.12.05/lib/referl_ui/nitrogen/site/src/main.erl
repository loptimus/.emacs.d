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
%%% Portions created  by Eötvös Loránd University are Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @author Viktoria Fordos <v@fviktoria.hu>

%% -*- mode: nitrogen -*-

-module (main).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-include("records.hrl").
-vsn("$Rev: 8315 $ ").

-define(MAX_CHR_OF_LINE,25).

%%% ============================================================================
%%% HTML output

main() -> 
	wf:session(selectednode_data, undefined),
	wf:session(currentjump_nodes, undefined),
	wf:session(click_node,undefined),
	nitrogen_lib:main("main.html").

title() -> "RefactorErl Queries".

query_editor()->
	SaveSkelBtn=#button{id=save_skeleton_button, 
				text="Save as skeleton", 
				postback=save_skeleton_req, 
				class=[referl_button]},
	RunNewQBtn=#button{id=new_query_button, 
				text="Run", 
				postback=run_new_query, 
				class=[referl_button]},
	BtnTable=#table{rows=[#tablerow{cells=#tablecell{body=[SaveSkelBtn]}},
						  #tablerow{cells=#tablecell{body=[RunNewQBtn]}}]},
	JumpTable=#table{id=jumptable},
	OptionTable=#table{id=jumpoptions},
	DeselectButton=#table{id=deselect},
	CustomQueryList=#table{id=customqueries},
	TextBox=#textarea_autocomplete{ id=query_box, 
								 tag="", 
								 text="",
								 style="max-height:50px; min-height:50px; height:50px",
								 minLength=2, 
								 delay=300, 
								 html_encode=true, 
								 class=[width_300],
								 next=new_query_button},
	HiddenData=#hidden{id=hiddendata,text=""},
	HiddenButton=#link{id=hiddenbutton,body="",postback=node_clicked},
	Table=#table{class=[width_100],rows=[#tablerow{cells=[
		#tablecell{colspan=2, body=[#table{rows=[
			#tablerow{cells=[#tablecell{body=TextBox},
							 #tablecell{body=HiddenData},
							 #tablecell{body=HiddenButton}]}]}]},
		#tablecell{body=[BtnTable]},
		#tablecell{body=["&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"]},
		#tablecell{body=[JumpTable]},
		#tablecell{body=[],class=[width_100]},
		#tablecell{body=[OptionTable]},
	#tablerow{cells=[#tablecell{body=[DeselectButton]},#tablecell{body=[CustomQueryList]},
		     	 #tablecell{id=curfilename,colspan=7,class=[menuitem, curFileNameCont],
				   			body=["Current file: No file shown."]}]}
				]}]},
	wf:wire(new_query_button, 
			query_box, 
			#validate { validators=[#is_required { text="Required." }]}),
	wf:wire(save_skeleton_button, 
			query_box, 
			#validate { validators=[#is_required { text="Required." }]}),
	Table.

jumptable() ->
	PrevBtn=#button{id=jump_next_button, 
				text="Prev", 
				postback=jumpprev,
				class=[referl_button]},
	NextBtn=#button{id=jump_next_button, 
				text="Next", 
				postback=jumpnext,
				class=[referl_button]},
	#table{id=jumptable,rows=[
		#tablerow{cells=[
			#tablecell{body=[PrevBtn]},
			#tablecell{body=[NextBtn]}
						]},
		#tablerow{cells=[
			#tablecell{id=jumpcounter,body=["..."],
					   colspan=2,class=[menuitem]}
						]}]}.

query_result()->
	#panel{id=query_result_placeholder,body=[]}.

query_result_source_code()->
	wf:session(selected_source, undefined),
	[#panel{id=linenums,body=[],class=["lnums"]},#panel{id=query_result_source_code_placeholder,body=[]}].

executed_query()->
	#panel{id=executed_query_placeholder,body=[]}.

warnings()->
	Warning=nitrogen_helper:get_possible_warning(),
	case Warning of
		no_warning -> "";
		_ ->#panel{id=warning_messages_placeholder,body=Warning}
	end.

previous_queries()->
	Form=[#panel{id=placeholder,
				 body=[
						 #dropdown{id=query_owner,
								   options=[
											#option{ text="My queries", 
													 value="my", 
													 selected=true },
											#option{ text="All queries", 
													 value="all", 
													 selected=false }
											],
								   html_encode=true,
								   postback=load_list
								  },
						 #panel{id=queries_list_panel,
								body=[
									  #table{id=query_list,
											 class="query_list_table", 
											 rows=[]}
									  ]
								}
						 ]}],
	List=nitrogen_helper:query_list_from_qtab(wf:user()),
	LoadListFun=load_list_function(),
	lists:foreach(LoadListFun, List),
	Form.

running_queries()->
	wf:comet(fun() -> background_update() end),
	#panel{id=running_queries_placeholder,body=[]}.
	
logout()->
	nitrogen_lib:logout_link(wf:user()).

get_db_root_dir()->
	TL=nitrogen_helper:get_file_browser_loaded_files_root(),
	if 
		length(TL) == 1 -> TL;
		true -> "__multi_dirs__"
	end.

browser_type_select()->
	#table{rows=[
		#tablerow{cells=[
			#tablecell{body=["Filter: ",#textbox{id=file_filter,text="",class="file_filter"}]},
			#tablecell{body=[#link{id=show_html_button,body=" ",postback=show_file_links}]}
						]}]}.

file_browser()->	
	[#panel{id=file_browser_placeholder,
		   body=[#panel{id=file_browser_db_panel,
						body="<div id='file_browser_db'></div>"}]}].

stored_skeletons()->
	[#panel{id=skeletons_placeholder, body=skeletons_table()}].

skeleton_message_panel()->
	[#panel{id=skeleton_message_panel_placeholder, body=""}].
%%% ============================================================================
%%% autocomplete

autocomplete_enter_event(SearchTerm, _Tag) ->
	PosQueries=nitrogen_helper:do_autocomplete(SearchTerm),
	Data=lists:map(fun({Label,Value})-> 
						  {struct, [{id, list_to_binary(Value)}, 
									{label, list_to_binary(Label)} , 
									{value, list_to_binary(Value) }]} 
				   end,PosQueries),
	mochijson2:encode(Data).

autocomplete_select_event({struct,[{<<"id">>,_},{<<"value">>, Value}]},_Tag) ->
	wf:flash(Value),
	ok.

%%% ============================================================================
%%% Handlers for postback events	
run_query(Arg) ->
	nitrogen_helper:execute_query(Arg).

showResult(Node) ->
	{_,NodeT,NodeId}=Node,
	NodeText=atom_to_list(NodeT)++integer_to_list(NodeId),
	cleanUp(),
	wf:wire(#script{script="
	try{
		elem=document.getElementById('jump_"++NodeText++"');
		if(elem!=null)
		{
			window.location = location.href.replace( /(#\\w*)?$/, '#jump_"++NodeText++"');
			if(elem.tagName=='A')
			{
				$(elem).click();
			}
			else
			{
				if(lastcolored!=null) $(lastcolored).removeClass('selectednode');
				$(elem).addClass('selectednode');
				lastcolored=elem;
				jt.value='"++atom_to_list(NodeT)++"|"++integer_to_list(NodeId)++"';
				$(jb).click();
			}
		}
	}catch(ex){}"}),
	wf:continue({continue, check_if_node_exists}, fun() -> Node end, 60*60*1000).

jumpToNode(Node) ->
	wf:replace(jumptable,jumptable()),
	wf:replace(jumpcounter,
		#tablecell{id=jumpcounter,body=[
						integer_to_list(wf:session(currentjump_cur))
						++" of "
						++integer_to_list(wf:session(currentjump_num))],
				   colspan=2,class=[menuitem]}),
	Curfile=wf:session(selected_source),
	Filepath=referl_htmlgen:getFile(Node),
	if
	Filepath/="" ->
		if
			Curfile/=Filepath ->
				wf:wire(#script{script="lastcolored=null;"}),
				wf:session(click_node,Node),
				event({show_selection_links,Filepath});
			true ->
				wf:session(node_exists,undefined),
				showResult(Node)
		end,
		wf:session(selected_source, Filepath),
		wf:session(selectednode_data,undefined),
		wf:replace(curfilename,#tablecell{id=curfilename,colspan=7,class=[menuitem],
											body=["Current file: "++Filepath]});
	true -> ok
	end.

nodeQueryFun(Query,{PNode,Node},Type,User) ->
fun() ->
	if
		Query=="@file.references" ->
			Error=[],
			Nodes=lists:flatten([reflib_query:exec(X,
				[{esub,back},{esub,back}]) || X <- reflib_query:exec(PNode,reflib_module:references())]);
		true ->
			Req = {transform,semantic_query,
				  [{ask_missing,false},
					{send_back_query_id, true},
					{querystr,Query},
					{display_opt,[{output,nodes}]},
					{start_opt,[{node_list,[PNode]}]}]},
			case nitrogen_helper:make_ui_request(Req, [{user,User},{querystr,Query}]) of
				{ok, {result,[{result,Result}]}} -> Error=[];
				{ok, {abort,{_,Error}}} -> Result=[{nodes,error}];
				{ok, {error,Err}} -> Error="Fatal error: "++lists:flatten(io_lib:format("~p",[Err])),Result=[{nodes,error}];
				_ -> Error="Unknown error.",Result=[{nodes,error}]
			end,
			Nodes=proplists:get_value(nodes,Result)
	end,
	if
		Nodes==error ->
			{error,Query,Error};
		is_integer(hd(Nodes)) ->
			{hd(Nodes),Query,Type,Node};
		true ->
			{referl_htmlgen:filterNodes(Nodes),Query,Type,Node}
	end
end.

doQuery(Query,{PNode,Node},Type) ->
	case Type of
		macrodef -> ValidStarts=["@expr","@expression","@macro"];
		mod -> ValidStarts=["@expr","@expression","@file","@module","@mod"];
		recdef -> ValidStarts=["@expr","@expression","@record","@rec"];
		var -> ValidStarts=["@expr","@expression","@variable","@var"];
		funappl -> ValidStarts=["@expr","@expression","@function","@fun"];
		fundef -> ValidStarts=["@expr","@expression","@function","@fun"];
		recexpr -> ValidStarts=["@expr","@expression","@record","@rec"];
		recfield -> ValidStarts=["@expr","@expression","@recfield","@field"];
		atom -> ValidStarts=["@expr","@expression"];
		_ -> ValidStarts=[]
	end,
	case string:tokens(Query,".") of
		[Head|_] -> QueryStart=Head;
		[] -> QueryStart=""
	end,
	StartIsValid=lists:any(fun(X) -> QueryStart==X end, ValidStarts),
	if
		(Type==none) or (StartIsValid)-> Fun=nodeQueryFun(Query,{PNode,Node},Type,wf:user());
		true -> Fun=fun() -> {error,Query,"'"++QueryStart++"' is an illegal selector for this node." } end
	end,
	wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=[#p{body=nitrogen_helper:whitespaces_to_html(Query),
							   class="query_title"},
							#br{},
							#p{body="Running query, please wait.."},
							#br{}]}),
	wf:replace(query_result_placeholder, 
			   #panel{id=query_result_placeholder,body=[]}),
	wf:replace(jumptable,#table{id=jumptable}),
	cleanUp(),
	show_last_result_tab(),
	wf:continue({continue, jump}, Fun, 60*60*1000).		

getCustomQueries(Type) ->
	[#option{text="Choose previous query",value=" ",selected=true} |
	lists:map(
		fun(X) -> #option{text=X,value=X,selected=false} end,
		referl_htmlserver:getQueries(Type))
	].

cleanUp() ->
	wf:wire(#script{script="
				try{
					if(lastcolored!=null) $(lastcolored).removeClass('selectednode');
					lastcolored=null;
				}catch(ex){}"}),
	wf:session(selectednode_data, undefined),
	wf:replace(customqueries,#table{id=customqueries}),
	wf:replace(jumpoptions,#table{id=jumpoptions}),
	wf:replace(deselect,#table{id=deselect}).

event(deselect)->
	cleanUp();

event({query_jumptonode,Node,Num})->
	wf:replace(jumpcounter,
				#tablecell{id=jumpcounter,body=[
						integer_to_list(Num)
						++" of "
						++integer_to_list(wf:session(currentjump_num))],
				   		colspan=2,class=[menuitem]}),
	wf:session(currentjump_cur,Num),
	wf:wire(#script{script="
		try{
			if(obj('query_result"++integer_to_list(Num)++"')!=null) 
			{
				for(var i=0;i<="++integer_to_list(wf:session(currentjump_num))++";i++)
					obj('query_result'+i).style.backgroundColor='#FFFFD2';
				obj('query_result"++integer_to_list(Num)++"').style.backgroundColor='#EEEE00';
			}
		}catch(ex){}"}),
	jumpToNode(Node);

event(jumpprev)->
	Nextnum=wf:session(currentjump_cur)-1,
	Maxnum=wf:session(currentjump_num),
	if
		Maxnum==0 -> Jumpnum=1;
		Nextnum=<0 -> Jumpnum=Maxnum;
		true -> Jumpnum=Nextnum
	end,
	wf:session(currentjump_cur,Jumpnum),
	wf:wire(#script{script="try{$(obj('query_result"++integer_to_list(Jumpnum)++"')).click();}catch(ex){}"});

event(jumpnext)->
	Nextnum=wf:session(currentjump_cur)+1,
	Maxnum=wf:session(currentjump_num),
	if
		Nextnum>Maxnum -> Jumpnum=1;
		true -> Jumpnum=Nextnum
	end,
	wf:session(currentjump_cur,Jumpnum),
	wf:wire(#script{script="try{$(obj('query_result"++integer_to_list(Jumpnum)++"')).click();}catch(ex){}"});

event({predefquery,Query,Node})->
	event({docustomquery,Node,Query,none});

event({docustomquery,Node})->
	Query=wf:q(customqueries),
	Type=referl_htmlgen:getType(Node),
	event({docustomquery,Node,Query,Type});

event({docustomquery,Node,Query,Type})->
	NotExprQuery=lists:sublist(Query,5)/="@expr",
	if
		(Query/=[]) andalso (hd(Query)/=$@) -> event(run_new_query);
		Query=="" ->
			wf:wire(#alert{text="No custom query defined!"});
		NotExprQuery ->
			PNode=referl_htmlgen:getProperNode(Node),
			doQuery(Query,{PNode,Node},Type);
		true -> 
			doQuery(Query,{Node,Node},Type)
	end;

event(node_clicked)->
	wf:wire(#script{script="window.getSelection().removeAllRanges();"}),
	wf:session(node_exists,true),
	[D1,D2]=string:tokens(wf:q(hiddendata), "|"),
	TT=list_to_existing_atom(D1),
	Id=list_to_integer(D2),
	Node={'$gn',TT,Id},
	Type=referl_htmlgen:getType(Node),
	CurrentSel=wf:session(selectednode_data),
	if
	CurrentSel==Node -> 
		wf:wire(#script{script="
			try{
			elem=document.getElementById('jump_"++atom_to_list(TT)++integer_to_list(Id)++"');
			$(elem).removeClass('selectednode');
			lastcolored=null;
			}catch(ex){
			}"}),
		cleanUp();
	true ->
		wf:wire(#script{script="
		try{
		elem=document.getElementById('jump_"++atom_to_list(TT)++integer_to_list(Id)++"');
		if(lastcolored!=null) 
		{
				$(lastcolored).removeClass('selectednode');
		}
		$(elem).addClass('selectednode');
		lastcolored=elem;
		}catch(ex){
		}"}),
		case Type of
		var ->
			wf:replace(jumpoptions,
				#table{id=jumpoptions,rows=[#tablerow{cells=[#tablecell{body=
					#button{id=q1, 
					text="Var References", 
					postback={predefquery,"@var.references",Node},
					class=[referl_button_fixedwidth]}},#tablecell{body=
					#button{id=q2, 
					text="Var Binding", 
					postback={predefquery,"@var.bindings",Node},
					class=[referl_button_fixedwidth]}
					}]},
					#tablerow{cells=[#tablecell{body=
					#button{id=q3, 
					text="Var Origin", 
					postback={predefquery,"@expr.origin",Node},
					class=[referl_button_fixedwidth]}},#tablecell{body=
					#button{id=q4, 
					text="Var Reach", 
					postback={predefquery,"@expr.reach",Node},
					class=[referl_button_fixedwidth]}
					}]}]});				
		funappl ->
			wf:replace(jumpoptions,
				#table{id=jumpoptions,
				rows=[#tablerow{cells=#tablecell{body=
					#button{id=q1, 
					text="Function References", 
					postback={predefquery,"@fun.references",Node},
					class=[referl_button_fixedwidth]}}},#tablerow{cells=#tablecell{body=
					#button{id=q2, 
					text="Function Definition", 
					postback={predefquery,"@expr.funs",Node},
					class=[referl_button_fixedwidth]}}}
					]});
		fundef ->
			wf:replace(jumpoptions,#table{id=jumpoptions,
				rows=[#tablerow{cells=[#tablecell{body=
					#button{id=q1, 
					text="Function References", 
					postback={predefquery,"@fun.references",Node},
					class=[referl_button_fixedwidth]}}]}]});
		mod ->
			wf:replace(jumpoptions,#table{id=jumpoptions,
				rows=[#tablerow{cells=[#tablecell{body=
					#button{id=q1, 
					text="Functions", 
					postback={predefquery,"@file.funs",Node},
					class=[referl_button_fixedwidth]}},#tablecell{body=
					#button{id=q2, 
					text="Module References", 
					postback={predefquery,"@file.references",Node},
					class=[referl_button_fixedwidth]}}]},#tablerow{cells=#tablecell{body=
					#button{id=q3, 
					text="Records", 
					postback={predefquery,"@file.records",Node},
					class=[referl_button_fixedwidth]}}}
					]});
		macrodef ->
			wf:replace(jumpoptions,#table{id=jumpoptions,
				rows=[#tablerow{cells=[#tablecell{body=
					#button{id=q1, 
					text="Macro References", 
					postback={predefquery,"@macro.references",Node},
					class=[referl_button_fixedwidth]}}]}]});
		recdef ->
			wf:replace(jumpoptions,#table{id=jumpoptions,
				rows=[#tablerow{cells=[#tablecell{body=
					#button{id=q1, 
					text="Record References", 
					postback={predefquery,"@record.references",Node},
					class=[referl_button_fixedwidth]}}]}]});
		recexpr ->	
			wf:replace(jumpoptions,#table{id=jumpoptions,
				rows=[#tablerow{cells=[#tablecell{body=
					#button{id=q1, 
					text="Record References", 
					postback={predefquery,"@record.references",Node},
					class=[referl_button_fixedwidth]}},#tablecell{body=
					#button{id=q2, 
					text="Record Def", 
					postback={predefquery,"@expr.records",Node},
					class=[referl_button_fixedwidth]}}]},#tablerow{cells=#tablecell{body=
					#button{id=q3, 
					text="All Field References", 
					postback={predefquery,"@record.fields.references",Node},
					class=[referl_button_fixedwidth]}}}
					]});
		recfield ->
			wf:replace(jumpoptions,#table{id=jumpoptions,
				rows=[#tablerow{cells=[#tablecell{body=
					#button{id=q1, 
					text="Field References", 
					postback={predefquery,"@field.references",Node},
					class=[referl_button_fixedwidth]}}]}]});
		_ ->
			wf:replace(jumpoptions,#table{id=jumpoptions})
		end,
		wf:replace(customqueries,#dropdown{id=customqueries,options=getCustomQueries(Type),
					                       postback={docustomquery,Node}}),
		wf:replace(deselect,#button{id=deselect, 
									text="Deselect Node", 
									postback=deselect, 
									class=[referl_button_small]}),
		wf:session(selectednode_data,Node)
	end;

event({use_skeleton,{Name, _Body, _Owner, _Card}})->
	Call=nitrogen_helper:skeleton_call_format(Name),
	wf:set(query_box, Call);

event({delete_skeleton,{Name, _Body, _Owner, _Card}})->
	nitrogen_helper:delete_skeleton(Name),
	wf:replace(skeletons_placeholder, stored_skeletons());

event({edit_skeleton, Skel={_Name, Body, _Owner, _Card}})->
	UpdateSkelBtn=#button{id=save_skeleton_button, 
			text="Update skeleton", 
			postback={update_skeleton, Skel}, 
			class=[referl_button]},
	wf:set(query_box, Body),
	wf:replace(save_skeleton_button, UpdateSkelBtn),
	wf:wire(save_skeleton_button, 
			query_box, 
			#validate { validators=[#is_required { text="Required." }]});

event({update_skeleton, {Name, _Body, _Owner, _Card}})->
	NewBody=wf:q(query_box),
	SaveSkelBtn=#button{id=save_skeleton_button, 
			text="Save as skeleton", 
			postback=save_skeleton_req, 
			class=[referl_button]},
	wf:replace(save_skeleton_button, SaveSkelBtn),
	wf:wire(save_skeleton_button, 
					query_box, 
					#validate { validators=[#is_required { text="Required." }]}),
	message_box_hide(skeleton_message_panel_placeholder,
							  fun()->skeleton_message_panel() end),
	case nitrogen_helper:update_skeleton(Name, NewBody, wf:user()) of
		ok ->wf:replace(skeletons_placeholder, stored_skeletons());
		{error, Msg}->Content=#panel{body="Error: "++Msg, class=["centered", "error"]},
					  message_box(Content, skeleton_message_panel_placeholder, fun()->show_skeleton_tab() end)
	end;

event(save_skeleton)->
	SkelName=wf:q(skel_name),
	SkelBody=wf:q(query_box),
	case nitrogen_helper:save_skeleton(SkelName, SkelBody, wf:user()) of
		ok ->wf:replace(skeletons_placeholder, stored_skeletons()),
			 message_box_hide(skeleton_message_panel_placeholder,
							  fun()->skeleton_message_panel() end);
		{error, Msg}->Content=#panel{body="Error: "++Msg, class=["centered", "error"]},
					  message_box(Content, skeleton_message_panel_placeholder, fun()->show_skeleton_tab() end)
	end;

event(save_skeleton_req)->
	Content=#panel{body=["Save skeleton as:", 
						 #textbox{id=skel_name, next=new_skel_button}, 
						 #button{id=new_skel_button, 
								 text="Save", 
								 postback=save_skeleton, 
								 class=[referl_button]}], 
				   class="centered"},
	message_box(Content, skeleton_message_panel_placeholder, fun()->show_skeleton_tab() end),
	wf:wire(new_skel_button, 
			skel_name, 
			#validate { validators=[#is_required { text="Required." }]});
	
event(logout) ->
	nitrogen_lib:logout(wf:user());

event(run_new_query)->
	wf:replace(jumptable,#table{id=jumptable}),
	event(deselect),
	Q=wf:q(query_box),
	%selected file as starting point
	F=wf:session(selected_source),
	%selected position as starting point
	P=wf:session(selected_position),
	Pos=case {is_integer(P), is_list(P), is_atom(P)} of
		{true, false, false} -> P;
		{false, true, false} -> list_to_integer(P);
		{false, false, true} when P/=undefined->
			list_to_integer(atom_to_list(P))+1;
		{false, false, true} ->undefined
	end,							
	%wf:replace(query_result_source_code_placeholder,
	%		  query_result_source_code()),
	wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=[#p{body=nitrogen_helper:whitespaces_to_html(Q),
							   class="query_title"},
							#br{},
							#p{body="Running query, please wait.."},
							#br{}]}),
	wf:replace(query_result_placeholder, 
			   #panel{id=query_result_placeholder,body=[]}),
	{File, Position}=case string:chr(Q,$@) of
						 0 -> {undefined, undefined};
						 _ when (F/=undefined) and (Pos/=undefined) -> {F,Pos};
						 _ -> {undefined, undefined}
					 end,
	show_last_result_tab(),
	User=wf:user(),
	Fun = fun() -> run_query({unknown,{Q, File, Position},User}) end,
    wf:continue({continue, posquery, {Q, File, Position}}, Fun, 60*60*1000);

event({run_query,SafeQuery,Q})-> 
	wf:replace(jumptable,#table{id=jumptable}),
	event(deselect),
	wf:set(query_box, Q),
	wf:replace(query_result_source_code_placeholder,
			   query_result_source_code()),
	wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=[#p{body=nitrogen_helper:whitespaces_to_html(Q),
							   class="query_title"},
							#br{},
							#p{body="Running query, please wait.."},
							#br{}]}),
	wf:replace(query_result_placeholder, 
			   #panel{id=query_result_placeholder,body=[]}),
	show_last_result_tab(),
	User=wf:user(),
	Fun = fun() -> run_query({prev_query,SafeQuery,User}) end,
    wf:continue({continue, posquery, SafeQuery}, Fun, 60*60*1000);

event({run_query_node,SafeQuery,Q})->
	wf:replace(jumptable,#table{id=jumptable}),
	cleanUp(),
	wf:set(query_box, Q),
	wf:replace(query_result_source_code_placeholder,
			   query_result_source_code()),
	wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=[#p{body=nitrogen_helper:whitespaces_to_html(Q),
							   class="query_title"},
							#br{},
							#p{body="Running query, please wait.."},
							#br{}]}),
	wf:replace(query_result_placeholder, 
			   #panel{id=query_result_placeholder,body=[]}),
	show_last_result_tab(),
	User=wf:user(),
	Fun = fun() -> run_query({prev_query,SafeQuery,User}) end,
    wf:continue({continue, jump}, Fun, 60*60*1000);

event({delete_query,SafeQuery})->
	case wf:role(admin) andalso wf:q(query_owner)=="all" of
		true ->	nitrogen_helper:delete_from_qtab(SafeQuery,admin);
		false -> nitrogen_helper:delete_from_qtab(SafeQuery,wf:user())
	end,
	event(load_list);

event({abort_query,QueryId})->
	Message=case nitrogen_helper:kill_query(QueryId) of
		ok -> #p{body="Query is aborted successfully."};
		not_found -> #p{body="Query isn't found, sorry."}
	end,
	wf:replace(running_queries_placeholder,
			   #panel{id=running_queries_placeholder,
					  body=[Message]});

event(run_new_query_node)->
	RunFromNode = (wf:session(selectednode_data)/=undefined),
	if
		RunFromNode ->
			Node=wf:session(selectednode_data),
			if
				Node/=undefined ->
					Q=wf:q(query_box),
					case nitrogen_helper:determine_sq_request_type(Q) of
						sem_query -> Query=Q;
						skeleton ->
							{ok, Elements, _}=erl_scan:string(Q),
   							{_,_, SkelName}=hd(Elements),
   							ParamsList=lists:sublist(Elements, 3, length(Elements)-4),
   							Parameters=[atom_to_list(element(3, Item)) || 
                  			Item <- ParamsList, is_tuple(Item), element(1, Item)==atom],
							Query=nitrogen_helper:evaluate_skeleton(atom_to_list(SkelName), Parameters, 0, onlyconvert);
						_ -> Query=error
					end,
					if 
						(Query/=error) andalso (hd(Query)/=$@) -> event(run_new_query);
						Query/=error -> Type=referl_htmlgen:getType(Node),event({docustomquery,Node,Query,Type});
						true -> wf:wire(#alert{text=Query++" is not a valid query from this node!"})
					end;
				true -> ok
			end;
		true ->
			cleanUp(),
			event(run_new_query)
	end;

event({show_selection,File, StartPos, EndPos})->
	event({show_selection,File, StartPos, EndPos, none});
event({show_selection,File, StartPos, EndPos, Num})->
	case Num of
		none -> ok;
		_ -> 
			wf:replace(jumpcounter,
				#tablecell{id=jumpcounter,body=[
						integer_to_list(Num)
						++" of "
						++integer_to_list(wf:session(currentjump_num))],
				   		colspan=2,class=[menuitem]}),
			wf:session(currentjump_cur,Num),
			wf:wire(#script{script="
				try{
				if(obj('query_result"++integer_to_list(Num)++"')!=null) 
				{
					for(var i=0;i<="++integer_to_list(wf:session(currentjump_num))++";i++)
						obj('query_result'+i).style.backgroundColor='#FFFFD2';
					obj('query_result"++integer_to_list(Num)++"').style.backgroundColor='#EEEE00';
				}
				}catch(ex){}"})
	end,
	wf:session(selected_file, File),
	wf:session(selected_position, StartPos-1),
	wf:session(selected_startpos, StartPos-1),
	wf:session(selected_endpos, EndPos),
	event(show_file_links);

event(show_file_links)->
	wf:replace(new_query_button,#button{id=new_query_button, 
				text="Run", 
				postback=run_new_query_node,
				class=[referl_button]}),
	File=wf:session(selected_file),
	case File of
		undefined -> ok;%wf:wire(#alert{text="Please, select a file first!"});
		_ ->
			case lists:suffix("/", File) of
				true ->
					ok;%wf:wire(#alert{text="Please, select a file first!"});
				false ->
					wf:replace(curfilename,#tablecell{id=curfilename,colspan=7,
							   class=[menuitem],body=["Current file: "++File]}),
					cleanUp(),
					event({show_selection_links,File})
			end
	end;

event({show_selection_links,File})->
	LastFile=wf:session(selected_source),
	wf:session(selected_source, File),
	if
		File==LastFile -> 
			wf:wire(#script{script="
				try{
				if(lastcolored!=null) 
				{
					$(lastcolored).removeClass('selectednode');
				}
				}catch(ex){}"}),
			Fun = fun() -> same end;
		true ->
			referl_htmlserver:generate(File),
			Fun=fun() -> referl_htmlserver:getdata(File) end,
			wf:replace(linenums,#panel{id=linenums,body=[],class=["lnums"]}),
			wf:replace(query_result_source_code_placeholder,#panel{id=query_result_source_code_placeholder,body="Loading file, please wait..."}),
			wf:wire(#script{script="$('#mainwrapper').scrollTop(0);"}),
			wf:wire(query_result_source_code_placeholder,#hide{}),
			wf:wire(query_result_source_code_placeholder,#appear{})
	end,
	wf:continue({continue, generate}, Fun, 60*60*1000);

event(load_list)->
	Who=wf:q(query_owner),
	wf:replace(query_list,
			   #table{id=query_list,class="query_list_table", rows=[]}),
	LoadListFun=load_list_function(),
	case Who of
		"my"->List=nitrogen_helper:query_list_from_qtab(wf:user()),
			  lists:foreach(LoadListFun, List);
		"all"->List=nitrogen_helper:query_list_from_qtab("all"),
			  lists:foreach(LoadListFun, List)
	end;

event({show_comment_editor, SelfId, RowId})->
	wf:wire(RowId, #appear{speed=50}),
	Cell=#tablecell{id=SelfId,
						  body=[#link {text="C",
									class="delete_query_a", 
									title = "Edit comments of the query", 
									postback={hide_comment_editor,
											  SelfId, RowId }}],
							align="left",
							valign="middle",
							class=["query_list_tablecell","width_15", "white_border"]},
	wf:replace(SelfId, Cell);

event({hide_comment_editor, SelfId, RowId})->
	wf:wire(RowId, #fade{speed=50}),
	Cell=#tablecell{id=SelfId,
						  body=[#link {text="C",
									class="delete_query_a", 
									title = "Edit comments of the query", 
									postback={show_comment_editor,
											  SelfId, RowId }}],
							align="left",
							valign="middle",
							class=["query_list_tablecell","width_15", "white_border"]},
	wf:replace(SelfId, Cell);

event({save_comment, {sem_query, SafeQuery}, CommentTextBoxId})->
	NewComment=case wf:q(CommentTextBoxId) of
		undefined -> "";
		R -> R
	end,
	nitrogen_helper:update_prev_query_comment(SafeQuery, NewComment),
	event(load_list);

event({save_comment, {skeleton, SkelName}, CommentTextBoxId})->
	NewComment=case wf:q(CommentTextBoxId) of
		undefined -> "";
		R -> R
	end,
	nitrogen_helper:update_prev_skeleton_comment(SkelName, NewComment),
	wf:replace(skeletons_placeholder, stored_skeletons());

event({show_help,TempIdLink,TempIdCell,TempIdRow})->
	HelpLink=#link {id=TempIdLink,
												   text="?",
													class="delete_query_a", 
													title = "Hide information", 
													postback={hide_help,
															  TempIdLink, 
															  TempIdCell,
															  TempIdRow}},
	wf:replace(TempIdLink,HelpLink),
	wf:wire(TempIdRow, #appear{speed=50});

event({hide_help,TempIdLink,TempIdCell,TempIdRow})->
	HelpLink=#link {id=TempIdLink,
												   text="?",
													class="delete_query_a", 
													title = "Show information", 
													postback={show_help,
															  TempIdLink, 
															  TempIdCell,
															  TempIdRow}},
	wf:replace(TempIdLink,HelpLink),
	wf:wire(TempIdRow, #fade{speed=50});

event({show_errors,Message})->
	HideErrorsLink=#link{id=hide_errors_link, 
						 text=Message, 
						 postback={hide_errors,Message}, 
						 class="error_link"},
	wf:replace(show_errors_link,HideErrorsLink),
	wf:wire(warning_table, #appear{speed=100});

event({hide_errors,Message})->
	ShowErrorsLink=#link{id=show_errors_link, 
						 text=Message, 
						 postback={show_errors,Message}, 
						 class="error_link"},
	wf:replace(hide_errors_link,ShowErrorsLink),
	wf:wire(warning_table, #fade{speed=100});

event({alias_query, Alias, SafeQuery, FstCellId, FstCell,FourthCellId})->
	TBId=wf:temp_id(),
	TextB=#textbox { id=TBId, text=Alias},
	SaveB=#button{  text="Save", 
					postback={save_alias, SafeQuery, TBId}, 
					class=referl_button },
	Panel=#panel{body=[TextB, SaveB]},
	Cell=#tablecell{id=FstCellId,
				    body=[Panel],
					align="left",
					valign="middle",
					class=["query_list_tablecell", "white_border"]},
	wf:replace(FstCellId, Cell),
	FourthCell=#tablecell{id=FourthCellId,
						  body=[#link {text="E",
									class="delete_query_a", 
									title = "Cancel", 
									postback={cancel_alias, 
											  Alias, 
											  SafeQuery, 
											  FstCellId, 
											  FstCell,
											  FourthCellId}}],
							align="left",
							valign="middle",
							class=["query_list_tablecell","width_15", "white_border"]},
	wf:replace(FourthCellId, FourthCell);

event({cancel_alias, Alias, SafeQuery, FstCellId, FstCell,FourthCellId})->
	FourthCell=#tablecell{id=FourthCellId,
						  body=[#link {text="E",
									class="delete_query_a", 
									title = "Assign a name to the query", 
									postback={alias_query, 
											  Alias, 
											  SafeQuery, 
											  FstCellId, 
											  FstCell,
											  FourthCellId}}],
							align="left",
							valign="middle",
							class=["query_list_tablecell","width_15" , "white_border"]},
	wf:replace(FourthCellId, FourthCell),
	wf:replace(FstCellId,FstCell);

event({save_alias, SafeQuery, TBId})->
	case wf:q(TBId) of
		undefined -> wf:wire(#alert{text="Please, type the name of the query into the textbox!"});
		Name when length(Name)>0-> case nitrogen_helper:set_alias(SafeQuery, Name) of
									   ok -> event(load_list);
									   {error, E} -> wf:wire(#alert{text=E})
								   end;
		_ -> wf:wire(#alert{text="Please, type the name of the query into the textbox!"})
	end.

%%% ============================================================================
%%% Handlers for continous events	

continue({continue, jump}, {error,Query,Error}) ->
	wf:replace(jumptable,#table{id=jumptable}),
	query_result_handler({Query,0,0},{{result, no_result},{warning,no_warning},{error,Error}});
continue({continue, jump}, {FilteredNodes,Query,Type,StartNode}) when is_integer(FilteredNodes)->
	if
		Type/=none ->
			nitrogen_helper:update_tab_if_needed({Query, node, StartNode},Query,wf:user(),Query,FilteredNodes,referl_misc:database_hash()),
			event(load_list);
		true -> ok
	end,
	if
		FilteredNodes/=[] ->
			wf:replace(jumpoptions,#table{id=jumpoptions}),
			referl_htmlserver:addQuery(Type,Query);
		true -> ok
	end,
	Result=[{group_by,{nopos,"Result"},list,[{result,integer_to_list(FilteredNodes)}]}],
	query_result_handler({Query,0,0},{{result, Result},{warning,no_warning},{error,no_error}}),
	wf:session(currentjump_num,0),
	wf:session(currentjump_cur,0),
	wf:replace(jumptable,jumptable()),
	wf:replace(jumpcounter,#tablecell{id=jumpcounter,body=["0 of 0"],colspan=2,class=[menuitem]});
continue({continue, jump}, {FilteredNodes,Query,Type,StartNode}) ->
	FileList=lists:keysort(2,lists:map(fun(X) -> {X,referl_htmlgen:getFile(X)} end, FilteredNodes)),
	SortedResult=lists:map(fun({X,_}) -> X end, FileList),
	if
		Type/=none ->
			referl_htmlserver:addQuery(Type,Query),
			nitrogen_helper:update_tab_if_needed({Query, node, StartNode},Query,wf:user(),Query,SortedResult,referl_misc:database_hash()),
			event(load_list);
		true -> ok
	end,
	GroupedList=groupFiles(FileList),
	Result=lists:map(fun(X) -> 
		InnerList=lists:map(fun(Y) -> {{node,node,Y},nodeToText(Y)} end, tl(X)),
		{group_by,{nopos,lists:last(string:tokens (hd(X), "/"))},list,InnerList} end, GroupedList),
	query_result_handler({Query,0,0},{{result, Result},{warning,no_warning},{error,no_error}}),
	wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=[#p{body=nitrogen_helper:whitespaces_to_html(Query),
							   class="query_title"},
							#p{body=["Starting node: ",
									 #link{id=query_result0,
										   postback={query_jumptonode, StartNode, 0},
										   body=nodeToText(StartNode)}
									],
							   class="query_title"}]}),
	wf:session(currentjump_num,length(FilteredNodes)),
	wf:session(currentjump_cur,0),
	wf:replace(jumptable,jumptable()),
	if 
		FilteredNodes/=[] ->
			wf:session(currentjump_nodes,SortedResult),
			wf:session(currentjump_num,length(FilteredNodes)),
			wf:session(currentjump_cur,1),
			wf:wire(#script{script="try{$(obj('query_result1')).click();}catch(ex){}"});
		true -> 
			wf:replace(jumpcounter,#tablecell{id=jumpcounter,body=["0 of 0"],colspan=2,class=[menuitem]}),
			wf:session(currentjump_nodes,undefined)
	end;

continue({continue, generate}, same) ->
	StartPos=wf:session(selected_startpos),
	if 
		StartPos /= undefined ->
			wf:wire(#script{script="highlight(obj('query_result_source_code_placeholder'),"
				++integer_to_list(wf:session(selected_startpos))++","++integer_to_list(wf:session(selected_endpos))++");"});
		true -> ok
	end,
	wf:session(selected_startpos,undefined);

continue({continue, generate}, Text) ->
	wf:replace(query_result_source_code_placeholder,
				#panel{id=query_result_source_code_placeholder,
					body=Text,class=[monosp]}),
	StartPos=wf:session(selected_startpos),
	if 
		StartPos /= undefined ->
			wf:wire(#script{script="try{highlight(obj('query_result_source_code_placeholder'),"
				++integer_to_list(wf:session(selected_startpos))++","++integer_to_list(wf:session(selected_endpos))++");}catch(ex){}"});
		true -> ok
	end,
	wf:wire(#script{script="generatelinenums(obj('query_result_source_code_placeholder'),obj('linenums'));"}),
	wf:session(selected_startpos,undefined),
	Node=wf:session(click_node),
	if
		Node==undefined -> ok;
		true ->
			showResult(Node),
			wf:session(click_node,undefined)
	end;

continue({continue, check_if_node_exists}, Node) ->
	NodeExists=wf:session(node_exists),
	if
		NodeExists/=true ->
			Tokens=refcore_syntax:leaves(Node),
			File=hd(reflib_query:exec(reflib_file:find(referl_htmlgen:getFile(Node)))),
			{Start,_}=reflib_token:pos(File,hd(Tokens)),
			{_,End}=reflib_token:pos(File,lists:last(Tokens)),
			wf:wire(#script{script="
			try{
				highlight(obj('query_result_source_code_placeholder'),"
				++integer_to_list(Start-1)++","++integer_to_list(End)++");
			}catch(ex){}"});
		true -> wf:session(node_exists,undefined)
	end;

continue({continue, posquery,{Q,File,StartPos}}, timeout) ->
	query_result_handler({Q,File,StartPos},timeout),
	event(load_list);

continue({continue, posquery, {Q,File,StartPos}}, Result) ->
	query_result_handler({Q,File,StartPos},Result),
	event(load_list),
	wf:wire(#script{script="try{$(obj('query_result1')).click();}catch(ex){}"}).
%%% ============================================================================
%%% Helper functions
nodeToText(Node) ->
	try 
		refcore_esg:data(Node),
		case Node of
			{_,clause,_} -> 
				FuncNode=reflib_query:exec(Node,reflib_query:seq([reflib_clause:form(),reflib_form:func()])),
				if
					FuncNode==[] -> "";
					true ->	
						atom_to_list(reflib_module:name(reflib_query:exec1(hd(FuncNode),reflib_function:module(),error)))
						++":"++atom_to_list(reflib_function:name(hd(FuncNode)))
						++"/"++integer_to_list(reflib_function:arity(hd(FuncNode)))
				end;
			_ ->  referl_htmlgen:onlyText(Node)
		end
	catch
	      _:_ -> "Node does not exist anymore."
	end.

show_last_result_tab()->
	wf:wire(#script{script="$('#tabs').tabs('select', 4);"}).

show_skeleton_tab()->
	wf:wire(#script{script="$('#tabs').tabs('select', 3);"}).

message_box(Content)->
	message_box(Content, executed_query_placeholder, fun()->show_last_result_tab() end).

message_box(Content, What, Fun)->
	Cell=#tablecell{body=Content, class=["graph_controll_tablecell", "red_border"]},
	Table=#table{ rows=[#tablerow{cells=[Cell]}], class="query_result_table"},
	Panel=#panel{id=What, body=[Table], class=["width_100"]},
	wf:replace(What, Panel),
	wf:replace(query_result_placeholder, query_result()),
	wf:replace(query_result_source_code_placeholder, query_result_source_code()),
	Fun().

message_box_hide()->
	message_box_hide(executed_query_placeholder, fun()->executed_query() end).

message_box_hide(What, Fun)->
	wf:wire(What, #hide {}),
	wf:replace(What, Fun()),
	wf:replace(query_result_placeholder, query_result()),
	wf:replace(query_result_source_code_placeholder, query_result_source_code()),
	wf:wire(What, #show {}).

skeletons_table()->
	Skeletons=nitrogen_helper:list_skeletons(),
	Rows=case lists:foldl(skeletons_table_fun(), [], Skeletons) of
		[] -> EmptyListCell=#tablecell{body=[#p{body="No skeleton exists."}]},
			  [#tablerow{cells=[EmptyListCell]}];
		R -> R
	end,
	#table{class="query_list_table", 
				 rows=Rows}.

skeletons_table_fun()->
	fun({Name, Body, Owner, ParamCardinality, Comment}, AccIn)->
           Skel={Name, Body, Owner, ParamCardinality},
		   SkelLabel=split_label_into_rows(lists:flatten(Name++"/"++
														io_lib:format("~p",[ParamCardinality]))),
		   Cell1=#tablecell{body=[#link {text=SkelLabel,
									title = "Use skeleton", 
									postback={use_skeleton,Skel}}],
							align="left",
							valign="middle",
							class=["query_list_tablecell", "white_border"]},
			
			InfoStr="Name: "++Name++"<br/>"++
					"Body: "++Body++"<br/>"++
					"Owner: "++Owner++"<br/>",
	        CommentInfo=if (length(Comment)>0)->
				             "Comments: "++Comment++"<br/>";
			            true -> "Comments: -- <br/>"
			end,
			{HelpCell, InfoRow}=generate_help(InfoStr++CommentInfo),
			EditCell=case wf:user() of
						Owner ->#tablecell{body=[#link {text="E",
									class="delete_query_a", 
									title = "Edit skeleton", 
									postback={edit_skeleton,Skel}}],
							align="left",
							valign="middle",
							class=["query_list_tablecell","width_15", "white_border"]};
						_ ->#tablecell{body=[], class=["width_15", "white_border"]}
					 end,
			DeleteCell=case wf:user() of
						 Owner ->#tablecell{body=[#link {text="X",
									class="delete_query_a", 
									title = "Delete skeleton", 
									postback={delete_skeleton,Skel}}],
							align="left",
							valign="middle",
							class=["query_list_tablecell","width_15", "white_border"]};
						_ ->#tablecell{body=[], class=["width_15", "white_border"]}
					 end,
           {RowId,CommentEditorRow}=generate_comment_editor({skeleton, Name},Comment),
           FifthCellId=wf:temp_id(),
	       FifthCell=#tablecell{id=FifthCellId,
			        	            body=[#link {text="C",
									class="delete_query_a", 
									title = "Edit comments of the skeleton", 
									postback={show_comment_editor,
											  FifthCellId, RowId }}],
							align="left",
							valign="middle",
							class=["query_list_tablecell","width_15", "white_border"]},
	
		   [#tablerow{cells=[Cell1, HelpCell, DeleteCell, EditCell, FifthCell]}, 
            InfoRow, 
            CommentEditorRow]++AccIn
	end.

generate_help(InfoStr)->
	TempIdCell=wf:temp_id(),
	TempIdRow=wf:temp_id(),
	TempIdLink=wf:temp_id(),
	HelpLink=#link {id=TempIdLink,
					text="?",
					class="delete_query_a", 
					title = "Show information", 
					postback={show_help,TempIdLink, TempIdCell,TempIdRow}},
	HelpCell=#tablecell{body=[HelpLink],
					   align="left",
					   valign="middle",
					   class=["query_list_tablecell", "width_15", "white_border"]},

	Cell=#tablecell{id=TempIdCell,
					 body=[#p{body=InfoStr}],
					 class=["info_tablecell", "red_border"]},
	EmptyCell=#tablecell{body=["&nbsp;"], class="width_15"},
	InfoRow=#tablerow{id=TempIdRow,cells=[Cell, EmptyCell, EmptyCell, EmptyCell],style="display:none;"},
	{HelpCell, InfoRow}.

background_update()->
	timer:sleep(1000),
	Queries=nitrogen_helper:get_running_queries(wf:user()),
	Rows=case lists:foldl(queries_table_fun(), [], Queries) of
		[] -> EmptyListCell=#tablecell{body=[#p{body="Queries are not running now."}]},
			  [#tablerow{cells=[EmptyListCell]}];
		R -> R
	end,
	Table=#table{class="query_list_table", 
				 rows=Rows},
	wf:replace(running_queries_placeholder,
			   #panel{id=running_queries_placeholder,
					  body=[Table]}),
	wf:flush(),
	background_update().

queries_table_fun()->
	fun({QueryId, QueryStr,Ownership}, AccIn)->
		   Cell1=#tablecell{body=[#p{body=split_label_into_rows(QueryStr)}],
							align="left",
							valign="middle",
							class=["query_list_tablecell", "white_border"]},
		   Cell2=case Ownership of
					 own->
						 #tablecell{body=[#link {text="X",
									class="delete_query_a", 
									title = "Abort query", 
									postback={abort_query,QueryId}}],
									align="left",
									valign="middle",
									class=["query_list_tablecell","width_15"]};
					 extraneous->
						 #tablecell{body="",
									align="left",
									valign="middle",
									class=["query_list_tablecell","width_15"]}
				 end,
		   [#tablerow{cells=[Cell1, Cell2]}]++AccIn
	end.

query_result_handler({Q,_,_},timeout)->
wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=[#p{body=nitrogen_helper:whitespaces_to_html(Q),
							   class="query_title"},
							#br{},
							#p{body="The execution of the query has been exceeded the timeout limit.",
							   class="error"},
							#br{}]}),
	wf:replace(query_result_placeholder, 
			   #panel{id=query_result_placeholder, body=[]});

query_result_handler({Q,_,_},{error,E})->
	wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=[#p{body=nitrogen_helper:whitespaces_to_html(Q),
							   class="query_title"},
							#br{},
							#p{body=E,
							   class="error"},
							#br{}]}),
	wf:replace(query_result_placeholder, 
			   #panel{id=query_result_placeholder,body=[]});

query_result_handler({Q,_,_},{{result, no_result},{warning,no_warning},{error,no_error}})->
	wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=[#p{body=nitrogen_helper:whitespaces_to_html(Q),
							   class="query_title"},
							#br{},
							#p{body="nitrogen_helper crashed",
							   class="error"},
							#br{}]}),
	wf:replace(query_result_placeholder, 
			   #panel{id=query_result_placeholder, body=[]});

query_result_handler({Q,_,_},{{result, no_result},{warning,no_warning},{error,E}})->
	wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=[#p{body=nitrogen_helper:whitespaces_to_html(Q),
							   class="query_title"},
							#br{},
							#p{body=E,
							   class="error"},
							#br{}]}),
	wf:replace(query_result_placeholder, 
			   #panel{id=query_result_placeholder,body=[]});

query_result_handler({Q,File,StartPos},{{result, Result},{warning,no_warning},{error,no_error}})->
	if 
		is_list(File) ->
			wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=[#p{body=nitrogen_helper:whitespaces_to_html(Q),
							   class="query_title"},
							#p{body=#link {id="query_result0",
				  						   text="Starting position", 
				  						   postback={show_selection, File, StartPos-1, StartPos, 0}},
							   class="query_title"}]});
		true -> wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=#p{body=nitrogen_helper:whitespaces_to_html(Q),
							   class="query_title"}})
	end,
	wf:replace(query_result_placeholder,
			   #panel{id=query_result_placeholder,
					  body=[#table{id=query_result_table,
								   class="queries_result_table",
								   rows=html_from_result(Result)},
							#br{}]});

query_result_handler({Q,File,StartPos},{{result, Result},{warning,Warning},{error,no_error}})->
	if 
		is_list(File) ->
			wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=[#p{body=nitrogen_helper:whitespaces_to_html(Q),
							   class="query_title"},
							#p{body=#link {id="query_result0",
				  						   text="Starting position", 
				  						   postback={show_selection, File, StartPos, StartPos+1, 0}},
							   class="query_title"}]});
		true -> wf:replace(executed_query_placeholder,
			   #panel{id=executed_query_placeholder,
					  body=#p{body=nitrogen_helper:whitespaces_to_html(Q),
							   class="query_title"}})
	end,
	wf:replace(warning_messages_placeholder,
			   #panel{id=warning_messages_placeholder,
					  body=Warning}),
	wf:replace(query_result_placeholder,
			   #panel{id=query_result_placeholder,
					  body=[#table{id=query_result_table,
								   class="queries_result_table",
								   rows=html_from_result(Result)},
							#br{}]}).

warning_handler({Message, Warning}) when is_list(Warning)->
	[#link{id=show_errors_link, 
		   text=Message, 
		   postback={show_errors,Message}, 
		   class="error_link"},
	 #table{id=warning_table,
		   class="queries_result_table",
		   rows=lists:foldl(fun({File, StartPos, Length,ErrorMessage},Acc)->
								Link=#link {text=File,
									   title = "Show", 
									   postback={show_selection, 
												 File, 
												 StartPos, 
												 StartPos+Length-1}},
								Cell1=#tablecell{body=[Link],
												align="left",
												valign="middle",
												class="queries_result_tablecell"},
								Cell2=#tablecell{body=ErrorMessage,
												align="left",
												valign="middle",
												class="queries_result_tablecell"},
								Acc++[#tablerow{cells=[Cell1,Cell2]}]
							end, [], Warning),
		 style="display:none;"}];

warning_handler(_)->"".

load_list_function()->
	Fun=fun(E)->
	CurUser=wf:user(),
	{SafeQuery={SQ,File, Pos},Q,Alias, Comment, Users}=E,
	ThrdCell=case lists:member(CurUser, Users) orelse wf:role(admin) of
		true ->
			#tablecell{body=[#link {text="X",
									class="delete_query_a", 
									title = "Delete query", 
									postback={delete_query,SafeQuery}}],
							align="left",
							valign="middle",
							class=["query_list_tablecell","width_15", "white_border"]};
		false ->#tablecell{body=[], class=["width_15", "white_border"]}
	end,
	FstCellId=wf:temp_id(),
	case SafeQuery of
		{_,node,_} ->
			FstCell=#tablecell{id=FstCellId,
							   body=[#link{body=split_label_into_rows(Alias),
												title = "Run query",
												postback={run_query_node,SafeQuery,Q}}],
									align="left",
									valign="middle",
									class=["query_list_tablecell", "white_border"]};
		_ ->
			FstCell=#tablecell{id=FstCellId,
				   body=[#link{body=split_label_into_rows(Alias),
									title = "Run query",
									postback={run_query,SafeQuery,Q}}],
						align="left",
						valign="middle",
						class=["query_list_tablecell", "white_border"]}
	end,
	FourthCellId=wf:temp_id(),
	FourthCell=#tablecell{id=FourthCellId,
						  body=[#link {text="E",
									class="delete_query_a", 
									title = "Assign a name to the query", 
									postback={alias_query, 
											  Alias, 
											  SafeQuery, 
											  FstCellId, 
											  FstCell,
											  FourthCellId}}],
							align="left",
							valign="middle",
							class=["query_list_tablecell","width_15", "white_border"]},
	
	{RowId,CommentEditorRow}=generate_comment_editor({sem_query,SafeQuery},Comment),
	FifthCellId=wf:temp_id(),
	FifthCell=#tablecell{id=FifthCellId,
						  body=[#link {text="C",
									class="delete_query_a", 
									title = "Edit comments of the query", 
									postback={show_comment_editor,
											  FifthCellId, RowId }}],
							align="left",
							valign="middle",
							class=["query_list_tablecell","width_15", "white_border"]},
	
	QInfo=if 
			(is_list(File) and is_integer(Pos)) -> 
				"Query string: "++SQ++"<br/>"++
				"File: "++File++"<br/> Starting Position: "++wf:to_list(Pos)++"<br/>";
			File==node ->
				"Query string: "++SQ++"<br/>"++
				"File: "++referl_htmlgen:getFile(Pos)++"<br/> Starting Node: "++nodeToText(Pos)++"<br/>";
			true -> "Query string: "++SQ++"<br/>"
		  end,
	CommentInfo=if (length(Comment)>0)->
				   "Comments: "++Comment++"<br/>";
			   true -> "Comments: -- <br/>"
			end,
	{SndCell, InfoRow}=generate_help(QInfo++CommentInfo),

	wf:insert_bottom(
			  query_list,
			  [#tablerow{cells=[FstCell, SndCell, ThrdCell,FourthCell, FifthCell]}, 
			   InfoRow, 
			   CommentEditorRow])
	end,
	Fun.

generate_comment_editor(ID={_Type, _Identifier},Comment)->
	TempId=wf:temp_id(),
	Cell=#tablecell{body=[
					 #textarea{id=TempId, text=Comment, style="width:100%"},
					 #button{text="Save Comment", 
							 postback={save_comment, ID, TempId},
							 class=[referl_button]}],
					align="left",
					valign="middle",
			        colspan="5",
					class=["query_list_tablecell", "white_border"]},
	RowId=wf:temp_id(),
	{RowId, #tablerow{id=RowId, cells=[Cell], style="display:none;"}}.

split_label_into_rows(Query) when length(Query)<?MAX_CHR_OF_LINE ->
	[Query];
split_label_into_rows(Query)->
	{List1,List2}=lists:split(?MAX_CHR_OF_LINE,Query),
	lists:flatten([List1]++["<br/>"]++split_label_into_rows(List2)).

html_from_result(Table) ->
	wf:session(result_helper,0),
	TableElements = to_table(Table),
	wf:session(currentjump_num,wf:session(result_helper)),
	wf:session(currentjump_cur,0),
	wf:replace(jumptable,jumptable()),
	wf:replace(jumpcounter,
		#tablecell{id=jumpcounter,body=[
						integer_to_list(0)
						++" of "
						++integer_to_list(wf:session(result_helper))],
				   colspan=2,class=[menuitem]}),
	if 
		TableElements/=[] ->
			wf:session(currentjump_nodes,undefined),
			wf:session(currentjump_num,wf:session(result_helper)),
			wf:session(currentjump_cur,0);
		true -> 
			wf:replace(jumpcounter,#tablecell{id=jumpcounter,body=["0 of 0"],colspan=2,class=[menuitem]}),
			wf:session(currentjump_nodes,undefined)
	end,

	case TableElements of
		[] ->[#tablerow{cells=[#tablecell{body=[#p{body="No result"}],
										  align="left",
										  valign="middle",
										  class="queries_result_tablecell"}]}];
		_ ->TableElements
	end.

to_table([]) -> [];

to_table([C={eq,_Text1,_Text2}]) ->
	[#tablerow{cells=[#tablecell{body=[#p{body=get_text(C)}],
								 align="left",
								 valign="middle",
								 class="queries_result_tablecell"}]}];

to_table([{list,L}]) ->
	to_table({list,L});

to_table({list,L}) ->
	case L of
		[] ->[#tablerow{cells=[#tablecell{body=[#p{body="No result"}],
										  align="left",
										  valign="middle",
										  class="queries_result_tablecell"}]}];
		_  -> [#tablerow{cells=[#tablecell{body=[to_html({list,L})],
										   align="left",
										   valign="middle",
										   class="queries_result_tablecell"}]}] 
	end;

to_table([{group_by, Entity, list, List}|Tail]) ->
	to_table([{group_by, Entity},{list, List}| Tail]);

to_table([{group_by, Entity, list, Type, List}|Tail]) ->
	to_table([{group_by, Entity},{list, Type, List}| Tail]);

to_table([{group_by, PrevType, Entity, list, Type, List}|Tail]) ->
	to_table([{group_by, PrevType, Entity},{list, Type, List}| Tail]);

to_table([{group_by, PrevType, Entity, eq, Type, PropVal}|Tail]) ->
	to_table([{group_by, PrevType, Entity}, {eq, Type, PropVal}|Tail]);

to_table([{group_by, Entity, eq, Type, PropVal}|Tail]) ->
	to_table([{group_by, Entity}, {eq, Type, PropVal}|Tail]);

to_table([{group_by,Row1},Row2|Tail]) ->
	[#tablerow{cells=[#tablecell{body=[to_html(Row1)],
								 align="left",
								 valign="middle",
								 class="queries_result_tablecell"},
					  #tablecell{body=[to_html(Row2)],
								 align="left",
								 valign="middle",
								 class="queries_result_tablecell"}]}
	|to_table(Tail)];

to_table([{chain,L,End}|Tail]) ->
	[#tablerow{cells=[#tablecell{body=[to_html({chainList,L}),
									   to_html({chain_end,End})],
								 align="left",
								 valign="middle",
								 class="queries_result_tablecell"}]}
	|to_table(Tail)];

to_table(R=[C|_]) when is_list(R) and is_integer(C)->
	[#tablerow{cells=[#tablecell{body=[#p{body=R}],
								 align="left",
								 valign="middle",
								 class="queries_result_tablecell"}]}];

to_table(R) ->
    [#tablerow{cells=[#tablecell{body=[#p{body=io_lib:format("~p",[R])}],
								 align="left",
								 valign="middle",
								 class="queries_result_tablecell"}]}].

to_html({list,[]}) -> [];
to_html({list,[H]}) -> to_html(H);
to_html({list,[H|T]}) -> [to_html(H),"<br/>",to_html({list,T})];

to_html({chainList,[]}) -> [];
to_html({chainList,[H]}) -> to_html({H,{comma,no}});
to_html({chainList,[H|T]}) -> [to_html({H,{comma,yes}}),to_html({chainList,T})];

to_html({chain_end,"\n"}) -> [];
to_html({chain_end,"*\n"}) -> "*";

to_html({X,{comma,IsComma}}) ->
	Separator = case IsComma of
					yes -> ", ";
					no -> ""
				end,
	case get_pos(X) of
		nopos -> #p{ body=get_text(X) ++ Separator};
		{File,StartPos,EndPos} ->
			wf:session(result_helper,wf:session(result_helper)+1),
			#link {id="query_result"++integer_to_list(wf:session(result_helper)),
				   text=get_text(X)++Separator, 
				   title = "Show", 
				   postback={show_selection, File, StartPos, EndPos, wf:session(result_helper)}}
	end;

to_html(X) ->
	case get_pos(X) of
		nopos ->#p{ body=get_text(X)};
		result ->
			wf:session(result_helper,wf:session(result_helper)+1),
			#link {id="query_result"++integer_to_list(wf:session(result_helper)),
				   text=get_text(X), 
				   title = "Show"};
		{node,node,Node} ->
			wf:session(result_helper,wf:session(result_helper)+1),
			%LineText=try 
			%	refcore_esg:data(Node),
			%	Tokens=refcore_syntax:leaves(Node),
			%	File=hd(reflib_query:exec(reflib_file:find(referl_htmlgen:getFile(Node)))),
			%	io:format("~p~p",[File,hd(Tokens)]),
			%	{{Line,_},{_,_}}=reflib_token:linecol(File,hd(Tokens)),
			%	" (line "++integer_to_list(Line)++")"
			%catch
			%  _:_ -> ""
			%end,			
			#link {id="query_result"++integer_to_list(wf:session(result_helper)),
				   text=get_text(X),%++LineText, 
				   title = "Show", 
				   postback={query_jumptonode, Node, wf:session(result_helper)}};
		{File,StartPos,EndPos} ->
			{Line,_}=reftr_wrangler:pos2tlc(File,StartPos),
			LineText=" (line "++integer_to_list(Line)++")",
			wf:session(result_helper,wf:session(result_helper)+1),
			#link {id="query_result"++integer_to_list(wf:session(result_helper)),
				   text=get_text(X)++LineText, 
				   title = "Show", 
				   postback={show_selection, File, StartPos, EndPos, wf:session(result_helper)}}
	end.



get_text({group_by,{_Pos,Text}}) when is_list(Text)-> Text;
get_text({group_by,{_Pos,Text}}) -> io_lib:format("~p",[Text]);

get_text({_Pos,Text}) when is_list(Text)-> Text;
get_text({_Pos,Text}) -> io_lib:format("~p",[Text]);

get_text({eq,Text1,Text2}) when is_list(Text1) and is_list(Text2)-> 
	Text1 ++ " = " ++ Text2;
get_text({eq,Text1,Text2}) ->
	io_lib:format("~p",[Text1]) ++ " = " ++ io_lib:format("~p",[Text2]);
get_text(_) -> "notext".

get_pos({group_by,{{File,StartPos,EndPos},_Text}}) ->
    get_pos({{File,StartPos,EndPos},_Text});
get_pos({{File,StartPos,EndPos},_Text}) ->
    {File, StartPos, EndPos};
get_pos(_) -> nopos.

groupFiles([]) -> [["No Result"]];
groupFiles([{Data,File}|ES]) ->
	groupFiles(ES,[File,Data],[],File).

groupFiles([],InnerSum,Sum,_) -> Sum++[InnerSum];
groupFiles([{Data,File}|ES],InnerSum,Sum,LastFile) when File==LastFile ->
	groupFiles(ES,InnerSum++[Data],Sum,File);
groupFiles([{Data,File}|ES],InnerSum,Sum,_) ->
	groupFiles(ES,[File,Data],Sum++[InnerSum],File).

