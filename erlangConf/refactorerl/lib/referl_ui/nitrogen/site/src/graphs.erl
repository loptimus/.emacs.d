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

-module (graphs).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 8397 $ ").

-define(SQ_TAB, filename:join(mnesia:system_info(directory),"dep_files/saved_queries")).
-define(MOD_TAB, filename:join(mnesia:system_info(directory),"dep_files/mod1")).
-define(FUNC_TAB, filename:join(mnesia:system_info(directory),"dep_files/func1")).

%%% ============================================================================
%%% HTML output

main() ->
	wf:set(exclude_mod_list,undefined),
	wf:set(exclude_func_list,undefined),
	wf:set(leaves_mod_list,undefined),
	wf:set(leaves_func_list,undefined),
	nitrogen_lib:main("graphs.html").

title() -> "RefactorErl Queries".

logout()->
	nitrogen_lib:logout_link(wf:user()).

graph_control_panel()->
	LevelCell=#tablecell{body=[#label{text="Level:"},
							   #dropdown{id=graph_level,
										 options=[#option{ text="Function", 
														   value=func, 
														   selected=false},
												  #option{ text="Module", 
														   value=mod, 
														   selected=true }],
										 html_encode=true,
										 postback=type_changed}],
										 class="graph_controll_tablecell"},
	wf:set(graph_level,mod),
	TypeCell=#tablecell{body=[#label{text="Type:"},
							  #dropdown{id=graph_type,
										options=[#option{text="None", 
														 value=none, 
														 selected=true},
												 #option{ text="Whole graph", 
														  value=all, 
														  selected=false},
												 #option{ text="Cyclic sub-graph", 
														  value=cycles, 
														  selected=false}],
										html_encode=true}],
						class="graph_controll_tablecell"},
	SNodeModCell=#tablecell{body=[#label{text="Starting node (module):"},#table{rows=[#tablerow{cells=[
								  #tablecell{body=[
								  #textbox_autocomplete {id=starting_node_module, 
														 tag=mods}]},
								  #tablecell{body=
								  #button{id=add_exclude_mod_button,
									 text="Add module to exclude list",
									 postback=add_exclude_mod,
									 class=referl_button}},#tablecell{body=
 								  #button{id=add_leaves_mod_button,
									 text="Add module to no leaves list",
									 postback=add_leaves_mod,
									 class=referl_button}}]},
								  #tablerow{cells=[#tablecell{body=#checkbox{id=nootp_mod,text="Exclude OTP",checked=false}},
								  #tablecell{body=[#br{},
								  #label{text="Excluded modules:"},
								  #textarea{id=exclude_mod_list,class=[nodelist]}]},
								  #tablecell{body=[#br{},
								  #label{text="Excludes leaves of these modules:"},
								  #textarea{id=leaves_mod_list,class=[nodelist]}]}]}]}],
							class="graph_controll_tablecell"},

	SNodeFunCell=#tablecell{body=[#label{text="Starting node (function):"},#table{rows=[#tablerow{cells=[
								  #tablecell{body=[
								  #textbox_autocomplete {id=starting_node_function, 
														 tag=funs}]},
								  #tablecell{body=
								  #button{id=add_exclude_func_button,
									 text="Add function to exclude list",
									 postback=add_exclude_func,
									 class=referl_button}},#tablecell{body=
 								  #button{id=add_leaves_func_button,
									 text="Add function to no leaves list",
									 postback=add_leaves_func,
									 class=referl_button}}]},
								  #tablerow{cells=[#tablecell{body=#checkbox{id=nootp_func,text="Exclude OTP",checked=false}},
								  #tablecell{body=[#br{},
								  #label{text="Excluded functions:"},
								  #textarea{id=exclude_func_list,class=[nodelist]}]},
								  #tablecell{body=[#br{},
								  #label{text="Excludes leaves of these functions:"},
								  #textarea{id=leaves_func_list,class=[nodelist]}]}]}]}],
							class="graph_controll_tablecell"},
	BtnCell1=#tablecell{body=[#button{id=draw_graph,
									 text="Generate svg graph",
									 postback=generate_graph,
									 class=referl_button}],
					   class=["graph_controll_tablecell","centered"]},
	BtnCell2=#tablecell{body=[#button{id=draw_graph,
									 text="Print result",
									 postback=print_graph,
									 class=referl_button}],
					   class=["graph_controll_tablecell","centered"]},
	BtnCell3=#tablecell{body=[#button{id=draw_graph,
									 text="Generate smart graph (beta)",
									 postback=generate_test_graph,
									 class=referl_button}],
					   class=["graph_controll_tablecell","centered"]},	
	ControlTable=#table{id=graph_control_panel,
						rows=[#tablerow{cells=[LevelCell]},
							  #tablerow{cells=[TypeCell]},
							  #tablerow{id=gnode_mod,
										cells=[SNodeModCell]},
							  #tablerow{id=gnode_fun,
										cells=[SNodeFunCell]},
							  #tablerow{cells=[BtnCell1]},
							  #tablerow{cells=[BtnCell2]},
							  #tablerow{cells=[BtnCell3]}]},
	wf:wire(gnode_fun,#hide{}),
	%see custom_validator's definition
	wf:wire(draw_graph, graph_type, 
			#validate { validators=[#custom { text="Please, select type or starting node", 
											  tag=some_tag, 
											  function=fun custom_validator/2}]}),
	ControlTable.

functionblock_control_panel()->
	Dirs= nitrogen_helper:get_loaded_directories(),
	Items=gen_sortitem_list(Dirs),
	wf:state(fb_sources, Dirs),
	Source=fb_sources_sortblock(Items),
	Subject=#sortblock { class=["simple","fb_block"], group=subjects, 
				 connect_with_groups=directories, tag=subject, 
				 items=[]},
	wf:state(fb_subjects, []),
	HdRow=#tablerow{cells=[#tablecell{body="Functionblocks", 
									  class=["graph_controll_tablecell", "bold"]},
						   #tablecell{body="Subjects", 
									  class=["graph_controll_tablecell", "bold"]}]},
	RegexpContents=["Define by regexp: ",
					#textbox{id=user_regexp},
					#button{id=add_re, 
							text="Add", 
							postback=add_regexp, 
							class=[referl_button]}],
	RegexpRow=#tablerow{cells=[#tablecell{body=RegexpContents,
									  class="graph_controll_tablecell",
									  colspan=2,
									  align="left"}]},
	BdRow=#tablerow{cells=[#tablecell{body=Source, 
									  class="graph_controll_tablecell", 
									  valign="top"},
						   #tablecell{body=Subject, 
									  class="graph_controll_tablecell", 
									  valign="top"}]},
	CtrRow=#tablerow{cells=[#tablecell{body=#button{id=new_query_button, 
													text="Generate graph", 
													postback=generate_fb_graph, 
													class=[referl_button]}, 
									    class="graph_controll_tablecell", 
										colspan=2,
										align="center"}]},
	wf:wire(add_re, 
			user_regexp, 
			#validate { validators=[#is_required { text="Required." }]}),
	
	#table{rows=[RegexpRow, HdRow, BdRow, CtrRow], class="graph_controll_table"}.

result_panel()->
	#panel{id=result_panel, body=[]}.
	%#panel{id="gr", style="height:200px; width:200px", body="<canvas height='200px' width='200px' ></canvas>"}.
%%% ============================================================================
%%% autocomplete

autocomplete_enter_event(SearchTerm, mods) ->
	Mods=case nitrogen_helper:execute_system_query("mods", 
												   {query_display_opt,
													[],
													needed_pattern,none}) of
		{result, no_result} -> [];
		{result, ResultMods} -> ResultMods
	end,
	DataM=lists:map(fun(Elem)-> 
						   {struct, [{id, list_to_binary(Elem)}, 
									 {label, list_to_binary(Elem)} , 
									 {value, list_to_binary(filename:rootname(
															  Elem)) }]} 
					end,Mods),
	List = [{struct,[{id, Id }, {label, Label}, {value, Value}]} || 
			{struct,[{id, Id }, {label, Label}, {value, Value}]} <- DataM, 
			string:str(string:to_lower(binary_to_list(Label)), 
					   string:to_lower(SearchTerm)) > 0],
	mochijson2:encode(List);

autocomplete_enter_event(SearchTerm, funs) ->
	Funs=case nitrogen_helper:execute_system_query("mods.funs", 
												   {query_display_opt,
													[],
													needed_pattern,$:}) of
		{result, no_result} -> [];
		{result, ResultFuns} -> ResultFuns
	end,	
	DataF=lists:map(fun(Elem)-> 
						   {struct, [{id, list_to_binary(Elem)}, 
									 {label, list_to_binary(Elem)} , 
									 {value, list_to_binary(Elem) }]} 
					end,Funs),
    List = [{struct,[{id, Id }, {label, Label}, {value, Value}]} || 
			{struct,[{id, Id }, {label, Label}, {value, Value}]} <- DataF, 
			string:str(string:to_lower(binary_to_list(Label)), 
					   string:to_lower(SearchTerm)) > 0],
	mochijson2:encode(List).

autocomplete_select_event({struct,[{<<"id">>,_},{<<"value">>,Value}]},_Tag)->
	wf:flash(Value),
	ok.

%%% ============================================================================
%%% validators

%Only postback the form, when at least one of those three is selected.
custom_validator(_Tag, _Value) ->
	case {wf:q(graph_type),
		  wf:q(starting_node_module), 
		  wf:q(starting_node_function)} of
		{"none", undefined, undefined} -> false;
		_ ->true
	end.

%%% ============================================================================
%%% Handlers for postback events
event(type_changed)->
	case wf:q(graph_level) of
		"func" -> 
			wf:wire(gnode_mod,#hide{}),
			wf:wire(gnode_fun,#show{});
		"mod" ->
			wf:wire(gnode_mod,#show{}),
			wf:wire(gnode_fun,#hide{})
	end;

event({add_to_list,From,To}) ->
	Text=getText(From),
	List=wf:q(To),
	if
		(Text/="") and ((List==undefined) or (List=="")) -> wf:set(To,Text);
		Text/="" -> wf:set(To,List++"\n"++Text);
		true -> ok
	end;

event(add_exclude_mod) ->
	event({add_to_list,starting_node_module,exclude_mod_list});
	
event(add_exclude_func) ->
	event({add_to_list,starting_node_function,exclude_func_list});

event(add_leaves_mod) ->
	event({add_to_list,starting_node_module,leaves_mod_list});
	
event(add_leaves_func) ->
	event({add_to_list,starting_node_function,leaves_func_list});

event(add_regexp)->
	Regexp=wf:q(user_regexp),
	List=wf:state(fb_sources)++[Regexp]--wf:state(fb_subjects),
	UniqueItems=lists:usort(List),
	wf:state(fb_sources, UniqueItems),
	Items=gen_sortitem_list(UniqueItems),
	wf:replace(src_sortblock, fb_sources_sortblock(Items));

event(print_graph) ->
	{Parameters,Error}=getParameters(),
	Fun=case Error of
		{error,E} -> fun() -> {error,E} end;
		_ -> fun() -> print_dep_graph(Parameters) end
	end,
	wf:continue({continue, dep_graph_print}, Fun, 60*60*1000);

event(generate_graph)->
	{Parameters,Error}=getParameters(),
	Fun=case Error of
		{error,E} -> fun() -> {error,E} end;
		_ -> fun() -> gen_dep_graph(Parameters) end
	end,
	wf:continue({continue, dep_graph}, Fun, 60*60*1000);

event(generate_test_graph)->
	{Parameters,Error}=getParameters(),
	Fun=case Error of
		{error,E} -> fun() -> {error,E} end;
		_ -> fun() -> draw_test_graph(Parameters) end
	end,
	wf:continue({continue, draw_graph}, Fun, 60*60*1000);

event(generate_fb_graph)->
	case wf:state(fb_subjects) of
		FBs when is_list(FBs) andalso length(FBs)>0 -> gen_fb_graph(FBs);
		_ -> create_message("Subject is empty!", error)
	end;

event(logout) ->
	nitrogen_lib:logout(wf:user()).
%%% ============================================================================
%%% Handlers for continous events	
continue({continue, _}, timeout) ->
	create_message("The execution of the analysis has been exceeded the timeout limit.", 
				   error);

continue({continue, dep_graph_print}, Result) ->
	case Result of
		{error, Reason} -> create_message(Reason, error);
		{Res,Type} -> 
			{Cycles,Items}=Res,
			FormattedItems=lists:map(fun(X) -> io_lib:format("<br />~p",[lists:map(fun(Y) -> translateNode(Y) end,X)]) end,Items),
			case Type of
				mod -> TabName=?MOD_TAB;
				_ -> TabName=?FUNC_TAB
			end,
			{_, Ref}=dets:open_file(TabName),
			RelationsResult=dets:match(Ref,'$1'),
			dets:close(TabName),
			FormattedResult=lists:map(fun(E) -> {_,_,Node,Rels,_}=hd(E),io_lib:format("<br />~s -> ~p",[translateNode(Node),lists:map(fun(X) -> translateNode(X) end,Rels)]) end,RelationsResult),
			create_message(io_lib:format("<strong>Relations</strong>:~s<br /><br /><strong>~s</strong>:~s",[FormattedResult,Cycles,FormattedItems]), information)
	end;

continue({continue, draw_graph}, {Result,DotName}) ->
	case Result of
		{error, Reason} -> create_message(Reason, error);
		_ ->
			ResultFile=filename:join(filename:dirname(DotName),wf:user()++".txt"),
			os:cmd("dot -Tplain-ext -Gnodesep=0.001 "++DotName++" -o"++ResultFile),
			{X, Binary} = file:read_file(ResultFile),
			GraphData=if
				X==error -> [];
				true -> binary_to_list(Binary)
			end,
			Lines=string:tokens(GraphData,"\n"),
			{_,_,W,H}=list_to_tuple(string:tokens(hd(Lines)," ")),
			Width=list_to_num(W),
			Height=list_to_num(H),
			Nodes=[parseGraphNodeLine(E,Width,Height) || E<-Lines, hd(E)==$n],
			Edges=[parseGraphEdgeLine(E) || E<-Lines, hd(E)==$e],
			Script=generateGraphScript(Nodes,Edges),
			wf:session(javagraph,Script),
			wf:replace(result_panel,create_message_table(#link{text="See generated graph", 
		    	title = "See result", 
		    	url="javascript:newWindow('graphresult')"})),
			wf:wire(result_panel,#hide{}),
			wf:wire(result_panel,#appear{})
	end;

continue({continue, _}, Result) ->
	%{_, Ref}=dets:open_file(?SQ_TAB),
	%RelationsResult=dets:match(Ref,'$1'),
	%io:format("~p~n",[RelationsResult]),
	case Result of
		{error, Reason} -> create_message(Reason, error);
		_ -> create_message(Result, result)
	end.
%%% ============================================================================
%%% Handlers for sort events	
sort_event(subject, Items) -> 
    wf:state(fb_subjects,Items),
	ok;

sort_event(source,Items) ->
	wf:state(fb_sources, Items),
	ok.
%%% ============================================================================
%%% Hepler functions

parseGraphNodeLine(E,Width,Height) ->
	L=string:tokens(E," "),
	{lists:nth(2,L),lists:nth(7,L),list_to_num(lists:nth(3,L))/Width,(Height-list_to_num(lists:nth(4,L)))/Height}.

parseGraphEdgeLine(E) ->
	L=string:tokens(E," "),
	{lists:nth(2,L),lists:nth(3,L),lists:last(L)}.

list_to_num(L) ->
    case string:to_float(L) of
        {error,no_float} -> list_to_integer(L);
        {F,_} -> F
    end.

num_to_list(N) ->
	try
		float_to_list(N)
	catch
		_:_ -> integer_to_list(N)
    end.

generateGraphScript(Nodes,Edges) ->
	NodesScript=lists:foldr(fun(E,Text) -> 
		{Name,Label,X,Y}=E, 
		"g.addNode('"++Name++"',{
		x: "++num_to_list(X*3)++", 
		y: "++num_to_list(Y)++",
		size:'"++getNodeSize(Name)++"',
		label: '"++Label++"',
		color: '"++getNodeColor(Name)++"'});"++Text end,"",Nodes),
	EdgesScript=lists:foldr(fun(E,Text) -> 
		{From,To,Color}=E, 
		"g.addEdge('"++From++To++"','"++From++"','"
		++To++"',{color: '"++Color++"'});"++Text end,"",Edges),
	
"g = sigma.init(document.getElementById('gr'));

g.drawingProperties({
  defaultLabelColor: '#000',
  font: 'Arial',
  edgeColor: 'source',
});

g.bind('overnodes',function(event){
var nodes = event.content;
var neighbors = {};
g.iterEdges(function(e){
if(nodes.indexOf(e.source)<0 && nodes.indexOf(e.target)<0){
e.hidden=1;
}else{
neighbors[e.source] = 1;
neighbors[e.target] = 1;
}
}).iterNodes(function(n){
if(!neighbors[n.id]){
n.hidden = 1;
}else{
n.hidden = 0;
}
}).draw(2,2,2);
}).bind('outnodes',function(){
g.iterEdges(function(e){
e.hidden = 0;
}).iterNodes(function(n){
n.hidden = 0;
}).draw(2,2,2);
});"
++NodesScript++EdgesScript++"g.draw();".

getNodeName({_,module,Id}) -> "m"++integer_to_list(Id);
getNodeName({_,func,Id}) -> "f"++integer_to_list(Id).

getNodeSize([$m|_]) -> "6";
getNodeSize([$f|_]) -> "4";
getNodeSize(_) -> "10".

getNodeColor([$m|_]) -> "#000000";
getNodeColor([$f|_]) -> "#666666";
getNodeColor(_) -> "#000000".

translateNode({_,module,_}=Node) ->
	atom_to_list(reflib_module:name(Node));

translateNode({_,func,_}=Node) -> 
	atom_to_list(reflib_module:name(reflib_query:exec1(Node,reflib_function:module(),error)))
						++":"++atom_to_list(reflib_function:name(Node))
						++"/"++integer_to_list(reflib_function:arity(Node)).

getParameters() ->
	%feedback
	create_message("Running analysis, please wait..", information),
	%start async
	Level=wf:q(graph_level),
	Type=wf:q(graph_type),
	GnodeM=wf:q(starting_node_module),
	GnodeF=wf:q(starting_node_function),
	Gnode=case Level of
			  "func"->funs;
			  "mod"-> mods;
			  _ ->    none
		  end,
	{GnodeMTuple,ExcludeMods,LeavesMods,Error}=try
	{begin
		StartingModname=try
			list_to_existing_atom(filename:basename(GnodeM,".erl"))
		catch _:_ -> throw({error,"Error: The following module doesn't exist: "++ GnodeM}) end,
		case StartingModname of
			'' -> {gnodenone,none};
			_ -> {gnode,StartingModname}
		end 
	 end,
	lists:map(fun(X) ->
		try
			list_to_existing_atom(filename:basename(X,".erl")) 
		catch _:_ -> throw({error,"Error: The following module doesn't exist: "++ X}) end
						  end,
			string:tokens(getText(exclude_mod_list),"\n")),
	lists:map(fun(X) -> 
		try 
			list_to_existing_atom(filename:basename(X,".erl")) 
		catch _:_ -> throw({error,"Error: The following module doesn't exist: "++ X}) end
						  end,
			string:tokens(getText(leaves_mod_list),"\n")),ok}
	catch {error,E} -> {[],[],{gnodenone,none},{error,E}} end,	

	ExcludeFuns=string:tokens(getText(exclude_func_list),"\n"),
	LeavesFuns=string:tokens(getText(leaves_func_list),"\n"),

	case wf:q(nootp_mod) of
		"on" -> OtpMod=true;
		_ -> OtpMod=false
	end,
	case wf:q(nootp_func) of
		"on" -> OtpFunc=true;
		_ -> OtpFunc=false
	end,

	GnodeFTuple=case GnodeF of
		[] -> {gnodenone,none};
		_ -> {gnode,[GnodeF]}
	end,


	Options=case Gnode of
		none -> [{level,list_to_existing_atom(Level)}, 
				 {type, list_to_existing_atom(Type)}];
		mods ->[{level,list_to_existing_atom(Level)},
				{type, list_to_existing_atom(Type)},
				GnodeMTuple,
				{exception,ExcludeMods},
				{leaves,LeavesMods},
				{otp,OtpMod}];
		funs -> [{level,list_to_existing_atom(Level)},
				{type, list_to_existing_atom(Type)},
				GnodeFTuple,
				{exception,ExcludeFuns},
				{leaves,LeavesFuns},
				{otp,OtpFunc}]
	end,
	if 
		Gnode==mods -> {{Options,wf:user()},Error};
		true -> {{Options,wf:user()},ok}
	end.

getText(Id) ->
	Result=wf:q(Id),
	if
		Result==undefined -> "";
		true -> Result
	end.

gen_dep_graph({Options, User})->
	nitrogen_helper:generate_dependency_graph(Options, User).

print_dep_graph({Options, User})->
	nitrogen_helper:print_dependency_graph(Options, User).

draw_test_graph({Options, User})->
	FileName=User++".dot",
    TargetDir=nitrogen_helper:get_images_root(),
    DotName=filename:join([TargetDir,FileName]),
    XOptions=Options++[{dot, DotName}],
	{nitrogen_helper:print_dependency_graph(XOptions, User),DotName}.

start_gen_fb_graph({Parameters, User})->
	nitrogen_helper:generate_fb_graph(Parameters, User).

gen_fb_graph(Subjects)->
	%feedback
	create_message("Running analysis, please wait..", information),
	%start async
	Parameters={Subjects, wf:user()},
	Fun = fun() -> start_gen_fb_graph(Parameters) end,
	wf:continue({continue, fb_graph}, Fun, 60*60*1000).

create_message(Content, Type)->
	Message=case Type of
				result -> create_result_message(Content);
				information -> create_information_message(Content);
				_ -> create_error_message(Content)
			end,
	wf:replace(result_panel,create_message_table(Message)),
	wf:wire(result_panel,#hide{}),
	wf:wire(result_panel,#appear{}).

create_message_table(Message)->
	Cell=#tablecell{body=Message, class=["graph_controll_tablecell", "red_border"]},
	#table{id=result_panel, rows=[#tablerow{cells=[Cell]}], class="query_result_table"}.

create_information_message(Text)->
	[#p{body=wf:to_list(Text)}].

create_error_message(Text)->
	[#p{body="Error: "++wf:to_list(Text), class="error"}].

create_result_message({DotName, SvgName})->
	[#link{text="Generated graph in .dot", 
		   title = "Download result", 
		   url="images?image="++DotName,
		   style="margin:15px; padding:5px;" },
	 #br{},
	 #br{},
	 #link{text="See generated graph in .svg", 
		   title = "See result in new window", 
		   url="javascript:newWindow('images?image="++SvgName++"');", 
		   style="margin:15px; padding:5px;" }].

fb_sources_sortblock(Items)->
	#sortblock { id=src_sortblock, class=["simple","fb_block"], group=directories, 
				 connect_with_groups=subjects, tag=source, 
				 items=Items}.

gen_sortitem_list(Items)->
	[#sortitem { tag=Item, body=Item, class="fb_block_item" } || Item <- Items, Item/=[]].
