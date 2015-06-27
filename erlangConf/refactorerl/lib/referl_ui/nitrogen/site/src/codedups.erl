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

-module (codedups).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-include("records.hrl").
-vsn("$Rev: 8408 $ ").

-define(MAX_CHR_OF_LINE,25).

%%% ============================================================================
%%% HTML output

main() -> 
	nitrogen_lib:main("codedups.html").

title() -> "RefactorErl Queries".

menu() ->
	FileLabel=#label{text="Filepath or regexp:"},
	FileText=#textbox_autocomplete {id=filetextbox,style="width:190px",tag=mods},
    FileAddButton=#button{id=filebutton,
						  text="Add to list",
						  postback=add_file,
						  class=referl_button},
	FileareaLabel=#label{text="Files to run analysis on:"},
	FileTextarea=#textarea{style="height:80px; width:100%",id=file_list},
	Param1Label=#label{text="Min length:"},
	Param1=#textbox{style="width:30px",id=param1},
	Param2Label=#label{text="Min number of dups:"},
	Param2=#textbox{style="width:30px",id=param2},
	Param3Label=#label{text="Overlap amount:"},
	Param3=#textbox{style="width:30px",id=param3},
	wf:set(param1,"10"),
	wf:set(param2,"2"),
	wf:set(param3,"0"),
	RunDCAnalBtn=#button{id=run_dupcodeanal, 
				text="Run duplicate code\nanalysis", 
				postback=run_dupcodeanal, 
				class=[referl_button]},
	ResultPanel=#panel{id=dupcode_result},

	#table{rows=[
		#tablerow{cells=#tablecell{colspan=2,body=FileLabel}},
		#tablerow{cells=#tablecell{colspan=2,body=FileText}},
		#tablerow{cells=#tablecell{colspan=2,body=FileAddButton}},
		#tablerow{cells=#tablecell{colspan=2,body=#br{}}},
		#tablerow{cells=#tablecell{colspan=2,body=FileareaLabel}},
		#tablerow{cells=#tablecell{colspan=2,body=FileTextarea}},
		#tablerow{cells=[#tablecell{body=Param1Label},#tablecell{body=Param1}]},
		#tablerow{cells=[#tablecell{body=Param2Label},#tablecell{body=Param2}]},
		#tablerow{cells=[#tablecell{body=Param3Label},#tablecell{body=Param3}]},
		#tablerow{cells=#tablecell{colspan=2,body=#br{}}},
		#tablerow{cells=#tablecell{colspan=2,body=RunDCAnalBtn}},
		#tablerow{cells=#tablecell{colspan=2,body=#br{}}},
		#tablerow{cells=#tablecell{colspan=2,body=ResultPanel}}
				]}.

sourcecode_browsers() ->
	ResultList1=#dropdown{id=resultlist1},
	DiffButton=#hidden{id=diff_button},
	ResultList2=#dropdown{id=resultlist2},
	SourceViewer1=[#tablecell{body=#panel{id=linenums1,body=[],class=["lnums"]}},
				   #tablecell{body=#panel{id=source1}}],
	SourceViewer2=[#tablecell{body=#panel{id=linenums2,body=[],class=["lnums"]}},
				   #tablecell{body=#panel{id=source2}}],
	Filename1=#tablecell{body=#panel{id=filename1,body=[],class=centered},colspan=2},
	Filename2=#tablecell{body=#panel{id=filename2,body=[],class=centered},colspan=2},
	Panel1=#panel{id=scrollable1,class=["dupcode_column1"],body=SourceViewer1},
	Panel2=#panel{id=scrollable2,class=["dupcode_column2"],body=SourceViewer2},
	[#table{class=[fullwidth],rows=[#tablerow{cells=[
				#tablecell{class=[quarterwidth],body=ResultList1},
				#tablecell{class=[quarterwidth,alignright],body=DiffButton},
				#tablecell{class=[quarterwidth],body=ResultList2},
				#tablecell{class=[quarterwidth],body="&nbsp;"}]},#tablerow{cells=[Filename1,Filename2]}]},Panel1,Panel2].
	
logout() ->
	nitrogen_lib:logout_link(wf:user()).

%%% ============================================================================
%%% Handlers for postback events	
event(logout) ->
	nitrogen_lib:logout(wf:user());

event({add_to_list,From,To}) ->
	Text=getText(From),
	List=wf:q(To),
	if
		(Text/="") and ((List==undefined) or (List=="")) -> wf:set(To,Text);
		Text/="" -> wf:set(To,List++"\n"++Text);
		true -> ok
	end;

event(add_file) ->
	event({add_to_list,filetextbox,file_list});

event(run_dupcodeanal) ->
	wf:wire(run_dupcodeanal,#hide{}),
	wf:replace(dupcode_result,#panel{id=dupcode_result,body=["Running analysis, please wait.."]}),
	FileList=string:tokens(getText(file_list),"\n"),
	Fun=try
		P1=list_to_integer(getText(param1)),
		P2=list_to_integer(getText(param2)),
		P3=list_to_integer(getText(param3)),
		fun() -> {_,Dups} = refusr_dupcode:search_initial_clones([{files,FileList},{minlen,P1},{minnum,P2},{overlap,P3}]), Dups end
	catch
		_:_ -> fun() -> {error,"Bad parameters."} end
	end,
	wf:continue({continue, dupcodeanal}, Fun, 60*60*1000);

event({show_diff,Nodes1,Nodes2}) ->
	wf:wire(#script{script="window.getSelection().removeAllRanges();"}),
	{CDiffs,VDiffs}=refusr_dupcode:const_var_diff(Nodes1,Nodes2),
	lists:map(fun({N1,N2}) -> showNode(N1,1),showNode(N2,2) end,CDiffs),
	lists:map(fun({N1,N2}) -> showNode(N1,1),showNode(N2,2) end,VDiffs);

event({selected_group,Items}) ->
	ItemList=parse_items(Items,1),
	wf:replace(resultlist1,#dropdown{id=resultlist1,
								   options=ItemList,
								   html_encode=true,
								   postback={load_source,1}
								  }),
	wf:replace(resultlist2,#dropdown{id=resultlist2,
								   options=ItemList,
								   html_encode=true,
								   postback={load_source,2}
								  }),
	wf:set(resultlist1,convert_position_to_string(hd(Items))),
	wf:set(resultlist2,convert_position_to_string(hd(tl(Items)))),
	wf:continue({continue, showfirstcodes}, fun() -> ok end, 60*60*1000);

event({load_source,Tab})->
	event({load_source,Tab,wf:q("resultlist"++integer_to_list(Tab))});

event({load_source,Tab,String})->
	L=parseString(String),
	File=proplists:get_value(filepath,L),
	{SL,SC}=proplists:get_value(startpos,L),
	{EL,EC}=proplists:get_value(endpos,L),
	case Tab of
		1 -> wf:replace(diff_button,#button{id=diff_button,
						  text="Show differences",
						  postback={show_diff,proplists:get_value(nodes,L),proplists:get_value(nodes,parseString(wf:q("resultlist2")))},
						  class=referl_button_small});
		2 -> wf:replace(diff_button,#button{id=diff_button,
						  text="Show differences",
						  postback={show_diff,proplists:get_value(nodes,parseString(wf:q("resultlist1"))),proplists:get_value(nodes,L)},
						  class=referl_button_small})
	end,
	referl_htmlserver:generate(File),
	Fun=fun() -> referl_htmlserver:getdata(File) end,
	wf:replace("linenums"++integer_to_list(Tab),#panel{id="linenums"++integer_to_list(Tab),body=[],class=["lnums"]}),
	wf:replace("filename"++integer_to_list(Tab),#panel{id="filename"++integer_to_list(Tab),body=File,class=centered}),
	wf:session("filename"++integer_to_list(Tab),File),
	TabName="source"++integer_to_list(Tab),
	wf:replace(TabName,#panel{id=TabName,body="Loading file, please wait..."}),
	wf:wire(TabName,#hide{}),
	wf:wire(TabName,#appear{}),
	wf:continue({continue, generate, Tab, {SL,SC,EL,EC}}, Fun, 60*60*1000).

%%% ============================================================================
%%% Handlers for continous events	

continue({continue, generate, Tab, {SL,SC,EL,EC}}, Text) ->
	TabName="source"++integer_to_list(Tab),
	wf:replace(TabName,#panel{id=TabName,body=Text,class=[monosp]}),
	wf:wire(#script{script="generatelinenums(obj('"++TabName++"'),obj('linenums"++integer_to_list(Tab)++"'));"}),
	showResult({SL,SC,EL,EC},Tab);

continue({continue, showfirstcodes}, ok) ->
	event({load_source,1}),
	event({load_source,2});

continue({continue, dupcodeanal}, {error,E}) ->
	wf:wire(run_dupcodeanal,#show{}),
	wf:replace(dupcode_result,#panel{id=dupcode_result,body=[E]});

continue({continue, dupcodeanal}, []) ->
	wf:wire(run_dupcodeanal,#show{}),
	wf:replace(dupcode_result,#panel{id=dupcode_result,body="No result."});

continue({continue, dupcodeanal}, Result) ->
	wf:wire(run_dupcodeanal,#show{}),
	wf:replace(dupcode_result,#panel{id=dupcode_result,body=[["Result:",#br{}]++parse_result(Result,1)]}).

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
									 {value, list_to_binary(
										referl_htmlgen:getFile(
											list_to_existing_atom(
												filename:rootname(Elem)))) }]} 
					end,Mods),
	List = [{struct,[{id, Id }, {label, Label}, {value, Value}]} || 
			{struct,[{id, Id }, {label, Label}, {value, Value}]} <- DataM, 
			string:str(string:to_lower(binary_to_list(Label)), 
					   string:to_lower(SearchTerm)) > 0],
	mochijson2:encode(List).

autocomplete_select_event({struct,[{<<"id">>,_},{<<"value">>,Value}]},_Tag)->
	wf:flash(Value),
	ok.

%%% ============================================================================
%%% Helper functions
showResult({StartLn,StartCol,EndLn,EndCol},Tab) ->
	wf:wire(#script{script="
		highlight(obj('"++"source"++integer_to_list(Tab)++"'),
		obj('"++"scrollable"++integer_to_list(Tab)++"'),
		"++integer_to_list(StartLn)++","++integer_to_list(StartCol)++",
		"++integer_to_list(EndLn)++","++integer_to_list(EndCol+1)++");"}).

showNode(Node,Tab) ->
	Tokens=refcore_syntax:leaves(Node),
	File=hd(reflib_query:exec(reflib_file:find(wf:session("filename"++integer_to_list(Tab))))),
	{Start,_}=reflib_token:pos(File,hd(Tokens)),
	{_,End}=reflib_token:pos(File,lists:last(Tokens)),
	wf:wire(#script{script="
	try{
		highlight2(obj('"++"source"++integer_to_list(Tab)++"'),
				  obj('"++"scrollable"++integer_to_list(Tab)++"'),"
		++integer_to_list(Start-1)++","++integer_to_list(End)++");
	}catch(ex){}"}).

parseString(S) ->
	{ok,Scanned,_}=erl_scan:string(S++"."),
    {ok,Parsed}=erl_parse:parse_exprs(Scanned),
    {value,L,_}=erl_eval:exprs(Parsed,[]),
	L.

getText(Id) ->
	Result=wf:q(Id),
	if
		Result==undefined -> "";
		true -> Result
	end.

parse_result([],_) -> [];
parse_result([L|LS],Num) ->
	[#link{ id="dc_group"++integer_to_list(Num),
			body="Duplicate code group "++integer_to_list(Num),
			title="Browse group",
			postback={selected_group,L}},#br{}]
	++parse_result(LS,Num+1).

parse_items([],_) -> [];
parse_items([L|LS],Num) ->
	[#option{ text="Code "++integer_to_list(Num), 
			  value=convert_position_to_string(L),
			  selected=false}]
	++parse_items(LS,Num+1).


convert_position_to_string(L) ->
	lists:flatten(io_lib:format("~p", [L])).

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
