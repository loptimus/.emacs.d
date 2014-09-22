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

-module (errors).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 6562 $ ").


%%% ============================================================================
%%% HTML output

main() -> 
	nitrogen_lib:main("errors.html").

title() -> "RefactorErl Queries".

warnings_header()->
	case nitrogen_helper:get_possible_warning() of
		no_warning -> "<h2>No error!</h2>";
		Warning -> "<h2>"++Warning++"</h2>"
	end.

warnings()->
	Fun = fun() -> collect_warnings() end,
    wf:continue({continue, warning}, Fun, 60*60*1000),
	Cell=#tablecell{body=#panel{body="Collecting information, please wait.."},
					   style="padding:10px; vertical-align:top;text-align:center;"},
	#table{id=warnings_main_table,
		   rows=[#tablerow{cells=[Cell]}], 
		   class="warning_table"}.

logout()->
	nitrogen_lib:logout_link(wf:user()).


%%% ============================================================================
%%% Handlers for postback events	

event(logout) ->
	nitrogen_lib:logout(wf:user());

event({show_selection,File, StartPos, EndPos})->
	case nitrogen_helper:get_source(File) of
		{data, Data} ->
			wf:replace(show_sorce_panel,
					   #panel{id=show_sorce_panel,
							  body=[#textarea{id=source_code_textarea,
											  text=Data,
											  html_encode=false,
											  class="source_code_ta"}]}),
			wf:wire(#resize_textarea { targetId=source_code_textarea, 
									   maxWidth=80}),
			wf:wire(#highlight_source { targetId=source_code_textarea, 
										startPos=StartPos, 
										endPos=EndPos}),
			wf:wire(#script{script="if (/Firefox/.test(navigator.userAgent))"++
						   "{obj('source_code_textarea').readOnly=true;}"});
		{error, Error} ->
			wf:replace(show_sorce_panel,
					   #panel{id=query_result_source_code_placeholder,
							  body=[#p{id=error_in_load,
									   body=Error,
									   class="error"}]})
	end.

%%% ============================================================================
%%% Handlers for continous events	
continue({continue, warning}, timeout) ->
	Cell=#tablecell{body=#panel{body="Sorry, failed to collect information."},
					   style="padding:10px; vertical-align:top;"},
	wf:replace(warnings_main_table,
			   #table{rows=[#tablerow{cells=[Cell]}], class="warning_table"});

continue({continue, warning}, {error, Error}) ->
	Cell=#tablecell{body=#panel{body=Error},
					   style="padding:10px; vertical-align:top;"},
	wf:replace(warnings_main_table,
			   #table{rows=[#tablerow{cells=[Cell]}], class="warning_table"});

continue({continue, warning}, Warning) ->
	FstCell=#tablecell{body=#panel{id=warning_messages_placeholder,
								   body=warning_handler(Warning)},
					   style="padding:10px; vertical-align:top;"},
	SndCell=#tablecell{body=#panel{id=show_sorce_panel, body=""},
					   style="padding:10px; vertical-align:top;"},
	wf:replace(warnings_main_table,
	#table{rows=[#tablerow{cells=[FstCell, SndCell]}], class="warning_table"}).

%%% ============================================================================
%%% Hepler functions
collect_warnings()->
	nitrogen_helper:add_possible_warning().

warning_handler({_Message, Warning}) when is_list(Warning)->
	[#table{id=warning_table,
		   class="query_result_table",
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
							end, [], Warning)}];

warning_handler(_)->"".