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
-module (element_textarea_autocomplete).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 6460 $ ").

reflect() -> record_info(fields, textarea_autocomplete).

render_element(Record) ->
	% Get properties...
	Delegate = Record#textarea_autocomplete.delegate,
	Tag = Record#textarea_autocomplete.tag,
	Anchor = Record#textarea_autocomplete.anchor,
	AutoCompleteMinLength = Record#textarea_autocomplete.minLength,
	AutoCompleteDelay = Record#textarea_autocomplete.delay,
	
	% Write out the script to make this element autocompletable...
	AutoCompleteEnterPostbackInfo = wf_event:serialize_event_context(
									  {autocomplete_enter_event, Delegate, Tag}, 
									  Anchor, 
									  undefined, 
									  ?MODULE),
	AutoCompleteSelectPostbackInfo = wf_event:serialize_event_context(
									   {autocomplete_select_event,Delegate,Tag},
									   Anchor, 
									   undefined, 
									   ?MODULE ),
	
	AutoCompleteOptions = {struct, [
		{dataType, <<"json">>},
		{minLength, AutoCompleteMinLength},
		{delay, AutoCompleteDelay}]},
	
	AutoCompleteScript = #script {
		script = wf:f("Nitrogen.$autocomplete('~s', ~s, '~s', '~s');", [
		  Anchor,
		  mochijson2:encode(AutoCompleteOptions),
		  AutoCompleteEnterPostbackInfo, 
		  AutoCompleteSelectPostbackInfo])},
	
	wf:wire(AutoCompleteScript),
	
	% Render as a textarea.
	Value = wf:html_encode(Record#textarea_autocomplete.text, 
						   Record#textarea_autocomplete.html_encode),
	wf_tags:emit_tag(textarea, 
					 Value, [{class, 
							  [textarea_autocomplete, 
							   Record#textarea_autocomplete.class]},
							 {style, Record#textarea_autocomplete.style}]).

event({autocomplete_select_event, Delegate, SelectTag})->
	SelectItem = mochijson2:decode(wf:q(select_item)),
	Module = wf:coalesce([Delegate, wf:page_module()]),
	Module:autocomplete_select_event(SelectItem, SelectTag);

event({autocomplete_enter_event, Delegate, EnterTag})->
	SearchTerm = wf:q(search_term),
	wf_context:type(first_request),
	wf:content_type("application/json"),
	Module = wf:coalesce([Delegate, wf:page_module()]),
	wf_context:data([Module:autocomplete_enter_event(SearchTerm, EnterTag)]).

