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
-module (action_highlight_source).
-compile (export_all).
-include_lib ("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 6460 $ ").

render_action(_Record = #highlight_source{targetId=TargetId, 
										  startPos=StartPos, 
										  endPos=EndPos}) ->
	TargetIdStr = io_lib:format("~p",[TargetId]),
	StartPosStr = io_lib:format("~p",[StartPos]),
	EndPosStr = io_lib:format("~p",[EndPos]),
	"var target = obj('"++TargetIdStr++"');
	var nrCharInComment =0;
	if (target.selectionStart === undefined) {   // Internet Explorer
        var inputRange = target.createTextRange ();
        inputRange.moveStart ('character', (" ++ StartPosStr ++ "+nrCharInComment));
        inputRange.collapse ();
        inputRange.moveEnd ('character', (" ++ EndPosStr ++ "+nrCharInComment));
        inputRange.select ();
    }
    else {    // Firefox, Opera, Google Chrome and Safari
        target.selectionStart = " ++ StartPosStr ++ "-1+nrCharInComment;
        if (target.value.length <= ("++EndPosStr++" + nrCharInComment)){
			target.selectionEnd = target.value.length;
		}else{
			target.selectionEnd = " ++ EndPosStr ++"+nrCharInComment;
		}
        target.focus();
    }
	".
