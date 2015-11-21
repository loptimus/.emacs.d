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

-module (action_resize_textarea).
-compile (export_all).
-include_lib ("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 6460 $ ").


render_action(_Record = #resize_textarea{targetId=TargetId, maxWidth=MaxWidth}) ->
	TargetIdStr=io_lib:format("~p",[TargetId]),
	MaxWidthStr=io_lib:format("~p",[MaxWidth]),
    "var ta = obj('"++TargetIdStr++"')
	var lines = ta.value.split(String.fromCharCode(10));
    var width = "++MaxWidthStr++";
    var height = 1;
    for (var i = 0; i < lines.length; i++) {
        var linelength = lines[i].length;
        if (linelength >= width ) {
            height += Math.abs(Math.ceil(linelength / width));
        }
    }
    height += lines.length;
	ta.cols= width;
    ta.rows = height;".
