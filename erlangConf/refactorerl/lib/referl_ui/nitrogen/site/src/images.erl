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
%%% The Initial Developer of the  Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s  Lor�nd University are  Copyright 2009,
%%% E�tv�s Lor�nd University. All Rights Reserved.

%%% @author Viktoria Fordos <v@fviktoria.hu>

%% -*- mode: nitrogen -*-

-module (images).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 6466 $ ").


main() -> 
	case wf:q(image) of
		undefined-> ok;
		Name-> case filename:extension(Name) of
				   ".svg"->wf:content_type("image/svg+xml");
				   ".dot"->wf:content_type("application/octet-stream");
				   _ -> ok
			   end
	end,
    binary_to_list(image_data()).

event(_) -> ok.

image_data() -> 
	case wf:q(image) of
		undefined -> <<>>;
		Name -> case file:read_file(Name) of
					{ok, Binary} -> Binary;
					{error,_}-> <<>>
				end
	end.
