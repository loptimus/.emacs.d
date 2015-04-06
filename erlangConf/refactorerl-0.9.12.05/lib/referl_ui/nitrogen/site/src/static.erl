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

-module (static).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 6466 $ ").


main() -> 
	case wf:q(file) of
		undefined-> <<>>;
		Name-> File=filename:join([nitrogen_helper:get_nitrogen_index(),Name]),
			   case filename:extension(File) of
				   ".js"->wf:content_type("application/javascript");
				   ".css"->wf:content_type("text/css");
				   _ -> ok
			   end,
			   wf_context:header("Expires", expire()),
			   binary_to_list(content(File))
	end.

event(_) -> ok.

content(File) -> 
    case file:read_file(File) of
			{ok, Binary} -> Binary;
			{error,_}-> <<>>
	end.

expire()->
%% Calculate expire date far into future...
	Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	TenYears = 10 * 365 * 24 * 60 * 60,
	Seconds1 = calendar:gregorian_seconds_to_datetime(Seconds + TenYears),
	httpd_util:rfc1123_date(Seconds1).
