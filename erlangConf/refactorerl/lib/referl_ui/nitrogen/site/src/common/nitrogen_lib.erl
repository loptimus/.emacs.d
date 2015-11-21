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

-module (nitrogen_lib).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 7733 $ ").

-export([logout/1, logout_link/1, main/1]).

logout(User)->
	nitrogen_helper:delete_dependency_graphs_files(User),
	nitrogen_helper:stop_running_queries(User),
	wf:logout(),
	wf:redirect("/index").

main(TemplateName=[C|_]) when is_integer(C)-> 
	case wf:role(auth_users) of
		true ->wf:session(flashes, undefined),
			   #template { file=nitrogen_helper:get_nitrogen_site_dir()
						 ++"/templates/"++TemplateName };
		false ->wf:redirect_to_login("/index")
	end.

logout_link(User)->
		#link {text="Log out "++User, 
		   title = "Log out "++User,
		   postback=logout }.