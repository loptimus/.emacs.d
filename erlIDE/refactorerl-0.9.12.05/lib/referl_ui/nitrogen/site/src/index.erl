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
-module (index).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-vsn("$Rev: 8337 $ ").

-define(PW_TAB, filename:join(mnesia:system_info(directory),"adminpass")).

%%% ============================================================================
%%% HTML output

main() -> #template { file=nitrogen_helper:get_nitrogen_site_dir()++
							   "/templates/loginpage.html" }.

title() -> "RefactorErl Queries".

body() ->
    #panel{ body=[
       form()
    ]}.

form() ->
	Form=[% Create a label
		  #label { text="Username" },
		  % Create a textbox
		  #textbox { id=usernameTextBox, next=loginButton },
		  % Create a button with a postback of 'login'
		  #button { id=loginButton, 
					text="Ok", 
					postback=login, 
					class=referl_button },

		  #label { id=passwordlabel, text="" },
		  #panel{id=password},
		  #panel{id=wrongpw}],
	% Validate the text in the textbox
	wf:wire(loginButton, 
			usernameTextBox, 
			#validate { validators=[#is_required { text="Required." }]}),
	Form.

%%% ============================================================================
%%% Handlers for postback events	

event(login) ->
	User=wf:q(usernameTextBox),
	Pass=wf:q(password),
	RestrictedMode=nitrogen_helper:get_restricted_mode(),
	PasswordIsCorrect=if
		not RestrictedMode -> true;
		Pass==undefined -> checkPassword("");
		true -> checkPassword(Pass)
	end,
	if
		(Pass==undefined) and (not PasswordIsCorrect) and (User=="admin") -> askForPassword();
		(not PasswordIsCorrect) and (User=="admin") -> ok;
		true ->
			wf:replace(wrongpw,#panel{id=wrongpw,body=""}),
			wf:session(username, User),
			wf:role(auth_users, true),
			if
				(User=="admin") or (not RestrictedMode) -> wf:role(admin, true);
				true -> ok
			end,
			wf:user(User),
			wf:redirect_from_login("/main")
	end.

askForPassword() ->
	wf:replace(passwordlabel,#label { id=passwordlabel, text="Password" }),
	wf:replace(password, #password { id=password, next=loginButton }),
	wf:replace(wrongpw,#panel{id=wrongpw,body=""}).

checkPassword(Pass) ->
	dets:open_file(?PW_TAB,[]),
    Result=dets:match_object(?PW_TAB,{'_'}),
    dets:close(?PW_TAB),
	if
		Result==[] -> wf:wire(#script{script="alert('No admin password set!');"}),false;
		true -> 
			{Md5}=hd(Result),
			WrittenPass=erlang:md5(Pass),
			if
				WrittenPass==Md5 -> true;
				true -> 
					wf:replace(wrongpw,#panel{id=wrongpw,body="Wrong password!"}),
					false
			end
	end.

makeadminpassword(Pass) ->
	dets:open_file(?PW_TAB,[]),
	dets:delete_all_objects(?PW_TAB),
	dets:insert(?PW_TAB,{erlang:md5(Pass)}),
	dets:close(?PW_TAB).
