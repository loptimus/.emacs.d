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

%%% ===========================================================================
%%% Modules

%% Storage layer
-define(Graph,          refcore_graph).

%% Semantical layer
-define(ESG,            refcore_esg).
-define(Syn,            refcore_syntax).
-define(FileMan,        refcore_fileman).
-define(PreProc,        refcore_preproc).
-define(Dataflow,       refanal_dataflow).


%%% ===========================================================================
%%% Graph node

-define(NODETAG, '$gn').
-define(IS_NODE(Node), element(1, Node) =:= ?NODETAG).

%%% ===========================================================================
%%% Refactoring changes

-define(RefChangeTab, filename:join(mnesia:system_info(directory), "ref_change")).

%%% ===========================================================================
%%% Development mode

%% For development, uncomment the first line and comment the second one.
%% For releases, uncomment the second line and comment the first one.
-define(development_mode, development_mode).
-undef(development_mode).

%%% ===========================================================================
%%% Debugging

-ifdef(development_mode).
    %% Prettyprints the value of a single expression with module/line information.
    -define(d2(Name, X), io:format("~4w ~s~n ~12s: ~p~n", [?LINE, atom_to_list(?MODULE), Name, X])).
    -define(d(X), ?d2(??X, X)).

    %% Prettyprints a list of expressions with module/line information.
    -define(ds(Xs), io:format(lists:flatten(["~p-~p", ["~n  ~p" || _ <- Xs], "~n"]),
                                [?LINE, ?MODULE] ++ Xs)).

    -define(autoreset_schema, true).

     %%% On/off switch for dynamic analysis of fun expressions
     %%% Note that this setting adjusts the database scheme
    -define(anal_fun_exprs, ok).

-else.
    -define(d2(Name, X), no_debug_in_release_mode).
    -define(d(X),        no_debug_in_release_mode).
    -define(ds(Xs),      no_debug_in_release_mode).
    -define(autoreset_schema, false).
-endif.
