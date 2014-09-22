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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc This query shows all bad code smells.

%%% @author Robert Kitlei <kitlei@inf.elte.hu>

%%% ============================================================================
%%% Module information

-module(reftr_query_badsmells).
-vsn("$Rev: 7172 $ ").

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Errors

%%% @private
error_text(_, _) ->
    ["Internal error."].

%%% ============================================================================
%%% Callbacks

-define(Metrics, refanal_metrics_helper).

%% @private
prepare(Args)->
    fun() ->
        ?Metrics:all_metric_errors()
    end.
