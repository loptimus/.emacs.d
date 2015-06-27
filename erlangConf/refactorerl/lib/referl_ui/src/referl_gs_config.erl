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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2010,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @author Jimmy <>
%%%
%%% @doc Erlang GS user interface configuration functions
%%%
%%% Created : 13 Nov 2009 by Jimmy &lt;&gt;
%%%-------------------------------------------------------------------
-module(referl_gs_config).

-svn("$Rev$ ").

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([color/2, size/1, order/1, toolbar/1, options/1, set_options/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("gs.hrl").

-define(SERVER, ?MODULE).
-record(state, {colors, sizes, orders, options}).
%-define(ConfigFile, "gs.cfg").

-define(Colors, gs_config_color).
-define(Sizes, gs_config_sizes).
-define(Order, gs_config_order).
-define(Toolbar, gs_config_toolbar).
-define(Options, gs_config_options).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stops the server
stop() ->
    gen_server:call(?SERVER, stop).

%% @doc Gets the color associated with a GUI widget
color(Attr, Value) ->
    gen_server:call(?SERVER, {color, Attr, Value}).

%% @doc Gets the size of a column
size(Attr) ->
    gen_server:call(?SERVER, {size, Attr}).

%% @doc Gets the column ordering
order(Attr) ->
    gen_server:call(?SERVER, {order, Attr}).

%% @doc Gets the visible tool buttons
toolbar(Attr) ->
    gen_server:call(?SERVER, {toolbar, Attr}).

%% @doc Gets the visible tool buttons
options(Attr) ->
    gen_server:call(?SERVER, {options, Attr}).

%% @doc Sets the toolbar options
set_options(Oplist) ->
    gen_server:call(?SERVER, {setoptions, Oplist}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
init([]) ->
    ColorsTable = ets:new(?Colors, []),
    SizesTable = ets:new(?Sizes, []),
    OrderTable = ets:new(?Order, []),
    OptionsTable = ets:new(?Options, []),
    ets:insert(ColorsTable,
        [{{tree, background}, {0, 120, 240}},
        {{default, default}, {{255, 255, 255}, {0, 0, 0}}},
        {{class, root},      {{0, 0, 0}, {255, 255, 255}}},
        {{class, module},    {{255, 255, 255}, {100, 0, 0}}},
        {{class, header},    {{255, 255, 255}, {0, 100, 100}}},
        {{class, macro},     {{255, 255, 255}, {100, 0, 100}}},
        {{class, record},    {{255, 255, 255}, {0, 100, 0}}},
        {{class, func},      {{255, 255, 255}, {0, 60, 120}}},
        {{class, clause},    {{255, 255, 255}, {255, 0, 0}}},
        {{class, expr},      {{255, 255, 255}, {0, 0, 240}}},
        {{class, variable},  {{255, 255, 255}, {200, 100, 0}}},
        {{type, expr},       {{255, 255, 255}, {0, 0, 240}}},
        {{type, pattern},    {{255, 255, 255}, {0, 0, 240}}},
        {{type, guard},      {{255, 255, 255}, {0, 0, 240}}}]),
    ets:insert(SizesTable, [{class, 60}, {type, 56}, {kind, 80},
        {id, 40}, {name, 160}]),
    ets:insert(OrderTable, [{default, [class, type, kind, id, name]},
        {expr, [type, kind, id, name]},
        {clause, [class, type, kind, id]}]),
    ets:insert(OptionsTable, 
        [{tool,["Add module","Undo","Refresh","Run query"]},
         {module,["Rename","Drop"]},
         {func,["Rename","Move","Reorder","Tuple"]},
         {macro,["Rename"]},
         {variable,["Rename","Eliminate"]},
         {implicit_fun,["Expand"]},
         {application,["Inline"]}]),
    {ok, #state{colors=ColorsTable,
        sizes=SizesTable,
        orders=OrderTable,
        options=OptionsTable}}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({color, Attr, Value}, _From, State = #state{colors=ColorsTable}) ->
    case ets:lookup(ColorsTable, {Attr, Value}) of
        [] ->
            [{_, Reply}] = ets:lookup(ColorsTable, {default, default});
        [{_, Reply}] ->
            void
    end,
    {reply, Reply, State};

handle_call({size, Attr}, _From, State = #state{sizes=SizesTable}) ->
    [{_, Reply}] = ets:lookup(SizesTable, Attr),
    {reply, Reply, State};

handle_call({order, Attr}, _From, State = #state{orders=OrderTable}) ->
    case ets:lookup(OrderTable, Attr) of
        [] -> [{_, Reply}] = ets:lookup(OrderTable, default);
        [{_, Reply}] -> void
    end,
    {reply, Reply, State};

handle_call({toolbar, Attr}, _From, State) ->
    dets:open_file(?Toolbar, []),
    Reply = case dets:lookup(?Toolbar, Attr) of
        [{_, Oplist}] -> Oplist;
        _ -> []
    end,
    dets:close(?Toolbar),
    {reply, Reply, State};

handle_call({options, Attr}, _From, State = #state{options=OptionsTable}) ->
    case ets:lookup(OptionsTable, Attr) of
        [] -> Reply = [];
        [{_, Reply}] -> void
    end,
    {reply, Reply, State};

handle_call({setoptions, Oplist}, _From, State) ->
    dets:open_file(?Toolbar, []),
    Reply = case dets:insert(?Toolbar, Oplist) of
        ok -> true;
        {error, _} -> false
    end,
    dets:close(?Toolbar),
    {reply, Reply, State}.

%% @private
%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    ColorsTable = State#state.colors,
    SizesTable = State#state.sizes,
    OrderTable = State#state.orders,
    OptionsTable = State#state.options,

    ets:delete(ColorsTable),
    ets:delete(SizesTable),
    ets:delete(OrderTable),
    ets:delete(OptionsTable).

%% @private
%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
