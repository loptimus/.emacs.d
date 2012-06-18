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
%%% The Initial Developer of the  Original Code is Eotvos Lorand University.
%%% Portions created  by Eotvos  Lorand University are  Copyright 2007-2009,
%%% Eotvos Lorand University. All Rights Reserved.

%%% @doc
%%%
%%% @author Andras Nemeth <neataai@caesar.elte.hu>

-module(gen_refdb).
-vsn("$Rev: 8051 $ ").

%%
%% Include files
%%

-record(refdb,
        {state = [],
         dbmod,
         cb,
         name}).

-include("core.hrl").

%%
%% Exported Functions
%%
-export([behaviour_info/1,
         system_continue/3,
         system_terminate/4,
         system_code_change/4,
         handle_dbg/3]).

% we are cheating here. The user doesn't let to decide the name of refdb
-export([start_link/0,
         init_it/6,
         sync_operation/1,
         sync_operation/2,
         async_operation/1,
         async_operation/2,
         async_operation/3,
         result/2,
         get_state/0
        ]).

%%% ===========================================================================
%%% Init arguments

%%
%% API Functions
%%

behaviour_info(callbacks) ->
    [{handle_operation, 3},
     {init, 1},
     {code_change, 2},
     {terminate, 2}];

behaviour_info(_Else) ->
    undefined.

start_link() ->
    %DbMod = refcore_lib:get_init_arg(dbmod),
    %DbArgs = refcore_lib:get_init_arg(dbargs),
    DbMod = get_init_arg(dbmod),
    DbArgs = get_init_arg(dbargs),
    start_link({local, ?RefDb}, DbMod, DbArgs).

%% start_link(DbMod, Args) ->
%%     start_link({local, ?RefDb}, DbMod, Args).

start_link(Name, DbMod, Args) when is_list(Args) ->
    gen:start(?MODULE, link, Name, DbMod, Args, []).

init_it(Starter, Parent, Name, DbMod, Args, Options) ->
    State =
        case DbMod:init(Args) of
            {ok, S} ->
                S;
            {X, Reason} when X =:= stop;
                             X =:= 'EXIT'->
                unregister_name(Name),
                proc_lib:init_ack(Starter, {error, Reason}),
                exit(Reason);
            Other ->
                unregister_name(Name),
                Error = {unexpected_return_value, Other},
                proc_lib:init_ack(Starter, {error, Error}),
                exit(Error)
        end,
    RefDb = #refdb{state = State,
                   dbmod = DbMod,
                   name = Name},
    Deb = debug_options(Options),
    proc_lib:init_ack(Starter, {ok, self()}),
    loop(Parent, Deb, RefDb).

debug_options(Options) ->
    DebugOpts =
        case lists:keysearch(debug, 1, Options) of
            {value, {debug, DebOpts}} ->
                DebOpts;
            _ ->
                []
        end,
    sys:debug_options(DebugOpts).

%% well, this is what it is
%% since RefDb will be a single process, the interface functions
%% can be put into the generic part
sync_operation(Q) ->
    sync_operation(?RefDb, Q, infinity).

sync_operation(Q, Timeout) ->
    sync_operation(?RefDb, Q, Timeout).

sync_operation(Dest, Q, Timeout)
  when is_pid(Dest) orelse is_atom(Dest) ->
    sync_wrapper(Dest, Q, Timeout);
sync_operation(_, _, _) ->
    throw (invalid_pid_or_name).

async_operation(Q) ->
    async_operation(Q, no_callback).

async_operation(Q, CallBack) ->
    async_operation(?RefDb, Q, CallBack).

async_operation(Dest, Q, {_M, _F, A} = CallBack)
  when is_list(A) andalso
       (is_pid(Dest) orelse 
        is_atom(Dest)) ->
    async_wrapper(Dest, Q, CallBack).

result(Result, Extra) ->
    return_result(Result, Extra).

get_state() ->
    whereis(?RefDb) ! {'sync_op', get_state}.

%%%
%%% Callbacks for system messages
%%%

system_event(Msg, From, Parent, Mod, Deb, Misc) ->
    sys:handle_system_msg(Msg, From, Parent, Mod, Deb, Misc).

system_continue(Parent, Deb, RefDb) ->
    loop(Parent, Deb, RefDb).

system_terminate(Reason, _Parent, _Deb, RefDb) ->
    terminate(Reason, RefDb).

system_code_change(#refdb{dbmod=DbMod, state = State} = RefDb, _Module, OldVsn, Extra) ->
    case catch DbMod:code_change(OldVsn, State, Extra) of
        {ok, NewState} ->
            {ok, RefDb#refdb{state = NewState}};
        Other ->
            Other
    end.

%%
%% Local Functions
%%

sync_wrapper(Dest, Q, Timeout) ->
    case catch gen:call(Dest, '$sync_op', {request, Q}, Timeout) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            io:format("EXIT from user! ~n"
                      "    caught exception ~p~n params ~p ~p~n", [Reason, Q, Timeout]),
            exit({Reason, {?MODULE, request_wrapper, [Dest, Q]}})%;
    end.

async_wrapper(Dest, Q, CallBack) ->
    throw(not_implemented),
    Msg = {'$async_op', Q, CallBack},
    case catch erlang:send(Dest, Msg, [noconnect]) of
        noconnect ->
            spawn(erlang, send, [Dest, Msg]); 
        Else ->
            Else
    end.

loop(Parent, Deb, RefDb) ->
    Message =
        receive M -> M
        end,
    {NewDeb, NewState} = process_message(Message, Parent, Deb, RefDb),    
    loop(Parent, NewDeb, RefDb#refdb{state = NewState}).

process_message({'$sync_op', From, Msg}, Parent, Deb, RefDb) ->
    case Msg of
        {get_state} ->
            print_state(RefDb),
            {Deb, RefDb#refdb.state};
        {request, Query} ->
            {sys:handle_debug(Deb, {?MODULE, handle_dbg}, ?MODULE, {request, Query}),
             exec_operation(Query, From, RefDb)};
        % now result messages doesn't come, since we don't use gen_refdb
        % to dispatch back the results, those will go directly to the caller
        {result, Result} ->
            {sys:handle_debug(Deb, {?MODULE, handle_dbg}, ?MODULE, {result, Result}),
             return_result(From, Result)};
        {System, From, Msg} ->
            Deb1 =
                sys:handle_debug(Deb, {?MODULE, handle_dbg}, ?MODULE, {system_msg, System, From, Msg}),
            system_event(Msg, From, Parent, ?MODULE, Deb, RefDb),
            {Deb1, RefDb#refdb.state} %% FIXME: will this work??
    end;
%% process_message({'$async_op', From, Msg}, Parent, Deb, RefDb) ->
%%     ok;
process_message({'EXIT', Parent, Reason}, Parent, _Deb, _RefDb) ->
    exit(Reason).

%% TODO
%% exec_opearation({schema, Schema}, Extra, RefDb) ->
%%     handle_schema(Schema, RefDb);
%% exec_operation({get_schema}, Extra, RefDb) ->
%%     handle_get_schema(RefDb);
%% exec_opearation({reset_schema}, Extra, RefDb) ->
%%     handle_reset_schema(RefDb);
%% exec_operation({path, _} = Path, Extra, RefDb) ->
%%     handle_path(Path, RefDb);
exec_operation(Query, Extra,  #refdb{dbmod = DbMod, state = State} = RefDb) ->
    case catch DbMod:handle_operation(Query, Extra, State) of
        {noresult, NewState} ->
            NewState;
        {result, Result, NewState} ->
            return_result(Result, Extra),
            NewState;
        {stop, Reason, Result, State} ->
            (catch terminate(Reason, RefDb)),
            return_result(Result, Extra),
            exit({stop, Reason});
        Other ->
            Reason = {unexpected_return_value, Other},
            (catch terminate(Reason, RefDb)),
            exit(Reason)
    end.

%% return_result(Result, {async, {Mod, Fun, Args}} = A) ->
%%     %io:format("*****returnung async result: ~p~n",[{Result,A}]),
%%     apply(Mod, Fun, [Result | Args]).

return_result(Result, {To, Tag}) ->
    To ! {Tag, Result}.
% i guess the below clause is useless....
%% return_result({To, Tag}, Result, {async, {Mod, Fun, Args}} = A) ->
%%     %io:format("*****returnung async result2: ~p~n",[{Result,A}]),
%%     To ! {Tag, Result},
%%     apply(Mod, Fun, [Result | Args]).
    
handle_dbg(Io, Event, Name) ->
    io:format(Io, "~p event: ~p~n", [Name, Event]).

terminate(Reason, #refdb{name = Name, dbmod = DbMod, state = State}) ->
    unregister_name(Name),
    DbMod:terminate(Reason, State),
    exit(Reason).

unregister_name(Name) ->
    erlang:unregister(Name).

print_state(RefDb) ->
    ?d(RefDb).

get_init_arg(Arg) ->
    try 
        begin
            {ok, [[AStr]]} = init:get_argument(Arg),
            {ok, Tokens, _} = erl_scan:string(AStr ++ "."),
            {ok, Term} = erl_parse:parse_term(Tokens),
            Term
        end 
    of
        Defined ->
            Defined
    catch
        _:_ -> undefined
    end.

%%
%% Common stuff provided by gen_refdb
%%

%% TODO
