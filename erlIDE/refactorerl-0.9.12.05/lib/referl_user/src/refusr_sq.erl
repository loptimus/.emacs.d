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

%%% @author Lilla Hajós <lya@elte.hu>

-module(refusr_sq).
-vsn("$Rev: 7973 $ ").

-export([run/3]).
-export([format_nodes/2]).
-export([prepare/1, error_text/2]).
-export([closure_worker/5, chains_worker/6]).

-include("user.hrl").
-include("sq_lib.hrl").

-define(Lib, refusr_sq_lib).
-define(Format, refusr_sq_format).

%%% ============================================================================
%%% Errors

error_text(lexical_error, Error) ->
    refusr_sq_lexer:format_error(Error);
error_text(syntax_error, Error) ->
    refusr_sq_parser:format_error(Error);
error_text(illegal_property, Params) ->
    io_lib:format("illegal ~p property: ~p", Params);
error_text(illegal_selector, Params) ->
    io_lib:format("illegal ~p selector: ~p  ", Params);
error_text(type_mismatch, [At, Of, ExpType]) ->
    [ case At of
          {Pl,De} -> io_lib:format("type mismatch at ~p ~p :\n    ", [Pl,De]);
          Pl -> io_lib:format("type mismatch at ~p:\n    ", [Pl])
      end,
      case Of of
          {Ent, Type} -> io_lib:format("the type of ~p is ~s, not ~s",
                                       [Ent, Type, ExpType]);
          Ent -> io_lib:format("the type of ~p is not ~s", [Ent, ExpType])
      end ];
error_text(no_property_in_comparison, []) ->
    "no property in comparison";
error_text(statistics_error, []) ->
    "statistics are only available for properties with numeric values".

%%% ============================================================================
%%% Callbacks

%% @spec run(DisplayOpt::proplist(), Params::proplist(), Query::string()) ->
%%           QueryResult::term()
%% @doc Returns the result of `Query' starting from the initial state given by
%%      `Params'. The format of the result is determined by `DisplayOpt'.
%%
%%      `Params' contains either the
%%          - (optional) `file' and `position' keys or
%%          - the `node_list' and (optional) `node_type' key.
%%       The possible values for
%%          - `node_type': file|function|record|field|macro|variable|expression
%%
%%      `DisplayOpt' contains the keys `positions' and `output'.
%%      The possible values for
%%          - `positions': none|scalar|linecol
%%          - `output': stdio|{iodev,io_device()}|msg|other|nodes
%%
%%      The `QueryResult' depends on the `output' key in `DisplayOpt'.
%%          - stdio: a formatted text written to stdio
%%          - {iodev,Dev::io_device()}: a formatted text written to Dev
%%          - msg: a message containing a list with the following types of
%%                 elements: {eq, Name, Value} |
%%                           {list, [{Position, Text}]} |
%%                           {chain, [{Position, Text}], PostWS} |
%%                           {group_by, {Position, Text}, eq, Name, Value} |
%%                           {group_by, {Position, Text},
%%                            list, [{Position, Text}]}
%%          - other: the same list that is otherwise sent by a message
%%          - nodes: a proplist with the keys: `nodes' for a list of nodes and
%%                                             `text' for a formatted text
%%      The format of positions depends on the `positions' key in `DisplayOpt'.
%%          - none: nopos
%%          - scalar: {File::string(), PosFrom::integer(), PosTo::integer()}
%%          - linecol: {File::string(), PosFrom::{integer(), integer()},
%%                                      PosTo::{integer(), integer()}}
run(DisplayOpt, Params, Query) when is_list(Query) ->
    Tokens   = tokenize(Query),
    SynTree  = parse(Tokens),
    SynTree2 = make_internal_representation(SynTree),
    Result   = process_semantic_query(Params, SynTree2),
    ?Format:result(Result, DisplayOpt).

make_internal_representation(SynTree) ->
    case refusr_ac:validate_query(SynTree) of
        {ok, Res} ->
            Res;
        {error, E} ->
            throw(?LocalError(syntax_error, E))
    end.

parse(Tokens) ->
    case refusr_ac_parser:parse(Tokens) of
        {ok, SynTree} ->
            SynTree;
        {error, {_, _, Err}} ->
            case lists:flatten(Err) of
                "{quoted," ++ A ->
                    A1 = lists:reverse(tl(lists:reverse(A))),
                    throw(?LocalError(syntax_error, A1));
                _ ->
                    throw(?LocalError(syntax_error, Err))
            end
    end.

tokenize(Query) ->
    case refusr_sq_lexer:string(Query) of
        {ok, Tokens, _} ->
            Tokens;
        {error, {_, _, Error}, _} ->
            throw(?LocalError(lexical_error, Error))
    end.

%% @private
%% @todo alapertelmezett? meggondolni mi kellene + eclipsenek nem biztos h ez kell
%% @todo no Hungarian in comments
prepare(Args) ->
    fun () ->
        [DisplayOpt,StartOpt,QueryStr] =
            ?MISC:pgetu([display_opt,start_opt,querystr],Args),
        run(DisplayOpt,
            StartOpt,
            QueryStr)
    end.

%%% ============================================================================
%%% Implementation

process_semantic_query(Params, SemanticQuery) ->
    {InitialSelector, _, Seq, Last} =
	lists:foldr(
	  fun(Filter = {filter, _F}, {undef, undef, [], Last}) ->
		  {undef, undef, [], [Filter|Last]};
	     ({initial_selector, InitialSelector}, {undef, Type, Mid, Last}) ->
		  {InitialSelector, Type, Mid, Last};
	     (Elem, {undef, undef, [], Last}) ->
		  {undef, element(1,Elem), [], [Elem|Last]};
	     (Filter = {filter, _F}, {undef, Type, Mid, Last}) ->
		  {undef, Type, [Filter|Mid], Last};
	     (Closure = {closure, _S, _M}, {undef, closure, Mid, Last}) ->
		  {undef, closure, [], [Closure|Mid++Last]};
	     (Iteration = {iteration, _S, _M}, {undef, iteration, Mid, Last}) ->
		  {undef, iteration, [], [Iteration|Mid++Last]};
	     (Elem, {undef, _Type, Mid, Last}) ->
		  {undef, mid, [Elem|Mid], Last}
	  end,
	  {undef, undef, [], []},
	  SemanticQuery),
    {InitType, InitEnts} = initial_state(Params, InitialSelector),
    {SeqType, CheckedSeq} = lists:foldl(fun check/2, {InitType, []}, Seq),
    {LastType, CheckedLast} = lists:foldl(fun check/2, {SeqType, []}, Last),
    case {LastType, CheckedLast} of
        {help, {EntityType, HelpType}} ->
            {help, {EntityType, HelpType}};
        _ ->
            {SeqType, SeqRes} =
                process_query_seq({InitType, InitEnts}, CheckedSeq),
            process_last_query({SeqType, SeqRes}, CheckedLast)
    end.

process_query_seq(State, QuerySeq) ->
    lists:foldr(fun process/2, State, QuerySeq).

process_last_query({Type, Entities}, LastQuery) ->
    State = #state{action= selection, type= Type, res= Entities},
    lists:foldr(fun process_last/2, State, LastQuery).

%%% ============================================================================
%%% Preprocessing of queries

%todo:  node list <-> query format check
initial_state(Params, {quoted, InitialSelector}) ->
    initial_state(Params, InitialSelector);
initial_state(Params, InitialSelector) ->
    {NodeType, NodeList} =
        case proplists:get_value(node_list, Params) of
            undefined ->
                case InitialSelector of
                    undef -> {none, []};
                    _ ->
                        ?Lib:init_sel([{ask_missing,false} | Params],
                                      InitialSelector)
                end;
            [] ->
                {none, []};
            [HNode|TNodes] = Nodes ->
                Type = proplists:get_value(node_type, Params,
                                           ?Lib:node_type(HNode)),

                NT = [{Node, ?Lib:node_type(Node)}|| Node <- TNodes],
                Diffs = lists:filter(fun({_N, NType}) -> NType/=Type end, NT),
                case Diffs of
                    [] ->
                        {Type, Nodes};
                    _ ->
                        Params = ["initial state", hd(Diffs), Type],
                        throw(?LocalError(type_mismatch, Params))
                end
        end,
    {NodeType, ordsets:from_list(NodeList)}.

check({selector, Sel}, {Type, Lst}) ->
    Selector =
        case Sel of
            {quoted, Atom} -> Atom;
            Atom -> Atom
        end,
    case ?Lib:sel_type(Type, Selector) of
        [SelType] ->
            {SelType, [{selector, Selector, SelType}| Lst]};
        [] ->
            case ?Lib:prop_type(Type, Selector) of
                [PropType] -> {PropType, [{property, Selector, PropType}| Lst]};
                [] -> throw(?LocalError(illegal_selector, [Type, Selector]))
            end
    end;

check({Action, {seq, Seq}, {mult, Mult}}, {Type, Lst}) ->
    case lists:foldl(fun check/2, {Type, []}, Seq) of
        {Type, QSLst} ->
            case Mult of
                0 -> {Type, Lst};
                _ -> {Type, [{Action, QSLst, Mult}| Lst]}
            end;
        {BadType, _} ->
            throw(?LocalError(type_mismatch, [Action, {Seq, BadType}, Type]))
    end;

check({filter, Filter}, {Type, Lst}) ->
    {Type, [{filter, check_filter(Type, Filter)}|Lst]};

check({statistics, Stat}, {PropType, []}) ->
    Statistics =
        case Stat of
            {quoted, S} -> S;
            S           -> S
        end,
    case PropType of
        any ->
            {int, [{statistics, Statistics}]};
        int ->
            {int, [{statistics, Statistics}]};
        _ ->
	    throw(?LocalError(statistics_error, []))
    end;

%todo: filterekben help?
check({help, HelpType}, {Type, _List}) ->
    case HelpType of
        initial_selectors -> {help, {initial_selectors, []}};
        queries           -> {help, {selectors, Type}};
        statistics        -> {help, {statistics, []}};
        filters           -> {help, {properties, Type}}
    end.

%% @private
%% @spec check_filter(Type::atom(), Filter::atom()|tuple()) -> atom()|tuple()
check_filter(_Type, 'true') ->
    'true';

check_filter(_Type, 'false') ->
    'false';

check_filter(Type, {'or', Filter1, Filter2}) ->
    {'or', check_filter(Type, Filter1), check_filter(Type, Filter2)};

check_filter(Type, {'and', Filter1, Filter2}) ->
    {'and', check_filter(Type, Filter1), check_filter(Type, Filter2)};

check_filter(Type, {'not', Filter}) ->
    {'not', check_filter(Type, Filter)};

check_filter(Type, {seq, Seq}) ->
    {_QSType, CheckedQS} = lists:foldl(fun check/2, {Type, []}, Seq),
    {seq, CheckedQS};

check_filter(_Type, {_CompOp, {quoted, _}, {quoted, _}}) ->
    throw(?LocalError(no_property_in_comparison, []));

check_filter(Type, {CompOp, {quoted, _} = F1, F2}) ->
    check_filter(Type, {CompOp, F2, F1});

check_filter(Type, {CompOp, F1, {quoted, _} = F2}) ->
    if
        is_tuple(F1) -> {CompOp, check_filter(Type, F1), F2};
        is_atom(F1)  ->
            PropType1 = ?Lib:prop_type(Type, F1),
            ?Check(PropType1 =:= [atom],
                   ?LocalError(no_property_in_comparison, [])),
            {CompOp, F1, F2};
        true ->
            throw(?LocalError(no_property_in_comparison, []))
    end;

check_filter(Type, {CompOp, F1, F2}) when is_tuple(F1) andalso is_tuple(F2) ->
    {CompOp, check_filter(Type, F1), check_filter(Type, F1)};

check_filter(Type, {CompOp, F1, F2}) when is_tuple(F1) ->
    ?Check(?Lib:prop_type(Type, F2) == [bool] orelse is_boolean(F2),
           ?LocalError(type_mismatch, [{filter,{CompOp, F1, F2}}, F2, bool])),
    {CompOp, check_filter(Type, F1), F2};

check_filter(Type, {CompOp, F1, F2}) when is_tuple(F2) ->
    {CompOp, CheckedF2, F1} = check_filter(Type, {CompOp, F2, F1}),
    {CompOp, F1, CheckedF2};

check_filter(Type, {CompOp, name, F2}) ->
    TypeOfName = ?Lib:prop_type(Type, name),
    %`Type' doesn't have a `name' property
    ?Check(TypeOfName /= [], ?LocalError(illegal_property, [Type, name])),

    PropTypeOfF2 = ?Lib:prop_type(Type, F2),
    
    case PropTypeOfF2 of
        %%`F2' is not a property, but not string neither atom
        [] when not (is_list(F2) orelse is_atom(F2)) ->
            throw(?LocalError(type_mismatch, [{filter, {CompOp, name, F2}},
                                              F2, hd(TypeOfName)]));
        
        %%`F2' is a property, but it differs in type
        [_] when TypeOfName /= PropTypeOfF2 ->
            throw(?LocalError(type_mismatch, [{filter, {CompOp, name, F2}},
                                              {F2, hd(PropTypeOfF2)},
                                              hd(TypeOfName)]));
        
        %%`F2' is a property and the two types are matching, or is not
        %%a property, but is a type of atom or string
        _ -> ok

    end,
            
    %% %`F2' is a property, but not of a matching type
    %% ?Check(PropTypeOfF2 == [] orelse TypeOfName == PropTypeOfF2,
    %%        ?LocalError(type_mismatch, [{filter, {CompOp, name, F2}},
    %%                                    {F2, hd(PropTypeOfF2)},hd(TypeOfName)])),
    %% %`F2' is not a property, but not string or atom either
    %% ?Check(PropTypeOfF2 == [] andalso (is_list(F2) orelse is_atom(F2)),
    %%        ?LocalError(type_mismatch, [{filter, {CompOp, name, F2}},
    %%                                    F2, hd(TypeOfName)])),
    TypeOfF2 = if
                   PropTypeOfF2 /= [] ->PropTypeOfF2;
                   is_list(F2)        -> [string];
                   is_atom(F2)        -> [atom]
               end,

    if
        TypeOfName == TypeOfF2 -> {CompOp, name, F2};
        TypeOfF2 == [atom]     -> {CompOp, name, atom_to_list(F2)};
        TypeOfF2 == [string]   -> {CompOp, name, list_to_atom(F2)}
    end;

check_filter(Type, {CompOp, F1, name}) ->
    {CompOp, name, Comp2} = check_filter(Type, {CompOp, name, F1}),
    {CompOp, Comp2, name};

check_filter(Type, {CompOp, F1, F2}) when is_atom(F1) andalso is_atom(F2)->
    PropType1 = ?Lib:prop_type(Type, F1),
    PropType2 = ?Lib:prop_type(Type, F2),
    ?Check(PropType1 /= [] orelse PropType2 /= [],
           ?LocalError(no_property_in_comparison, [])),

    Type1 = case PropType1 of [] -> [atom]; _ -> PropType1 end,
    Type2 = case PropType2 of [] -> [atom]; _ -> PropType2 end,

    if
        Type1 == Type2 -> {CompOp, F1, F2};
        Type1 == [any] orelse Type2 == [any] -> {CompOp, F1, F2};
        Type1 == [bool] andalso is_boolean(F2) -> {CompOp, F1, F2};
        Type2 == [bool] andalso is_boolean(F1) -> {CompOp, F1, F2};
        true -> throw(?LocalError(type_mismatch, [{filter, {CompOp, F1, F2}},
                                                  {F1, hd(Type1)}, hd(Type2)]))
    end;

check_filter(Type, {CompOp, F1, F2}) when is_atom(F1) ->
    PropType = ?Lib:prop_type(Type, F1),

    ?Check(PropType /= [],
           ?LocalError(no_property_in_comparison, [])),

    ?Check(PropType == [any] orelse
           (PropType == [int] andalso is_integer(F2)) orelse
           (PropType == [atom] andalso is_atom(F2)) orelse
           (PropType == [string] andalso is_list(F2)) orelse
           (PropType == [bool] andalso is_boolean(F2)),
           ?LocalError(type_mismatch,
                       [{filter, {CompOp, F1, F2}}, F2, hd(PropType)])),

    {CompOp, F1, F2};

check_filter(Type, {CompOp, F1, F2}) when is_atom(F2) ->
    check_filter(Type, {CompOp, F2, F1}),
    {CompOp, F1, F2};

check_filter(_Type, {_CompOp, _F1, _F2}) ->
    throw(?LocalError(no_property_in_comparison, []));

check_filter(Type, Filter) ->
    PropType = ?Lib:prop_type(Type, Filter),
    case PropType of
        [bool] ->
            Filter;
        [] ->
            throw(?LocalError(illegal_property, [Type, Filter]));
        _ ->
            throw(?LocalError(type_mismatch,
                              [{filter,Filter}, {Filter, hd(PropType)}, bool]))
    end.

%%% ============================================================================
%%% Processing of queries

process({selector, Selector, SelType}, {Type, Entities}) ->
    [Fun] = ?Lib:sel_fun(Type, Selector),
    NewEntities =
        ordsets:fold(
          fun(Entity, Acc) ->
                  ordsets:union(ordsets:from_list(Fun(Entity)), Acc)
          end,
          ordsets:new(),
          Entities),
    {SelType, NewEntities};

process({_Action, _QSLst, 0}, State) -> State;

process({iteration, QSLst, Mult}, State) ->
    process({iteration, QSLst, Mult-1}, process_query_seq(State, QSLst));

process({closure, QSLst, Mult}, {Type, Entities}) ->
    Tab  = ets:new(store, [public, ordered_set, {write_concurrency, true}]),
    EntityLists = group_entities(Entities),
    [ets:insert(Tab, {E}) || E <- Entities],
    Keys = [ rpc:async_call(node(), ?MODULE, closure_worker,
                            [QSLst, Mult, Type, [E], Tab])
             || E <- EntityLists],
    [ ok = rpc:yield(K) || K <- Keys],
    Result =
        ets:select(Tab, [{{'$1'},[], ['$1']}]),
    ets:delete(Tab),
    {Type, Result};

process({filter, Filter}, {Type, Entities}) ->
    {Type, filter(Filter, Type, Entities)};

process({property, Prop, PropType}, {Type, Entities}) ->
    [Fun] = ?Lib:prop_fun(Type, Prop),
    {PropType, [Fun(Entity) || Entity <- Entities]}.


group_entities(Entities) ->
    NumberOfData = length(Entities),
    Divide =
        case erlang:system_info(logical_processors_available) of
            unknown ->
                (NumberOfData div 10) + 1;
            N ->
                (NumberOfData div (N*10)) + 1
        end,
    ?MISC:slice_list(Divide, Entities).

%% @private
closure_worker(_QSLst, 0, _Type, _Entities, _Tab) ->
    ok;
closure_worker(_QSLst, _Mult, _Type, [], _Tab) ->
    ok;
closure_worker(QSLst, Mult, Type, Entities, Tab) ->
    {Type, NewEntities} = process_query_seq({Type, Entities}, QSLst),
    ReallyNewEntities =
        [begin
             ets:insert(Tab, {NEntity}),
             NEntity
         end || NEntity <- NewEntities, not  ets:member(Tab, NEntity)],
    NewMult = case Mult of infinite = I -> I; M -> M-1 end,
    closure_worker(QSLst, NewMult, Type, ReallyNewEntities, Tab).


process_last(_Query, #state{res=[]}=State) -> State;

process_last({selector, Selector, SelType}, #state{type=Type, res=Entities}) ->
    [Fun] = ?Lib:sel_fun(Type, Selector),
    #state{action    = selection, %TODO: set lista helyett?
           type      = SelType, res      = [Fun(Entity)|| Entity <- Entities],
           prev_type = Type,    prev_res = Entities};

process_last({filter, Filt},
	     #state{action=selection, type=Type, res=Res, prev_res=[]}=St) ->
    St#state{res=filter(Filt, Type, Res)};

process_last({filter, Filt}, #state{action=selection, type=Type, res=Res}=St) ->
    St#state{res=[filter(Filt, Type, Entities)|| Entities <- Res]};

process_last({property, Prop, _PropType}, #state{type=Type, res=Entities}) ->
    [Fun] = ?Lib:prop_fun(Type, Prop),
    #state{action    = property_query,
           type      = Prop, res      = [Fun(Entity) || Entity <- Entities],
           prev_type = Type, prev_res = Entities};

process_last({Action, QSLst, Mult}, #state{type=Type, res=Res}=State) ->
    InitialChains =
        case State#state.action of
            selection -> #chains{incomplete=[[Entity]|| Entity<-Res]};
            Action -> Res
        end,
    #state{action = Action,
           type = Type, res = chains(InitialChains, QSLst, Type, Mult, Action)};

process_last({filter, Filt}, #state{action=iteration, type=Type, res=Res}=St) ->
    NewIncomplete = lists:filter(
                      fun(Chain) -> filter(Filt, Type, [hd(Chain)]) /= [] end,
                      Res#chains.incomplete),

    St#state{res = Res#chains{incomplete = NewIncomplete}};

process_last({filter, Filt}, #state{action=closure, type=Type, res=Res}=St) ->
    NewComplete = filter_chain(Filt, Type, Res#chains.complete),
    NewIncomplete = filter_chain(Filt, Type, Res#chains.incomplete),
    NewRecursive = filter_chain(Filt, Type, Res#chains.recursive),

    NewChains = Res#chains{complete   = NewComplete,
                           incomplete = NewIncomplete,
                           recursive  = NewRecursive},

    St#state{res = NewChains};

% todo: preproc
process_last({statistics, Stat}, #state{res=PropValues}) ->
    NonNum = lists:filter(fun(Val) -> not is_number(Val) end, PropValues),

    ?Check(NonNum == [],
           ?LocalError(type_mismatch, [{statistics,Stat}, hd(NonNum), number])),

    Fun = ?Lib:stat_fun(Stat),
    #state{action = statistics, type = Stat, res = Fun(PropValues)}.

%%% ============================================================================
%%% Helper functions

chains_worker([] = _Chains, _QSLst, _Type, _Mult, _Action, _Tab) ->
    ok;
chains_worker(Chains, _QSLst, _Type, 0, _Action, Tab) ->
    ets:insert(Tab, [{incomplete, Chain} || Chain <- Chains]),
    ok;
chains_worker(Chains, QSLst, Type, Mult, Action, Tab) ->
    NewChains =
        lists:flatmap(
          fun([H|_] = Chain) ->
                  {Type, Res} = process_query_seq({Type, [H]}, QSLst),
                  next_chain(Action, Chain, Res)
          end,
          Chains),
    SplitFun =
        fun({complete, _} = C, {Incomp, Comp, Rec}) ->
                {Incomp, [C | Comp], Rec};
           ({recursive, _} = R, {Incomp, Comp, Rec}) ->
                {Incomp, Comp, [R | Rec]};
           (List, {Incomp, Comp, Rec}) ->
                {[List | Incomp], Comp, Rec}
        end,

    {Incomplete, Completed, Recursive} =
        lists:foldl(SplitFun, {[], [], []}, NewChains),

    ets:insert(Tab, Completed ++ Recursive),

    NewMult = case Mult of infinite -> infinite; Mult -> Mult-1 end,
    chains_worker(Incomplete, QSLst, Type, NewMult, Action, Tab).

chains(#chains{incomplete = []} = Chains, _QSLst, _Type, _Mult, _Action) ->
    Chains;
chains(#chains{incomplete = Chains} = ChainsRec, QSLst, Type, Mult, Action) ->
    Tab  = ets:new(store, [public, bag, {write_concurrency, true}]),
    ChainGroups = group_entities(Chains),
    Keys = [rpc:async_call(node(), ?MODULE, chains_worker,
                           [Ch, QSLst, Type, Mult, Action, Tab])
            || Ch <- ChainGroups],
    [ ok = rpc:yield(K) || K <- Keys],

    Recursive =
        ets:select(Tab, [{{'$1', '$2'},[{'==', '$1', recursive}], ['$2']}]),
    Completed =
        ets:select(Tab, [{{'$1', '$2'},[{'==', '$1', complete}], ['$2']}]),
    Incomplete =
        ets:select(Tab, [{{'$1', '$2'},[{'==', '$1', incomplete}], ['$2']}]),
    ets:delete(Tab),
    ChainsRec#chains{complete   = Completed ++ ChainsRec#chains.complete,
                     incomplete = Incomplete,
                     recursive  =  Recursive ++ ChainsRec#chains.recursive}.

next_chain(iteration, Chain, Result) ->
    lists:foldl(fun(Entity, Acc) -> [[Entity| Chain]|Acc] end, [], Result);
next_chain(closure, Chain, []) ->
    [{complete, Chain}];
next_chain(closure, Chain, Result) ->
    lists:foldl(
      fun(Entity, Acc) ->
              case lists:member(Entity, Chain) of
                  true -> [{recursive, [Entity|Chain]}|Acc];
                  _    -> [[Entity| Chain]|Acc]
              end
      end,
      [],
      Result).

filter_chain(Filter, Type, Chain) ->
    NewChainWithEmpties = [ filter(Filter, Type, List) || List <- Chain ],
    lists:filter( fun(List) -> List /= [] end, NewChainWithEmpties).

%%% ============================================================================
%%% Filters

%% TODO: entity listakat rendezni filter elott!!!!
%% @private
%% @spec filter(Filter::term(), EntityType::atom(), ordset()) -> ordset()
filter(_Filter, _EntityType, []) -> [];

filter('true', _EntityType, Entities) -> Entities;

filter('false', _EntityType, _Entities) -> [];

filter({'not', Filter}, EntityType, Entities) ->
    ordsets:subtract(Entities, filter(Filter, EntityType, Entities));

filter({'or', Filter1, Filter2}, EntityType, Entities) ->
    ordsets:union(filter(Filter1, EntityType, Entities),
                  filter(Filter2, EntityType, Entities));

filter({'and', Filter1, Filter2}, EntityType, Entities) ->
    filter(Filter2,
           EntityType,
           filter(Filter1, EntityType, Entities));

%% @todo sq + list [a,s,d...] -> split
%% filter({'in', Property, {query_seq, QuerySeq}}, EntityType, Entities) ->
%%     FstInitialState = #state{res=[hd(Entities)], type=EntityType},
%%     FstResultingState = process_query_seq(FstInitialState, QuerySeq),
%%     NewProperty = FstResultingState#state.type,
%%     NewEntityType = FstResultingState#state.prev_type,

%%     ?Check( FstResultingState#state.action == property_query,
%%             ?LocalError(non_property, [NewProperty])),

%%     PropertyType = ?Lib:prop_type(EntityType, Property),
%%     NewPropertyType = ?Lib:prop_type(NewEntityType, NewProperty),
%%     ?Check( PropertyType == any orelse NewPropertyType == any orelse
%%             PropertyType == NewPropertyType,
%%             ?LocalError(prop_type_mismatch, [Property, NewProperty])),

%%     PropFun = prop_fun(EntityType, Property),

%%     lists:filter(
%%       fun(Entity) ->
%%               InitialState = #state{res=[Entity], type=EntityType},
%%               ResultingState = process_query_seq(InitialState, QuerySeq),
%%               lists:member(PropFun(Entity), ResultingState#state.res)
%%       end,
%%       Entities);

filter({seq, QuerySeq}, EntityType, Entities) ->
    ordsets:filter(
      fun(Entity) ->
              case process_query_seq({EntityType, [Entity]}, QuerySeq) of
                  {_QSType, Res} -> Res /= [];
                  _ -> false
              end
      end,
      Entities);

%% Comparison works on atom, int and string.
%% todo: regexp match -> if a string doesn't match
filter({CompOp, Filt1, Filt2}, EntityType, Entities) ->
    Comp1 = case Filt1 of
                {quoted, _} -> Filt1;
                _ when is_tuple(Filt1) -> filter(Filt1, EntityType, Entities);
                _  -> Filt1
            end,

    Comp2 = case Filt2 of
                {quoted, _} -> Filt2;
                _ when is_tuple(Filt2) -> filter(Filt2, EntityType, Entities);
                _ -> Filt2
            end,

    PropFun1 = ?Lib:prop_fun(EntityType, Comp1),
    PropFun2 = ?Lib:prop_fun(EntityType, Comp2),

    ordsets:filter(
      fun(Entity) ->
              CompL = case PropFun1 of [] -> Comp1; [Fun1] -> Fun1(Entity) end,
              CompR = case PropFun2 of [] -> Comp2; [Fun2] -> Fun2(Entity) end,
              compare(CompOp, CompL, CompR)
      end,
      Entities);

filter(Filter, EntityType, Entities)->
    ordsets:filter(prop_fun(EntityType, Filter), Entities).


%%% ============================================================================
%%% Helper functions

compare(like, CompL, CompR) ->
    Distance =
        if
            is_list(CompL) andalso is_list(CompR) ->
                refusr_strm:getDistance(CompL, CompR);
            is_atom(CompL) andalso is_atom(CompR) ->
                refusr_strm:getDistance(atom_to_list(CompL),
                                        atom_to_list(CompR));
            true -> false
        end,

    case Distance of
         {lev, 0} -> true;
         {lev, 1} -> true;
         {lev, 2} ->
            Length = if is_atom(CompL) -> length(atom_to_list(CompL));
                        true           -> length(CompL)
                     end,
            case Length > 5 of true -> true; _ -> false end;
        _ -> false
    end;

compare(CompOp, {quoted, A}, CompR) ->
    erlang:CompOp(A, CompR);

compare(CompOp, CompL, {quoted, A}) ->
    erlang:CompOp(CompL, A);

compare(CompOp, CompL, CompR) ->
    erlang:CompOp(CompL, CompR).

prop_fun(EntityType, Filter) ->
    case ?Lib:prop_fun(EntityType, Filter) of
        [] -> throw(?LocalError(illegal_property, [EntityType, Filter]));
        [Fun]   -> Fun
    end.

%% @spec format_nodes(Nodes::[entity()], Positions::atom()) -> string()
%%       Positions = none|scalar|linecol
%% @doc Returns a textual representation for a list of nodes.
format_nodes(Nodes, Position) -> ?Format:nodes(Nodes, Position).
