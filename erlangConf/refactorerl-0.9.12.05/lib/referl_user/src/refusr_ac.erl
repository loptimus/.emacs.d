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

%%% ============================================================================
%%% Module information

%%% @doc
%%%  Completes unfinished semantic queries. This module is intended to be 
%%%  served as a backend for various user interfaces. Thus the output is 
%%%  human readable.
%%%
%%%  It uses an extended semantic query grammar to control the amount of 
%%%  possible cases. The module provides a validator for the original grammar.
%%%  

%%% == Implementation status ==
%%% This feature is not fully implemented.

%%% @author Gabor Olah <olikas.g@gmail.com>
-module(refusr_ac).

-include("user.hrl").
-include("sq_lib.hrl").

-vsn("$Rev: 7973 $ ").


-export([run/1]).
-export([get_selectors/1]).
-export([validate_query/1]).
-export([sel_entity/1]).

%%%
%% @private
%% @doc
%% Gives the possible endings of the given semantic query. It does not run the query, thus
%% overpredicts the possible postfixes.
%% @end
%%
%% @spec run(atom() | string()) -> 
%%                                [{string() , string()} | 
%%                                 {'type'   , ('int' | 'atom' | 'string')}]
run(Query) when is_atom(Query) ->
    run(atom_to_list(Query));
run([]) ->
    [{"", atom_to_list(Sel)} || {Sel, _Ent} <- initial_selectors()];
run(Query) ->
    ParsedList = case refusr_sq_lexer:string(Query) of
                     {ok, Tokens, _} ->
                         _ACP = case refusr_ac_parser:parse(Tokens) of
                                   {ok, Result} ->
                                       Result;
                                   {error, E} ->
                                       E
                               end;
 %                    {error, E} ->
 %                        E;
                     {error,E,_} -> E
                 end,
    {Base, Last} = separate_useful_parts(ParsedList),
    complete(Base, Last).


    
%% @doc
%%  Separates the parsed lists into the base entity and 
%%  the following part needs to be completed.    
%% @end
separate_useful_parts([{complete, {initial_selector, _Selector} = Res}]) ->
    Res;
separate_useful_parts([_ParsedListItem]) ->
    ?d(_ParsedListItem);
separate_useful_parts(ParsedList) ->
    %?d(ParsedList),
    Res = sup(ParsedList, lists:last(ParsedList)),
    %?d(Res),
    Res.

%% @doc
%%  Computes the last entity on which the last item is invoked, 
%%  and returns this last element.
%%  Both last entity and element  massively depend on the structure of the query.
%% @end
%%
%% Second argument is the last element of List.
%% List is NOT reversed.
sup(List, {complete,   {selector, _Sel} = Selector}) ->
    %?d(List),
    LS = last_selector(lists:reverse(List)),
    LastEntity = sel_entity(LS),
    {LastEntity, Selector};
sup(List, {incomplete, {'query', '.'}}) -> 
    %?d(List),
    %?d('query.'),
    LS = last_selector(lists:reverse(List)),
    LastEntity = sel_entity(LS),
    {LastEntity, {selector, ''}};
% ---- filter
sup(List, {complete, {filter, _} = _Filter}) -> 
    %?d(List),
    %?d(Filter),
    LS = last_selector(lists:reverse(List)),
    LastEntity = sel_entity(LS),
    %?d(LastEntity),
    {LastEntity, complete_filter};
sup(List, {incomplete, {filter, '['} = Filter}) ->
    %?d(List),
    %?d(Filter),
    LS = last_selector(lists:reverse(List)),
    LastEntity = sel_entity(LS),
    {LastEntity, Filter};
sup(List, {incomplete, {filter, {incomplete, {Type, _FiltItem} = _Filter}}}) 
  when Type == 'or' ; Type == 'and'->
    %?d(List),
    %?d(Filter),
    %?d(FiltItem),
    LS = last_selector(lists:reverse(List)),
    LastEntity = sel_entity(LS),
    {LastEntity, {filter, '['}};
sup(List, {incomplete, {filter, {complete, {Type, _Lhs, Rhs}}}})
  when Type == 'or' ; Type == 'and'->
    %?d(Rhs),
    sup(List, {incomplete, {filter, Rhs}});
sup(List, {incomplete,{filter,{incomplete,{C, _Prop}=Comp}}}) 
  when C == 'comparator' ; Comp == 'like' ->
    %?d(List),
    %?d(Comp),
    LS = last_selector(lists:reverse(List)),
    LastEntity = sel_entity(LS),
    {LastEntity, Comp};
sup(_List,  {incomplete,{filter,{incomplete,{'not'}}}}) ->
    %?d(List),
    {filter, 'not'};
sup(_List, {incomplete,{filter,{complete,{_Comp, _, _}}}}) ->
    %?d(List),
    {filter, separator};
sup(_List, {incomplete,{filter,{complete,{_Comp, _}}}}) ->
    %?d(List),
    {filter, separator};
sup(List, {incomplete, {filter, _F} = Filter}) ->
    %?d(List),
    %?d(Filter),
    LS = last_selector(lists:reverse(List)),
    LastEntity = sel_entity(LS),
    {LastEntity, Filter};
% ---- closure
sup(List, {incomplete, {closure, '('}}) -> 
    %?d(List),
    sup(List, {incomplete, {'query', '.'}});
sup(_List, {incomplete, {closure, ')'}}) ->
    %?d(List),
    {closure, ending};
sup(List, {incomplete, {closure, [Item]}}) ->
    %?d(List),
    sup(List, Item);
sup(_List, {incomplete, {closure, SubList}}) ->
    %?d(List),
    separate_useful_parts(SubList) ;
% ---- statistics
sup(List, {complete, {statistics,Stat}}) ->
    %?d(List),
    LS = last_selector(lists:reverse(List)),
    LastEntity = sel_entity(LS),
    {LastEntity, {statistics, Stat}};
sup(List, {incomplete, {statistics,':'}=S}) ->
    %?d(List),
    LS = last_selector(lists:reverse(List)),
    LastEntity = sel_entity(LS),
    {LastEntity, S};
% ---- iteration
sup(List, {incomplete, {iteration, '{'}}) -> 
    %?d(List),
    sup(List, {incomplete, {'query', '.'}});
sup(_List, {incomplete, {iteration, '}'}}) ->
    %?d(List),
    {iteration, ending};
sup(List, {incomplete, {iteration, [Item]}}) ->
    %?d(List),
    sup(List, Item);
sup(_List, {incomplete, {iteration, SubList}}) ->
    %?d(List),
    separate_useful_parts(SubList).

%% @doc
%%  Finds the last selector that determines the entity of the following.
%%  his last selector should be a complete one. If it is an iteration or
%%  closure than we should recursively find the last and most inner one.
%% @end
last_selector([_LastItem | RevList]) ->
    %?d(RevList),
    Res = lists:dropwhile(fun({complete, {selector,_}}) -> false;
                             ({complete, {initial_selector,_}}) -> false;
                             ({complete, {closure, _, _}}) -> false;
                             ({complete, {iteration,_,_}}) -> false;
                             (_) -> true
                          end,  RevList),
    %?d(Res),
    case Res of
        [] ->
            %?d(RevList),
            unimplemented1;
        [{complete, {closure, {seq,I}, _}} | _] ->
            %?d(I),
            last_selector([skippable_item | lists:reverse(I)]);
        [{complete, {iteration, {seq,I}, _}} | _] ->
            %?d(I),
            last_selector([skippable_item | lists:reverse(I)]);
        [{complete, I} | _] ->
            %?d(I),
            I
    end.

%% @doc
%%  Selects the entity from refusr_sq_lib:entities().
%%  First selects the entity records than searching for the given 
%%  selector in the selectors list of the record.
%%  The result should be a one element list.
%% @end
sel_entity({initial_selector, Selector}) ->
    [Ent || {S, Ent} <- initial_selectors(), S==Selector]; 
sel_entity({selector, Selector}) ->
    lists:usort(lists:flatten([ [S#selector.type 
                                 || S <- E#entity.selectors,
                                    lists:member(Selector, S#selector.name)
                                ]
                                || E <- refusr_sq_lib:entities()
                              ])).
    
%% @doc
%%  Returns the possible postfixes of the query in a tuple format.
%%  First element contains the original prefix, the second element contains 
%%  the possible ending.
%% @end
complete(initial_selector, Selector) ->
    SelStr = atom_to_list(Selector),
    [{SelStr, lists:sublist(atom_to_list(S), 
                             length(SelStr) + 1, 
                             length(atom_to_list(S)))}
     || {S, _} <- initial_selectors(),
        lists:prefix(SelStr, atom_to_list(S))];
complete(LastEntity, {selector, Sel}) ->
    %?d(LastEntity),
    %?d(Sel),
    Sels = get_selectors(LastEntity),
    Filts = get_filters(LastEntity),
    SelStr = atom_to_list(Sel),
    lists:usort([{SelStr, lists:sublist(atom_to_list(S), 
                                        length(SelStr) + 1, 
                                        length(atom_to_list(S)))} 
                 || S <- Sels, lists:prefix(SelStr, atom_to_list(S))] 
                ++
                    [{SelStr, lists:sublist(atom_to_list(Fi), 
                              length(SelStr) + 1, 
                              length(atom_to_list(Fi)))} 
                     || Fi <- Filts, lists:prefix(SelStr, atom_to_list(Fi))]);
complete(closure, ending) ->
    [{type, int}, {"", "+"}];
complete(iteration, ending) ->
    [{type, int}];
complete(_LastEntity, complete_filter) ->
    [{"", "."}, {"", ":"}, {"", "["}];
complete(LastEntity, {filter, '['}) ->
    [{"", atom_to_list(F)} || F<-get_filters(LastEntity)];
complete([LastEntity], {comparator, Prop}) ->
    %?d(LastEntity),
    [RetEnt] = refusr_sq_lib:prop_type(LastEntity, Prop),
    [{type, RetEnt}];
complete(filter, separator) ->
    [{"", ";"}, {"", ","}];
complete(filter, 'not') ->
    [{"", "("}, {type, atom}, {type, string}, {type, int}];
complete(LastEntity, {filter, F}) ->
    %?d(LastEntity),
    %?d(F),
    Filts = get_filters(LastEntity),
    FStr = atom_to_list(F),
    [{FStr, lists:sublist(atom_to_list(Fi), 
                            length(FStr) + 1, 
                            length(atom_to_list(Fi)))} 
     || Fi <- Filts, lists:prefix(FStr, atom_to_list(Fi))];
complete(LastEntity, {statistics, ':'}) ->
    %?d(LastEntity),
    [{"", atom_to_list(S)} || S<-get_stats(LastEntity)];
complete(LastEntity, {statistics, S}) ->
    %?d(LastEntity),
    %?d(S),
    Stats = get_stats(LastEntity),
    SStr = atom_to_list(S),
    [{SStr, lists:sublist(atom_to_list(Si),
                            length(SStr) + 1,
                            length(atom_to_list(Si)))}
     || Si <- Stats, lists:prefix(SStr, atom_to_list(Si))].




%% @private
%% @doc 
%% Returns the selectors of a given entity
%% @end
get_selectors([Entity]) ->
   lists:usort(
     lists:flatten([ T#selector.name || 
                       T <- (refusr_sq_lib:entity(Entity))#entity.selectors])).

%% @private
%% @doc 
%% Returns the properties of a given entity
%% @end
get_filters([Entity]) ->    
    lists:usort(
     lists:flatten([ T#property.name || 
                       T <- (refusr_sq_lib:entity(Entity))#entity.properties])).

%% @private
%% @doc 
%% Returns the statistics of a given entity
%% @end
get_stats(_Entities) ->
    lists:usort(
      lists:flatten(
	[T#statistics.name || T <- refusr_sq_lib:statistics()])).


%%% =========================================================================
%% @doc
%% Validate a maybe-incomplete query whether it could be originally parsed with the smaller grammar.
%% tested with: "mods.(funs[not (name==(.called[name==\"alam\"]))])3.calls.calls.{called_by}3[name=korte]"
%% @end
validate_query(ParsedList) ->
    try 
        {ok, validate_query_2(ParsedList)}
    catch
        validate_error ->
            {error, "Syntax error in query!"}
    end.

validate_query_2(ParsedList) ->
    [ validate_item(I) || I <- ParsedList ].

validate_item({complete, {closure, {seq, L}, Mult}}) ->
    {closure, {seq, [ validate_item(I) || I <- L ]}, Mult};
validate_item({complete, {iteration, {seq, L}, Mult}}) ->
    {iteration, {seq, [ validate_item(I) || I <- L]}, Mult};
validate_item({complete, {filter, F}}) ->
    {filter, validate_filter(F)};
validate_item({complete, Item}) ->
    Item;
validate_item({incomplete, _}) ->
    throw(validate_error).

validate_filter({complete, {Type, E1, E2}}) ->
    {Type, validate_filter(E1), validate_filter(E2)};
validate_filter({complete, {seq, E}}) ->
    {seq, validate_query_2(E)};
validate_filter({complete, {'not', E}}) ->
    {'not', validate_filter(E)};
validate_filter({seq, E}) ->
    %?d(E),
    {seq, validate_query_2(E)};
validate_filter({quoted, _} = E) ->
    E;
validate_filter(E) when is_atom(E) or is_list(E) or is_integer(E) ->
    E.

%% ==============================================================================
initial_selectors() ->
    [{'@function', function},
     {'@fun',      function},
     {'@variable', variable},
     {'@var',      variable},
     {'@record',   record},
     {'@rec',      record},
     {'@recfield', field},
     {'@field',    field},
     {'@macro',    macro},
     {'@expression', expression},
     {'@expr',       expression},
     {'@module',     file},
     {'@mod',        file},
     {'modules',     file},
     {'mods',        file},
     {'@file',       file},
     {'files',       file}
                                                %todo what are the possibilities
                                                %'@definition', '@def' unsupported
    ].


