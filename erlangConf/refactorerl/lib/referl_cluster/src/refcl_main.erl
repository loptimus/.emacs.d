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

%%% @doc This module provides interface functions for clustering. The different
%%% user interfaces can use this transformation to calculate clustering. There
%%% are two algorithms implemented for clustering:
%%% <ul>
%%%     <li>Agglomerative algorithm: which can be used for module and function
%%%         clustering too.</li>
%%%     <li>Genetic algorithm: which can be used for module clustering </li>
%%% </ul>
%%%
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>
%%% @author Gabor Horvath <tyros3@gmail.com>

-module(refcl_main).
-vsn("$Rev: 8230 $"). %"

-export([prepare/1, refresh/0]).

-include_lib("referl_cluster/include/cluster.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(TBNAME,cl_ui).
-record(?TBNAME,{id = 0, options, fittnum, result}).

%% Interface
%%----------------------------------------------------------------

% @todo document
prepare(Args) ->
    Algorithm = ?Args:algorithm(Args),
    %%Entity = ?Args:entity(Args),
    {AlgLabel, Entity} =
        case Algorithm of
            agglom_attr -> {"Agglomerative", ?Args:entity(Args)};
            genetic     -> {"Genetic", module}
        end,

    Data = lists:zip(cl_interface:run_cluster_labels(Algorithm),
                     cl_interface:run_cluster_default(Algorithm)),
    
    {EntLabel, ToAsk} =
        case Entity of
            module   ->
                TLV = [{T, L, V} || {{T, L}, {T, V}} <- Data,
                                    T =/= skip_functions],
                
                {"Module", TLV};
            function ->
                TLV = [{T, L, V} || {{T, L}, {T, V}} <- Data,
                                    T =/= skip_modules],
                
                {"Function", TLV}
        end,
    
    Qu = [[{format,info}, {text, EntLabel ++ " clustering with " ++
                               AlgLabel ++ " algorithm"}]],

    Question = Qu ++ add_to_question(ToAsk) ++
        [[{format,checkbox}, {text,"Save results to database:"},
          {default,false}]],

    Ans = lists:reverse(tl(?Transform:validated_question(Question))),

    CreateDb = case hd(Ans) of
                   yes -> true;
                   _   -> false
               end,

    FinalOpt = [{alg,Algorithm},{entities, Entity}, {entity_type,Entity}] ++
                get_options(lists:reverse(tl(Ans)), ToAsk),

    fun() ->
        Clusterings = cl_interface:run_cluster([{log_output, null}]++FinalOpt),

        FittOptions = [{mq, first_version}, {entity_type, Entity},
                       {entities, #which_entities{}}],
        Fitt_Num    = cl_interface:fitness([{clusterings, Clusterings},
                                 {fitness_options, FittOptions}]),

        case CreateDb of
            true  -> store_result(FinalOpt, Fitt_Num, Clusterings);
                  %% case table_handler() of
                  %%     {table, _} ->
                  %%         store_result(FinalOpt, Fitt_Num, Clusterings);
                  %%     _          -> throw({error, cl_ui_crashed})
                  %% end;
            false -> ok
        end,

        % Create output from clustering and fitness numbers
        Res = [[{format,info},{text,"Clustering results:"}]],
        ClusterOutput = [add_to_result(Cluster) || Cluster <- Clusterings],
        FittnessOutput = [[{format,info},{text,"Fitness Numbers:"}],
                          add_to_result(Fitt_Num)],
        Result = Res ++ ClusterOutput ++ FittnessOutput,

        ?Transform:question(Result),

        [Clusterings,Fitt_Num]
    end.

%% Private
%%----------------------------------------------------------------

add_to_question([]) -> [];

add_to_question([{T, L, V} | TagsLabelsValues]) ->
    [] ++ add_to_question({T, L}, V) ++ add_to_question(TagsLabelsValues).

add_to_question({Tag, Label}, DefValue) ->
    Validator = proplists:get_value(Tag, cl_interface:run_cluster_check_fun()),
    case Tag of
        transformfun ->
            add_to_question(select, Label, [none, zero_one], Validator);
        distfun      ->
            add_to_question(select, Label, [call_sum, weight], Validator);
        _            ->
            case DefValue of
                undefined -> add_to_question(type, Label, none, Validator);
                _         -> add_to_question(type, Label, DefValue, Validator)
            end
    end.

add_to_question(type, Label, DefValue, Validator) ->
    DefValueStr = io_lib:fwrite("~p",[DefValue]),
    [[{format,textbox}, 
      {text,Label ++ "(Type: " ++ DefValueStr ++ " for default)"},
      {validator, Validator},
      {default,-1}]];

add_to_question(select, Label, Values, Validator) ->
    %% DefValueStr = io_lib:fwrite("~p",[Values]),
    %% [[{format, info}, {text, Label ++ ":"}]] ++
    %%     [[{format, radio}, {text, io_lib:fwrite("~p",[Value])},
    %%       {default, false}] || Value <- Values].
    DefValueStr = io_lib:fwrite("~p",[Values]),
    [[{format,textbox}, {text,Label ++ "(Select: " ++ DefValueStr ++ ")"},
      {validator, Validator},
      {default,-1}]].

add_to_result(List) ->
    Text = io_lib:fwrite("~p", [List]),
    [{format,info},{text,Text}].

get_options(Answers, Questions) ->
    {Tags, _Labels, DefValues} = lists:unzip3(Questions),
    %%Opts    = correct_opt(Answer, DefValues),
    [{Tag, get_option(Value, DefVal)}
     || {Tag, Value, DefVal} <- lists:zip3(Tags, Answers, DefValues)].

get_option(undefined, DefVal) -> DefVal;
get_option(Value, _DefVal) -> Value.

%% convert(Value, DefValue) ->
%%     case Value of
%%         [] -> DefValue;
%%         _  ->
%%             try
%%                 case w_type(DefValue) of
%%                     float    -> ensure_float(Value);
%%                     int      -> ensure_integer(Value);
%%                     %% TODO:Need a branch for the [empty] lists
%%                     %% for example this term {nil, []} will be [nil ] on
%%                     %% the side of the emacs interface
%%                     %% The empty string is equivalent with the empty list
%%                     atom     -> list_to_atom(Value);
%%                     function -> not_implemented;
%%                     _        -> Value
%%                 end
%%             catch
%%                 _:_ -> 
%%                     reflib_ui:message(error,"Invalid type ~s", [Value]),
%%                     DefValue
%%             end
%%     end.

%% ensure_float(String) ->
%%     case erl_scan:string(String) of
%%         {ok, [{float, _, V}], _} ->
%%             V;
%%         {ok, [{integer, _, V}], _} ->
%%             V/1.0
%%     end.

%% ensure_integer(String) ->
%%     case erl_scan:string(String) of
%%         {ok, [{float, _, V}], _} ->
%%             round(V);
%%         {ok, [{integer, _, V}], _} ->
%%             V
%%     end.
        
    

%% w_type(T)->
%%     if
%%         is_atom(T)    -> atom;
%%         is_integer(T) -> int;
%%         is_float(T)   -> float;
%%         is_function(T)-> function;
%%         true          -> string
%%     end.

%% correct_opt(Opt,Term)->
%%     [valid(O,T) || {O,T} <- lists:zip(Opt,Term)].

%% valid([], Term) -> Term;
%% valid(Op, _   ) -> Op.

store_result(Opt, Fitt, Cl_res)->
    Qdc = qlc:q([Id || {_, Id, _, _, _} <-mnesia:table(?TBNAME)]),
    Qds = mnesia:async_dirty(fun()-> qlc:e(Qdc)  end),
    case Qds of
        [] -> Id = 0;
         _ -> Id = lists:max(Qds)
    end,
    Record=#cl_ui{id = Id + 1,
                  options=Opt,
                  fittnum = Fitt,
                  result = Cl_res},
    mnesia:dirty_write(?TBNAME, Record).

exists_table(Tab)->
    try
        mnesia:table_info(Tab,arity),
        {Tab,exists}
    catch
        _:_ -> {Tab, noexists}
    end.

table_handler()->
    case exists_table(?TBNAME) of
        {?TBNAME, exists}   -> {table, exists};
        {?TBNAME, noexists} ->
            mnesia:create_table(?TBNAME,
                                [{attributes,record_info(fields,?TBNAME)},
                                 {disc_copies,[node()]},
                                 {type, bag}]),
            {table, created}%%;
       %%_ -> throw({error, cl_ui_table})
       end.

%%% @doc Refreshing the mnesia tables. This means deleting the existing tables.
refresh()->
    case exists_table(?TBNAME) of
       {?TBNAME, exists}   ->
            mnesia:delete_table(?TBNAME),
            {cl_ui, recreated};
       {?TBNAME, noexists} ->
            {cl_ui, noexec}
    end.
