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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Cyclic depedency examination in module level.
%% Draws out the dependency graph. <br/>
%%% At first run a graph is used, which is stored into a dets table for further
%%% and faster use. <br/>X module is dependent from Y module: in the definition of a
%%% function of X there is a function call of a function of Y.
%% (X -> Y) <br/>For example, X -> Y -> Z -> X is a cycle. 
%%%
%%% @author Kinga Szava <guca04@gmail.com>

-module(refusr_cyclic_mod).
-vsn("$Rev: 8393 $ ").
-export([print_cycle/0, print_cycle/1, check_cycle/0, check_cycle/1, check_module/1]).

-export([get_relations/1]).

-export([draw/0, draw/1, draw_cycles/0, draw_cycles/1]).

-export([build_table/5]).

-export([get_mod_modcall/1]).


-include("user.hrl").

-define(LibRel, refusr_lib_relations).

%%% ============================================================================
%%% Building the dets table
%%
%% @spec build_table(Name, Args, Data, Exceptions, Leaves) -> ok
%% @doc Builds up the proper dets table from a built digraph.
%% Only for development use. (Data argument is only for the uniformity.)
build_table(Name, [], Exceptions, Leaves, _)->
	
    Graph = digraph:new(),
    try
        build_graph(Graph, Exceptions, Leaves),
        Cycles = cycles(Graph, digraph:vertices(Graph), [], []),
	?LibRel:build_paths(Graph, Name, Cycles, mod)
    after
        digraph:delete(Graph)
    end;
build_table(Name, Args, Exceptions, Leaves, _)->
    Graph = digraph:new(),
    try
         lists:foldl(fun(Arg, NewExceptions)-> 
			build_subgraph(Graph, Arg, NewExceptions, Leaves) end,
			Exceptions, Args),
        Cycles = cycles(Graph, digraph:vertices(Graph), [], []),
	?LibRel:build_paths(Graph, Name, Cycles, mod)
    after
        digraph:delete(Graph)
    end.

%% Exported Functions =============================================================
%% @spec check_cycle() -> {ok, no_cyclic_dependency} | {integer(), list()} | {error, ErrMsg::string(), ErrPars::list()}
%% @doc Checks for cycles in module level. If cycles are found then the following tuple is the result: {Number of
%% cycles, Cycle list}. If an error occurs then a tuple is thrown, which has to be caught by the interface functions.
check_cycle()->
	check_cycle([]).
	
check_cycle(ParList)->
  {_, Res} = check_module([{type, check} | ParList]),
  {integer_to_list(length(Res)) ++  " cycle(s)", Res}.

%% @spec check_module(Module)-> {integer(), list()} | false | {error, bad_module_name} | {error, ErrMsg::string(), ErrPars::list()}
%% Module = node() | string()
%% @doc Creates a subgraph from the given module as a starting node and checks for cycles.
%% <br/>The module can be given as a graph node or as its name.  If an error occurs then a tuple is thrown,
%% which has to be caught by the interface functions.
check_module([{_, _} | _ ] = Options)-> %% call from ri 
   output(?LibRel:invest_table(mod, Options));
check_module(FromNode)->
   output(?LibRel:invest_table(mod, [{type, check}, {gnode, FromNode}])).

output([])->
	{false, []};
output(Res)->
	{true, Res}.

%% @spec print_cycle() -> ok
%% @doc Prints the result of check_cycle/0 to the standard output.
print_cycle()->
   print_cycle([]).

print_cycle(FromNode)->
   ?LibRel:invest_table(mod, [{type, print}, {gnode, FromNode}]).




%% @spec get_relations(Modules)-> Relations
%% Modules = node() | [node() | atom() | string()]
%% Relations = {Module, Paths::lists(), Cycles::lists()}
%% @doc Prints out the relations and the cycles connected to a given module or modulelist.
%% The parameter modulelist has to be given in a list if the modules are marked by their name
%% as string.
get_relations({_, module, _} = Node)->
	get_relations([Node]);
get_relations(ParList) when is_list(ParList)->
	Res = ?LibRel:invest_table(mod,
			 [{type, get_rel}, {gnode, ParList}]),
	?LibRel:print_rels(Res, mod).

%% @spec draw()-> {ok, no_cyclic_dependency} | {integer(), list()}
%% @doc Equivalent to draw(""), prints out the entire graph. Output file is dep_mod.dot.
draw()->
  draw([{gnode, []}]).

%% @spec draw(Options::proplists())-> {ok, no_cyclic_dependency} | {integer(), list()} |
%% {error, bad_function_name} | erlang:error(error, Gnode)
%% @doc Creates a subgraph drawing from the directed graph from a given 
%% function as a starting node. The function can be given as a graph node or  in the following
%% order and combination: "module:name/arity" (eg.: "erlang:hd/1").<br/>
%% Options:
%% ```
%% {dot, DotFileName::string()}
%%
%% '''
%% User defined dot name.
%% ```
%% {gnode, Node | NodeList::lists(Nodes)}
%%  Node = node() | atom()
%% '''
%% Which module should be inspected.
%% Output file is dep_mod_cycles.dot or it can be given by the user.
draw([{_, _} | _] = Options)->
	do_draw(Options);
draw(Options) when not is_list(Options) ->
	do_draw([{gnode, Options}]);
draw(Options)->
	do_draw(Options).

do_draw(Options)->
           ?LibRel:invest_table(mod,[{type, draw} | Options]).

%% @spec draw_cycles()-> {ok, no_cyclic_dependency} | {integer(), list()}
%% @doc Prints out a subgraph which contains the cycle(s).
%% Unless cycles exist, calls {@link draw/0}.
%% Output file is dep_mod_cycles.dot.
draw_cycles()->
    draw_cycles([{gnode, []}]).

%% @spec draw_cycles(PropList::proplists())-> {ok, no_cyclic_dependency} | {integer(), list()} |
%% {error, bad_function_name} | erlang:error(error, Gnode)
%% @doc Prints out a subgraph which contains the cycle(s) from the given node.
%% Unless cycles exist, calls {@link draw/0}.
%% Output file is dep_mod_cycles.dot or user_defined.
%% Options:
%% ```
%% {dot, DotFileName::string()}
%%
%% '''
%% User defined dot name.
%% ```
%% {gnode, Node::node() | atom()}
%% '''
%% Which module should be inspected.
%% Output file is dep_mod_cycles.dot or user_defined.
draw_cycles(PropList)->
     ?LibRel:invest_table(mod,[{type, cycles} | PropList]).

%%% =========================================================================
%%% Queries
get_all_modules()->
   ?Query:exec(?Mod:all()).

get_modcalls(Mod) when not is_list(Mod)->
	get_modcalls([Mod]);
get_modcalls(Mods)->
   [{Mod,  get_mod_modcall(Mod)}
     		|| Mod<-Mods].

get_mod_modcall(Mod)->
     lists:usort([get_func_mod(Called)
     ||Called <- get_mod_funcall(Mod),
       get_func_mod(Called) /= Mod ]).

get_mod_funcall({_, Type, _}) when Type /= module->
	throw({error, "Error, bad argument given, a module type gnode should be the argument!~n"});
get_mod_funcall(Mod)->
    lists:flatten([Funcalls
			      || Fun<-?Query:exec(Mod, ?Mod:locals()),
				 Funcalls <- refusr_cyclic_fun:funcalls(Fun),
				 Funcalls /= []]).



get_func_mod(Fun)->
    ?Query:exec1(Fun, ?Fun:module(), mod_not_found).

%%=============================================================================
build_graph(Graph, Exceptions, Leaves)->
    AllMods = get_modcalls(get_all_modules()),
    build_vertex(Graph, AllMods, Exceptions, Leaves).

build_vertex(_, [], Exceptions, _)-> Exceptions;
%%build_vertex(Graph, Modules, [], Leaves)->
%%	build_vertex(Graph, Modules, {[], []}, Leaves);
build_vertex(Graph, [{Mod, ModCalls} | Tail], {Exceptions, Otp}, Leaves)->
	%%io:format("Mod: ~p, ModCalls: ~p, Tail:~p,  Exceptions: ~p, Leaves: ~p~n", [Mod, ModCalls, Tail, Exceptions, Leaves]),
    Bool = lists:member(Mod, Exceptions) or is_otp_member(?Graph:data(Mod), Otp),
    NewExceptions = case Bool of
			true ->
				Exceptions++ModCalls;
			false -> check_vertex(Graph, Mod),
				case lists:member(Mod, Leaves) of
    					true -> 
						Exceptions ++ ModCalls; %% when a leaf is found, it means that its children become exceptions for further analysis
					false -> 
    						[begin
         						check_vertex(Graph, Call),
         						check_edge(Graph, Mod, Call)
     						 end
     						|| Call <- ModCalls,
						  not lists:member(Call, Exceptions),
						  not is_otp_member(?Graph:data(Call), Otp)],
						Exceptions
				end
	 	end,
    build_vertex(Graph, Tail, {NewExceptions, Otp}, Leaves).

is_otp_member({module, Name}, Otp)->
	lists:member(Name, Otp).
    

check_vertex(Graph, Vertex)->
     case  digraph:vertex(Graph, Vertex) of
			      false -> digraph:add_vertex(Graph, Vertex);
			      _ -> ok
     end.

check_edge(Graph, Vertex1, Vertex2)->
    case edge(Graph, Vertex1, Vertex2) of
	[] ->
	     digraph:add_edge(Graph, Vertex1, Vertex2);
	_ -> ok
    end.
    
edge(Graph, Vertex1, Vertex2)->
    [digraph:edge(Graph, Edge)
     || Edge <- ?MISC:intersect(digraph:out_edges(Graph, Vertex1),
				digraph:in_edges(Graph, Vertex2))].

build_subgraph(Graph, Node, Exceptions, Leaves)->
    case get_modcalls(Node) of
	[] -> check_exceptions(Graph, Node, Exceptions);
	[{Mod, CalledList}]->  
	    case lists:subtract(CalledList, digraph:vertices(Graph)) of
		[] ->  build_vertex(Graph, [{Mod, CalledList}], Exceptions, Leaves);
		NewVertices -> 	NewExceptions = build_vertex(Graph, [{Mod, CalledList}], Exceptions, Leaves),
				update_exceptions_subgraph(Graph, NewVertices, NewExceptions, Leaves)
	    end
    end.


check_exceptions(Graph, {_, _, _} = Node, {Exceptions, Otp})->
	Bool = lists:member(Node, Exceptions) or lists:member(Node, Otp),
	case Bool of
		true->
			ok;
		false->
			check_vertex(Graph, Node)
	end;
check_exceptions(_Graph, [], _Exceptions)->
	ok;
check_exceptions(Graph, NodeList, Exceptions) when is_list(NodeList)->
	[check_exceptions(Graph, Node, Exceptions) || Node <- NodeList].


update_exceptions_subgraph(_, [], _, _)->
	{[], []};
update_exceptions_subgraph(Graph, [Node | Tail], Exceptions, Leaves)->
	NewExceptions = build_subgraph(Graph, [Node], Exceptions, Leaves),
	update_exceptions_subgraph(Graph, Tail, NewExceptions, Leaves).

cycles(Graph, [], Cycles, Edges)-> 
    [digraph:add_edge(Graph, E, V1, V2, Label) ||
	{E, V1, V2, Label} <- Edges],
    Cycles;
cycles(Graph, [Head|Tail] = Vertices, Cycles, Edges)->
    case digraph:get_cycle(Graph, Head) of
	false -> cycles(Graph, Tail, Cycles, Edges);
	[_  = Loop| []]  ->
	    cycles(Graph, Tail, [ [Loop, Loop] | Cycles], Edges);
	Cycle ->
	    [{E, _, _, _} = Edge | []] =
		edge(Graph, lists:nth(length(Cycle)-1, Cycle), Head),
	    digraph:del_edge(Graph, E),
	    cycles(Graph, Vertices, is_cycle(Cycles, Cycle,
					     lists:member(Cycle, Cycles)),
		   [ Edge| Edges])
    end.

is_cycle(Cycles, _, true)->
    Cycles;
is_cycle(Cycles, Cycle, false) ->
    [Cycle | Cycles].





 

