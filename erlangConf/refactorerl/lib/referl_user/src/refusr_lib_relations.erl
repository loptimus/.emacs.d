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

%%% @doc "Abstract" module for dependency investigations like circle-detection, 
%%% and relationship representation (refusr_{cyclic_fun, cyclic_mod, fb_relations})
%%%
%%% @author Kinga Szava <guca04@gmail.com>

-module(refusr_lib_relations).
-vsn("$Rev: 8390 $ ").

-export([invest_table/2, build_paths/4]).

-export([verify_node/2, print_names/2, print_rels/2]).


-export([list_otp_modules/0]).

-export([get_func_mod/1]).

-include("user.hrl").

-record(row, {number,
	      element,
	      relations,
	      cycles = []
	     }).

-record(save, {number,
	       level,
	       args = [],
	       exceptions = {[], []},
	       leaves = [],
	       file}).

%% ==========================================================================
%% Queries
get_func_mod(Fun)->
    ?Query:exec1(Fun, ?Fun:module(), {mod_not_found, Fun}).

get_all_modules()->
   ?Query:exec(?Mod:all()).


%% =============================================================================
%% @spec invest_table(Type::atom(), PropList::proplists()) -> any()
%% @doc Task (check_cycle, draw, print_table) handling. Interface for other modules.
invest_table(Type, PropList)->
	proper_query(Type, PropList,
		     check_db_change()).

%% Opens proper dets table, fills it, saves it
proper_query(Type, PropList, true)->
	delete_results(),
	proper_query(Type, PropList, false);
proper_query(Type, PropList, false)->
    Args = proper_args(proplists:get_value(
			      gnode, PropList), Type),
    Exceptions = get_exceptions(Type, PropList),	
    Leaves = verifyList(Type, proplists:get_value(leaves, PropList)),
    case check_table(Type, Args, Exceptions, Leaves) of
	{{Table, true}, _, NewArgs} -> %%already built,
	    funQuery(Table, NewArgs, Type,
		     proplists:get_value(type, PropList),
		     PropList, false);
	{{Table, false}, Module, NewArgs}-> %%have to build it
	    Res = Module:build_table(Table, NewArgs, Exceptions, Leaves,
				     {proplists:get_value(custom, PropList), true}),
	    try
		funQuery(Table, NewArgs, Type,
			 proplists:get_value(type, PropList),
			 PropList, Res)
	    catch
		no_mod ->
			  throw({error, "Error. There are no modules in the database!~n", []})
	    after
		dets:close(Table)
	    end			 
    end.

get_exceptions(fb_rel, _)->
	[];
get_exceptions(Type, PropList)->
	{verifyList(Type, proplists:get_value(exception, PropList)),
		 exclude_otp(proplists:get_value(otp, PropList))}.

exclude_otp(undefined)->
	[];
exclude_otp(false)->
	[];
exclude_otp(true)->
	list_otp_modules().

verifyList(_, undefined)->
	[];
verifyList(Type, [Hd | _] = ListofList) when is_list(Hd)->
	[verifyList(Type, List) || List <- ListofList];
verifyList(Type, List)->
	[verify_node(Next, Type) || Next <- List].

proper_args(undefined, _)->
	[];
proper_args(Args, Type) when is_list(Args)->
	[check_type(Arg, Type) || Arg <- Args],
	Args;
proper_args(Args, Type) ->
	check_type(Args, Type),
	[Args].

check_type({_, module, _}, mod)->
	true;
check_type({_, func, _}, func)->
	true;
check_type({_, _, _}, Type)->
	throw({error, "The given node is not a proper ~p type node! ~n" , [Type]});
check_type(_ , _)->
	true.


%%----------------------------------------------
%% Handles the different tasks
funQuery(Table, _, _, check,_, false)->
    check_cycle(Table);
funQuery(Table, Args, Type, print, _, false)->
    print_cycle(Table, Args, Type,false);
funQuery(Table, Args, Type, draw, PropList, false) ->
    DotName = get_dot_name(proplists:get_value(dot, PropList), Type, Args),
    draw(Table, Args, Type, DotName, false);
funQuery(Table, Args, Type, cycles, PropList, false) ->
    DotName = get_dot_name(proplists:get_value(dot, PropList), Type, Args),
    draw_cycles(Table, Args, Type, DotName, false);
funQuery(_, _, _, check, _, Res)->
    Res;
funQuery(Table, Args, Type, print, _, Res)->
    print_cycle(Table, Args, Type, Res);
funQuery(Table, [], Type, get_rel, _, _)->
       print_table(Table, [], Type); 
funQuery(Table, Args, Type, get_rel, _, _)->
       print_table(Table, Args, Type); 
funQuery(Table, Args, Type, draw, PropList, Res) ->
    DotName = get_dot_name(proplists:get_value(dot, PropList), Type, Args),
    draw(Table, Args, Type, DotName, Res);
funQuery(Table, Args, Type, cycles, PropList, Res) ->
    DotName = get_dot_name(proplists:get_value(dot, PropList), Type, Args),
    draw_cycles(Table, Args, Type, DotName, Res).

%%-----------------------------------------
%% Check for proper table name, and its existence
check_table(Type, Args, Exceptions, Leaves)->
       {File, Module, NewArgs} = 
	                         get_file_name(Type,
	                   			 Args, Exceptions, Leaves),
       {table_open(File), Module, NewArgs}.


get_file_name(func, [], [], [])->
    {"cyclic_fun", refusr_cyclic_fun, []};
get_file_name(mod, [], [], []) ->
    {"cyclic_mod", refusr_cyclic_mod, []};
get_file_name(func, [],  Exceptions, Leaves)->
    File = saved_files(func, [],
			   table_exists(func, [], Exceptions, Leaves),
			   Exceptions, Leaves),
    {File, refusr_cyclic_fun, []};
get_file_name(mod, [], Exceptions, Leaves) ->
    File = saved_files(mod, [],
			   table_exists(mod, [], Exceptions, Leaves),
			   Exceptions, Leaves),
    {File, refusr_cyclic_mod, []};
get_file_name(fb_rel, [], _, _) ->
    {"fb_rel", refusr_fb_relations, []};
get_file_name(fb_rel, [{Args}], Exceptions, Leaves) when is_list(Args)-> %%case from refusr_fb_regexp when fb key was defined
	File = saved_files(fb_rel, Args,
			   table_exists(fb_rel, Args, Exceptions, Leaves),
			   Exceptions, Leaves),
	{File, element(2, get_file_name(fb_rel, [], Exceptions, Leaves)), Args};
get_file_name(fb_rel, Args, Exceptions, Leaves)->
		NewArgList = [verify_node(Next, fb_rel)|| Next <- Args],
		File = saved_files(fb_rel, Args,
				   table_exists(fb_rel, NewArgList, Exceptions, Leaves),
				   Exceptions, Leaves),
		{File, element(2, get_file_name(fb_rel, [], Exceptions, Leaves)), NewArgList};
get_file_name(Type, [Hd | _ ] = Args, Exceptions, Leaves) when is_tuple(Hd) or is_atom(Hd)->		
		NewArgList = lists:flatten([verify_node(Next, Type)|| Next <- Args]),
		File = saved_files(Type, Args,
				   table_exists(Type, NewArgList, Exceptions, Leaves),
				   Exceptions, Leaves),
		{File, element(2, get_file_name(Type, [], Exceptions, Leaves)), NewArgList};
get_file_name(Type, Args, Exceptions, Leaves)->
		NewArgList = [verify_node(Next, Type)|| Next <- Args],
		File = saved_files(Type, Args,
				   table_exists(Type, NewArgList, Exceptions, Leaves),
				   Exceptions, Leaves),
		{File, element(2, get_file_name(Type, [], Exceptions, Leaves)), NewArgList}.

saved_files(_, _, {File, true}, _, _)->
	File;
%% this is needed, because when there is a complete save, 
%% maybe the arguments fb-s won't be complete, so at first run get_relations/1 will fail
saved_files(fb_rel, _, {File, _}, _, _)-> 
	File;
saved_files(Type, ArgsList, {File, false}, Exceptions, Leaves)->
	case complete_table( element(1, get_file_name(Type, [], Exceptions, Leaves)), ArgsList) of
		false ->
			File;
		Data ->
			{Ref, _} = table_open(File),
			[dets:insert(Ref, Next) || Next <- Data],
			dets:close(Ref),
			File	
	end.
	

table_exists(Type, ArgList, Exceptions, Leaves)->
	{Save, _} = Res = table_open("saved_queries"),
	try
		check_saved_file(Res,Type,ArgList, Exceptions, Leaves)
	after
	    dets:close(Save)
	end.

check_saved_file({Table, true}, Level, Args, Exceptions, Leaves)->	
	case dets:match(Table, #save{number = '_',
				    level = Level,
				    args = Args,
				    exceptions = Exceptions,
				    leaves = Leaves,
				    file = '$1'}) of
		[] ->
			{add_new_save(Table, Level, Args, Exceptions, Leaves), false};
		[File | _] ->
			{File, true}
	end;
check_saved_file({Table, false}, Level, Args, Exceptions, Leaves)->
	{add_new_save(Table, Level, Args, Exceptions, Leaves), false}.


add_new_save(Table, Level, Args, Exceptions, Leaves)->
	{_, N} = lists:nth(3, dets:info(Table)),
	Filename = atom_to_list(Level) ++ integer_to_list(N+1),
	dets:insert(Table, #save{number = N+1,
			         level = Level,
			         args = Args,
	                         exceptions = Exceptions,
				 leaves = Leaves,
			         file = Filename}),
	Filename.

%% Checks whether there is a saved dets for the entire database
complete_table(Table, ArgsList)->
	Name = check_os() ++ Table,
	case dets:open_file(Name) of
	   {ok, Ref} ->
		Res = args_elements(Ref, ArgsList, []),
		dets:close(Ref),
		Res;
	   {error, _} ->
	    	false
        end.

args_elements(_, [], Result)->
	Result ;
args_elements(Table, [Hd | Rest], Result)->
	args_elements(Table, Rest, 
		get_every_element(Table,
			 match_object(Table, Hd), Result)).

get_every_element(_, 
	#row{number = _, element = _, 
		relations = [], cycles = _} , 
		Result)->
		Result;
get_every_element(Table, 
	#row{number = _, element = _, 
		relations = [Hd | Rest], cycles = _} = Row, 
		Result)->
		case lists:member(Row, Result)	of
			false ->
				relations(Table,
				 match_object(Table, Hd), Rest, [Row | Result]);
			true ->
				Result
			end.

relations(Table, Row, [], Result)->
	get_every_element(Table, Row, Result);
relations(Table, Row, [Hd | Rest], Result)->
	relations(Table, match_object(Table, Hd), Rest,
	           get_every_element(Table, Row, Result)).
	
match_object(Table, Element)->
	match_check( dets:match_object(Table, #row{number='$1',
					   element = Element,
					   relations = '$2',
					   cycles = '$3'})).

%% Opens table, if it is not present, creates it
table_open(Table)->
        Name = check_os() ++ Table,
	case dets:open_file(Name) of
	{ok, Ref} ->
	    io:format("~p table already built.~n", [Table]),
	    {Ref, true};
	{error, _} ->
	    io:format("Building ~p table...~n", [Table]),
	    dets:open_file(Table, [{type, bag}, {file, Name}]),
	    {Table, false}
        end.


%% Creates proper path according to the operating system
check_os()->
    case filelib:ensure_dir(Path = 
				filename:join([?MISC:data_dir(), "dep_files"]) ++ "/") of
	ok ->
		Path;
	{error, Reason}->
		Err = "Error with the dep_files directory. Reason: " ++ file:format_error(Reason),
		throw({error, Err, []})
    end.

%% Checks whether the database was changed
check_db_change()->
    File = check_os() ++ "db_hash.txt",
    case file:open(File, [read, write]) of
	{ok, Dev}->
	        try
	    	   read_hash(Dev, file:read_line(Dev))
		catch
		 {error, Error} ->
			Err = "Error writing db_hash.txt, reason:" ++ file:format_error(Error),
			throw({error, Err, []})
		after
	    	   file:close(Dev)
		end;
        {error, Reason}->
	    Err = "Error while opening file. " ++ file:format_error(Reason),
	    throw({error, Err, []})
    end.	

read_hash(File, eof)->
	write_hash(File, ?MISC:database_hash()),
	true;
read_hash(File, {ok, Hash})->
	FHash = list_to_integer(Hash),
	case ?MISC:database_hash() of
		FHash ->
			false;
		NewHash ->
			write_hash(File, NewHash),
			true
	end;
read_hash(File, {error, Error})->
	io:format("Error with hash file, new hash
		 added, reason: ~p~n", [Error]),
	write_hash(File, ?MISC:database_hash()),
	true.

write_hash(File, Hash)->
	  io:format(File, "~p", [Hash]).
	

list_otp_modules()->
	{Ref, Bool} = table_open("otp"),
	try
		case Bool of
			true ->
				[Module ||
					 [{Module} | _] <- dets:match(Ref, '$1')];
			false 	 ->
                                dets:insert(Ref, {erlang}),
				[begin
					dets:insert(Ref, {Module}),
					Module
				 end
					 || {Module, Path} <- code:all_loaded(),
			                                      is_otp(Path) == true] ++ [erlang]
		end
	after
		dets:close(Ref)
	end.

is_otp(cover_compiled)->
	false;
is_otp(preloaded) ->
	true;
is_otp(Path)->
	is_otp(Path, string:str(filename:nativename(Path),
		       code:lib_dir())).

is_otp(_, 0)->
	false;
is_otp(_, _)->
	true.
%%-----------------------------------------------------------------------
%% Deletes the ./dep_files/ directory because the database changed
delete_results()->
	io:format("Earlier results deleted (except .dot files and otp table).~n"),
	try
	    del_files(file:list_dir(check_os()))
	catch
	    {del_file, Error, File} ->
		Err = "Error with deleting file ~p, reason: " ++ file:format_error(Error),
		throw({error, Err, [File]});
	    {list_dir, Error, _} ->
		Err = "Error with listing dep_files directory, reason:" ++ file:format_error(Error),
		throw({error, Err, []})
	end.

del_files([])->
	ok;
del_files(["db_hash.txt" |  Rest])->
	del_files(Rest);
del_files(["otp" | Rest])->
	del_files(Rest);
del_files([Hd | Rest])->
    case lists:suffix(".dot", Hd) of
	true ->
	    del_files(Rest);
	false ->
	    case file:delete(check_os() ++ Hd) of
		ok ->
		    del_files(Rest);
		{error, Error}->
		    throw({del_file, "Error file deleting file: " ++ file:format_error(Error), []})
	    end
    end;
del_files({ok, Files})->
    del_files(Files);
del_files({error, Error})->
	throw({list_dir, "Error file deleting file: " ++ file:format_error(Error), []}).


%% Makes proper .dot file name
get_dot_name(undefined, func, _)->
    check_os() ++ "dep_cyclic.dot";
get_dot_name(undefined, mod, _)->
    check_os() ++ "dep_mod.dot";
get_dot_name(undefined, Type, [Node | []])->
	get_dot_name(undefined, Type, Node);
get_dot_name(undefined, func, {_, Name, Arity, Mod})->
    check_os() ++
	make_file_name({Mod,
			Name,
			Arity}, func);
get_dot_name(undefined, mod, {module, Name})->
    check_os() ++ make_file_name(Name, mod);
get_dot_name(undefined, fb_rel, _)->
    check_os() ++ "fb_relations.dot";
get_dot_name(Name, Type, Node) when is_atom(Name)->
	get_dot_name(atom_to_list(Name), Type, Node);
get_dot_name(Name, Type, Node)->
	case valid_path(Name) of
	    true ->
		 Name;
	    false ->
		case filename:extension(Name) of
		    ".dot" ->
			check_os() ++ Name;
		    []->
			io:format("~p doesn't exist. Predefined .dot
                            name in ./dep_files/ directory~n", [Name]),
			get_dot_name(undefined, Type, Node)
		end
	end.

valid_path(Name)->
    case filename:dirname(Name) of
	"." -> false;
	Dir ->
		 filelib:is_dir(Dir)
    end.

make_file_name({Mod, Func, Arity}, func)->
    "dep_func_" ++ atom_to_list(Mod) ++ "_" 
	++ atom_to_list(Func)
	++ "_" ++ integer_to_list(Arity) ++".dot";
make_file_name(Name, mod)->
   "dep_mod_" ++ atom_to_list(Name) ++ ".dot".
    
%% Formats the result
format_res(Res)->
   {integer_to_list(length(Res)) ++  " cycle(s)", Res}.

%%=====================================================
%% @spec build_paths(Graph::digraph(), Dets::dets(), Cycles::list(), Type::atom()) -> Cycles::list()
%% @doc Fills the dets table. Interface for other modules.
build_paths(Graph, Dets, Cycles, fb_rel)->
	Vertices = digraph:vertices(Graph),
	[begin
		Row = #row{number=get_number(fb_rel, N), 
			   element=V, 
			   relations=digraph:out_neighbours(Graph, V),
			   cycles=vertex_cycles(Cycles, V, [])},
		dets:insert(Dets, Row)
	 end 
		|| {V, N} <-lists:zip(Vertices,
						    lists:seq(1,
							 length(Vertices)))],

	Cycles;
build_paths(Graph, Dets, Cycles, Type)->
	Vertices = digraph:vertices(Graph),
	[dets:insert(Dets, #row{number=get_number(Type, N), 
			   element=V, 
			   relations=digraph:out_neighbours(Graph, V),
			   cycles=vertex_cycles(Cycles, V, [])})||
				 {V, N} <-lists:zip(Vertices,
						    lists:seq(1,
							 length(Vertices)))],
	Cycles.
	
get_number(fb_rel, N)->
	N;
get_number(_, _)->
	0.

vertex_cycles([], _, Result)->
	Result;
vertex_cycles([[V |_] = Hd | Rest], V, Result)->
	vertex_cycles(Rest, V, [Hd | Result]);
vertex_cycles([_| Rest], V, Result)->
        vertex_cycles(Rest, V, Result).

get_cycles(Table)->
    Type = case dets:info(Table) of
	       undefined ->
		      ets;
	       _  ->
		   dets
	   end,
	Res  = Type:match(Table, #row{number = '_', 
			  element = '_',
			  relations='_',
			  cycles = '$1'}),
	plus_cycle(Res, []).

plus_cycle([], Result)->
	Result;
plus_cycle([[Hd | []] | Rest], Result)->
	plus_cycle(Rest, plus_cycle2(Hd, Result)).

plus_cycle2([], Result)->
	Result;
plus_cycle2([Hd | Rest], Result)->
	case lists:member(Hd, Result) of
		true->
			plus_cycle2(Rest, Result);
		false ->
			plus_cycle2(Rest, [Hd | Result])
	end.

%%================================================
%% Checks for cycles.
check_cycle(Table)->
   elim_dupl_cycles(get_cycles(Table)).

elim_dupl_cycles(Cycles)->
    sameCycles( headTail(Cycles), []).

sameCycles([], Res)->
    Res;
sameCycles([Hd | Rest], Res)->
    Cycles =  [reconstruct(Hd, N)
		    || N <- lists:seq(1, length(Hd)-2)],
    sameCycles(Rest -- Cycles, [Hd | Res]).

      
headTail(Cycles)->
    [[Hd | Rest] || [Hd | Rest] <- Cycles,
		    lists:last(Rest) == Hd].

reconstruct(Element, N)->
    {[_ |List1], [Hd | _] = List2} = lists:split(N, Element),
    List2 ++ List1 ++ [Hd]. 

%% ====================================================================================
%% Prints the entire table
print_table(Name, [], _)->
    dets:match(Name, '$1');
print_table(Name, List, Type) ->
     get_rows(Name, List, [], Type).


get_rows(_, [], Result, _)->
    Result;
get_rows(Table, List, _, fb_rel) ->
	try
	   DetsList = dets:match_object(Table, #row{number = '$1', 
						element = '$2', 
						relations = '$3',
						cycles = '_'}),
	   [Dets ||  Next <- List, 
		      #row{number = _,
			 element = Element,
			 relations = _} = Dets <- DetsList,
		 	   lists:prefix(Next, Element)]
	catch
	   {badarg, BadArg} -> throw({error, "Error while matching in dets table, argument: ~p~n ", [BadArg]})
       end;
get_rows(Table, [Hd | Rest], Result, Type)->
	try
           verify_node(Hd, Type), 
	    get_rows(Table, Rest,
		     [match_check(
				dets:match_object(Table, 
					   #row{number = '$1', 
						element = Hd, 
						relations = '$2',
						cycles = '_'}))
			 | Result],
			  Type)
	catch
	   {badarg, BadArg} -> throw({error, "Error while matching in dets table, argument: ~p~n", [BadArg]})
       end.


match_check([]) ->
		delete_results(),
		throw({error, "Cannot find the proper row in depenedency table, " ++ 
	 	                          "we have to rebuild the table." ++ 
	 	                          " Please, try to evaluate the query again!~n" ++
						"Rerun required~n", []}); 
match_check([Element | _]) ->
		Element.

%% @spec verify_node(Node, Type) -> [gnode()] | error
%% Node = gnode() | Name
%% Name = string()| atom(), Type = func | mod | fb_rel
%% @doc Checks whether the user-given parameter (given as gnode or with a name)
%% is correct or not. Only for intercalls, not for user use.
verify_node(Node, func) when is_atom(Node)->
	verify_node(atom_to_list(Node), func);
verify_node("Other", fb_rel)->
    "Other";
verify_node({_, _ , _} = Node, _) ->
	try
      	case  ?Graph:data(Node) of
			{func, _Name, _Arity, _, _} ->
					%% {func, Name, Arity,
					%% ?Mod:name(get_func_mod(Node))};
					Node;
			{module, _Name} ->
					%%{module, Name};
					Node;
		_ -> 
	     		throw({error, "The following function block doesn't exist: ~p~n", [Node]})
	     end
	catch
	     _-> throw({error, "The following gnode doesn't exist: ~p. ~n", [Node]})
	end;
verify_node(Node, fb_rel)->
    List = refusr_dir_sort:valid_paths(get_all_modules()),
    case lists:member(Node, 
		      List) of
	true ->
	    Node;
	false->
		
		Res = [Next|| Next <- List, 
			lists:prefix(Node, Next)],
		case Res of
			[] -> 
	   		      throw({error, "The following function block doesn't exist: ~p~n ", [Node]});
			_ ->
		        	Node
		end
    end;
verify_node(Node, mod)->
     case ?Query:exec(?Mod:find(Node)) of
	 [] ->
	     throw({error, "The following module doesn't exist: ~p ~n", [Node]});
	 [Module | _] ->
	     Module
     end;
verify_node(Node, func) ->
    case parse_funId(Node) of
        {Mod, Fun, Arity} ->
		case  ?Query:exec(verify_node(Mod, mod), ?Fun:find(Fun, Arity)) of
			[] -> throw({error, "The following function doesn't exist: ~p~n ", [Node]});
			[Res | _] -> Res
		end;
        false ->
	    throw({error, "The following function doesn't exist: ~p~n ", [Node]})
    end.
%% ----------------------------------------------------------------------

parse_funId(Str) ->
    try
        [Mod, Fun, Arity] = string:tokens(Str, ": /"),
        {list_to_atom(Mod), list_to_atom(Fun), list_to_integer(Arity)}
    catch
        _:_ -> false
    end.

%% Prints out the cycles.
print_names(Nodes, func)->
	[list_to_atom(lists:flatten(?MISC:fun_text(
                             [?Mod:name(?Query:exec1(Fun, ?Fun:module(), no_mod)), 
                              ?Fun:name(Fun), 
                              ?Fun:arity(Fun)]))) || Fun <- Nodes];
print_names(Nodes, mod)->
	[atom_to_list(?Mod:name(Mod)) || Mod <- Nodes].
			
print_cycle(Type, Cycles) when is_list(Cycles) -> 
    ListText = [print_names(Cycle, Type) || Cycle <- Cycles],
    io:format("{\"~p cycle(s)\",~n~p}~n", [length(Cycles), ListText]).
			
print_cycle(Table, _Args, Type, false)->
    print_cycle(Type, 
		check_cycle(Table));
print_cycle(_, _, Type, Res)->
	print_cycle(Type, Res).


%%============================================================================
%% Drawings

-define(Dot_Header, "digraph dependency_representation {\n").
-define(Dot_Footer, "}\n").
-define(Dot_Node, "~p~p [shape=\"~p\", label=~p, fontsize=\"~p\","
        "color=\"~p\"  URL=\"#ok\", tooltip=\"node={'$gn', ~p, ~p}\"]\n").
-define(Dot_FbNode, "~p~p [shape=\"~p\", label=~p, fontsize=\"~p\","
        "color=\"~p\"  URL=\"#ok\", tooltip=~p]\n").
-define(Dot_CycleEdge, "~p~b -> ~s~b [color=\"red\",  style=\"dashed\", arrowhead=\"invdot\", tooltip=\"~w\"]\n").
-define(Dot_Edge, "~p~b -> ~s~b [color=\"~s\",  style=\"~p\", tooltip=\"~w\"]\n").
-define(Dot_RootEdge, "root -> ~s~b [color=\"black\",  style=\"solid\"]\n").
-define(Dot_Root, "root [shape=\"triangle\", label=\"ROOT\", fontsize=\"18\","
        "URL=\"#ok\", tooltip=\"node={'$gn', root, 0}\"]\n").

draw(Table, FromNode, Type, DotName, false)->
    draw(Table, FromNode, Type, DotName,
	 check_cycle(Table));
draw(Table, [], Type, DotName, Cycles)->
    Ser = getElRel(Table),
    write_file({Ser, serialize(Ser, [])}, Cycles, Type,  DotName),
    format_res(Cycles);
draw(Table, FromNode, fb_rel, DotName, Cycles)->
    Ser = getElRel(Table, FromNode, []),
    write_file({Ser,serialize(Ser, [])}, sort_cycle(FromNode, Cycles), fb_rel, DotName),
    format_res(Cycles);
draw(Table, _, Type, DotName, Cycles)->
    Ser = getElRel(Table),
    write_file({Ser,serialize(Ser, [])}, Cycles, Type, DotName),
    format_res(Cycles).

draw_cycles(Table, FromNode, Type, DotName, false)->
    draw_cycles(Table, FromNode, Type, DotName,
	       check_cycle(Table));
draw_cycles(Table, _, Type, DotName, [])->
    io:format("No cycles, drawing the whole dependency graph~n"),
    draw(Table, [], Type, DotName, []);
draw_cycles(_, [], Type, DotName, Cycles)->
    Ser = serializeCycles(Cycles, []),
    write_file({[], Ser}, Cycles, Type, DotName),
    format_res(Cycles);
draw_cycles(_, _, Type, DotName, Cycles)->
    Ser = serializeCycles(Cycles, []),
    write_file({[], Ser}, Cycles, Type, DotName),
    format_res(Cycles).

getElRel(Table)->
    dets:match(Table, #row{ number = '$1',
			     element = '$2',
			     relations = '$3',
			     cycles = '_'}).
getElRel(_, [], Res)->
	Res;
getElRel(Table, [Hd | Tl], Res)->
	[[Num, Rel] | _] = dets:match(Table, #row{ number = '$1',
			     element = Hd,
			     relations = '$2',
			     cycles = '_'}),
	getElRel(Table, Tl, [[Num, Hd, Rel] | Res]).



%%-------------------------------------------------------------
write_file({RowList, SerList}, Cycles, Type, File)->
    io:format("Creating ~p file...~n", [File]),
    Ets = ets:new(added_vertices, [bag]), %% ets table for already added vertices

    case file:open(File, [write]) of
	{ok, Dev}->
	    try
	    	io:put_chars(Dev, ?Dot_Header),

		root(Dev, Type),
		fb_list(SerList, Type),

		NewCycles = cycles(Dev, Cycles, [],
			    SerList, Ets),

		case RowList of
		    [] ->
			ok;
		    _ ->
			process_list(Dev, RowList, NewCycles, SerList, Ets)
		end,

	    	io:put_chars(Dev,?Dot_Footer)
	    catch
		{error, Element} ->  throw({error, "Internal error with drawing. Serialize error with element: ~p~n",
						 [Element]});
		{mod_not_found, Fun} ->
					throw({error, "Module not found for function: ~p~n", [Fun]})
	    after
	    	file:close(Dev), 
		ets:delete(Ets)
	    end;

        {error, Reason}->
	    throw({error, "Error with openning the file: " ++ file:format_error(Reason), []})
    end.


root(Dev, func)->
     io:put_chars(Dev, ?Dot_Root);
root(_, _) ->
    ok.

fb_list(Fbs, fb_rel)->
	io:format("~n----------------------------------------------------------~n"),
	[io:format("Functionblock ~p is ~p~n", [N,E]) ||
		{N, E} <- lists:reverse(Fbs)],
	io:format("------------------------------------------------------------~n");
fb_list(_, _)->
	ok.

serialize([], SerList)->
	SerList;
serialize([[_, E, Rels] | Tail], SerList)->
	NewList = checkSerialized(E, Rels, SerList, fun serializeList/2),
	serialize(Tail, NewList).

serializeList([] , SerList)->
	SerList;
serializeList([Hd | Tail] , SerList)->
	checkSerialized(Hd, Tail, SerList, fun serializeList/2).
		

serializeCycles([], SerList)->
	SerList;
serializeCycles([Hd| Cycles], SerList) ->
	NewList = serializeList(Hd, SerList),
	serializeCycles(Cycles, NewList).

	
checkSerialized(Element, Rest, SerList, Func) ->
	case lists:keymember(Element, 2, SerList) of
			true ->
				Func(Rest, SerList);
			false ->
				Func(Rest, [{length(SerList)+1, Element} | SerList])
	end.
	

sort_cycle(NodeList, Cycles)->
	[  Cycle || [Hd | _] = Cycle <- Cycles, 
		lists:member(Hd, NodeList) ].


process_list(_, [],  _, _, _)->
    ok;
process_list(File, [[0, Element, Rels]  | Rest],
	     EdgeList, S, Ets)->	
    New = check_element(File, Element, EdgeList, Ets),
    New2 = process_edges(File, Element, Rels, New, S, Ets),
    process_list(File, Rest,  New2, S, Ets);
process_list(File, [[_, Element, Rels]  | Rest],
	     EdgeList, SerialList, Ets)->
    New = check_element(File, {Element, 
			SerialNo = get_serial(SerialList,
				   Element)}, EdgeList, Ets),
    New2 = process_edges(File, {Element, SerialNo}, 
			 Rels, New, SerialList, Ets),
    process_list(File, Rest,  New2, SerialList, Ets).

process_edges(_, _, [], EdgeList, _, _)->
    EdgeList;
process_edges(File, {Element, N}, [Hd | Rest], EdgeList, S, Ets)->
    New = 
	check_element(File, {Hd, M = get_serial(S, Hd)}, EdgeList, Ets),
    K = check_edge(New, Element, Hd),
    New2 = draw_edge(File, {Element, N}, {Hd, M}, called, 
	     K),
    process_edges(File, {Element, N}, Rest, New2, S, Ets);
process_edges(File, Element, [Hd | Rest], EdgeList, S, Ets)->
    New = 
	check_element(File, Hd, EdgeList, Ets),
    New2 = draw_edge(File, Element, Hd, called, 
	      check_edge(New, Element, Hd)),
    process_edges(File, Element, Rest, New2, S, Ets).


check_element(File, Element,EdgeList, Ets)-> %% already added
	case ets:member(Ets, Element) of
		true-> EdgeList;
		false-> 
			ets:insert(Ets, {Element}),
			draw_element(File, Element, EdgeList, Ets)
	end.

%%add_to_ets(Ets, Element)->
%%	Id = proplists:get_value(size, ets:info(Ets)),
%%	ets:insert(Ets, {Id, Element}).

draw_element(File, Mod = {_, module, Number}, EdgeList, _Ets)->
    io:format(File, ?Dot_Node,  [m, Number, box, label(Mod),
				 18, purple, mod, Number]),
    EdgeList;
draw_element(File, Fun = {_, func, Number}, EdgeList, Ets) ->
    New = check_element(File, Mod = get_func_mod(Fun), EdgeList, Ets),
    New2 = 
	draw_edge(File, root, Mod, parent,
		  check_edge(New, root, Mod)),
	{Label, Shape, Colour} = opaque_label(label(Fun)),
    io:format(File, ?Dot_Node,  [f, Number, Shape, Label,
				 14, Colour, func, Number]),
    draw_edge(File, Mod, Fun, parent,
	      check_edge(New2, Mod, Fun));
draw_element(File,  {Element, Number}, EdgeList, _Ets)->
    io:format(File, ?Dot_FbNode, [fb, Number, hexagon,
				Number,
			       18, black, Element]),
     EdgeList;
draw_element(File, [Hd | []], EdgeList, Ets)->
	draw_element(File, Hd, EdgeList, Ets).

opaque_label(Label)->
	case (lists:prefix("opaque", Label) or lists:suffix("/-1", Label)) of
		true ->
			{"¦ " ++ Label, doubleoctagon, gray53};
		false ->
			{Label, hexagon, black}
	end.

draw_edge(_, _, _, _,  {true, EdgeList})->
    EdgeList;
draw_edge(File,  root, {_, Type, N}, _,  {false, EdgeList})->
    io:format(File, ?Dot_RootEdge,
	      [element_type(Type), N]),
    EdgeList;
draw_edge(File, {_, Type1, N1} = Node1, {_, Type2, N2} = Node2,
	  EdgeType, {false, EdgeList})->
    Vertex1 = element_type(Type1),

    Vertex2 = element_type(Type2),
    Tooltip = tooltip(Node1, Node2),
    io:format(File, ?Dot_Edge, [Vertex1, N1, Vertex2, N2, colour_edge(Tooltip),
				edge_type(EdgeType, Tooltip), Tooltip]),
    EdgeList;
draw_edge(File,  {_, N1}, {_, N2}, _,  {false, EdgeList})->
    io:format(File, ?Dot_Edge, [fb, N1, fb, N2, black, dashed, ""]),
    EdgeList.


colour_edge(funcall)->
	black;
colour_edge(may_be)->
	gray;
colour_edge(ambcall)->
	gray;
colour_edge(dyncall)->
	gray;
colour_edge(_) ->
	black.

check_edge(EdgeList, Node1, Node2)->
    case lists:member({Node1, Node2}, EdgeList) of
	true ->
	    {true, EdgeList};
	false ->
	    {false, [{Node1, Node2}| EdgeList]}
    end.

cycles(_, [], EdgeList, _, _)->
    EdgeList;
cycles(File, [Hd | Rest], EdgeList, S, Ets)->
  cycles(File, Rest,
	 process_cycle(File, Hd,
			      EdgeList, S, Ets), S, Ets).

process_cycle(_, [_ | []], EdgeList, _, _)->
    EdgeList;
process_cycle(File,
	      [E1 = {_, Type1, N1} | 
	       Rest = [ E2 = {_, Type2, N2} | _]],
	      EdgeList, S, Ets)->
	case lists:member({E1, E2}, EdgeList) of 
	    false -> 
    			io:format(File, ?Dot_CycleEdge,
	 			     [element_type(Type1), N1,
	    			      element_type(Type2), N2,
				      tooltip(E1, E2)]),
    			New = check_element(File, E1, 
		                    	   check_element(File, E2, EdgeList, Ets), 
					   Ets),
   			process_cycle(File, Rest, 
				[{E1, E2} | New], S, Ets);
	    true -> process_cycle(File, Rest, EdgeList, S, Ets)
	end;
process_cycle(File, [Fb1 |Rest= [Fb2 | _]], EdgeList, S, Ets) ->
	case lists:member({Fb1, Fb2}, EdgeList) of
	    false ->
    		io:format(File, ?Dot_CycleEdge,
	      		[fb, N1 = get_serial(S, Fb1),
	       		 fb, N2= get_serial(S, Fb2), ""]),
    			New = check_element(File, {Fb1, N1}, 
		             		     check_element(File, {Fb2, N2}, [{Fb1, Fb2} | EdgeList], Ets),
					    Ets),
                 process_cycle(File, Rest, [{Fb1, Fb2} | New], S, Ets);
	    true ->
		process_cycle(File, Rest, EdgeList, S, Ets)
	end.

label(Vertex = {_, module, _})->
    atom_to_list(?Mod:name(Vertex));
label(Vertex = {_, func, _})->
    atom_to_list(?Fun:name(Vertex)) ++ "/" ++
                      integer_to_list(?Fun:arity(Vertex)).

element_type(module)->
    m;
element_type(func) ->
    f.

edge_type(called, funcall)->
    dashed;
edge_type(_, Opaque) ->
 case lists:member(Opaque, [ambcall, dyncall, may_be]) of
   true -> dotted;
   false -> solid
 end.

get_serial([], Element)->
    throw({error, Element});
get_serial([{N, Element} | _], Element)->
    N;
get_serial([_ | Rest], Element) ->
    get_serial(Rest, Element).

tooltip({_, module, _} = Node1, {_, module, _} = Node2)->
	connecting_functions(Node1, Node2);
tooltip({_, func, _} = Node1, {_, func, _} = Node2)->
	{value, {Tooltip, _}} = lists:keysearch(Node2, 2, ?Graph:links(Node1)),
	Tooltip;
tooltip({_, _, _}, {_, _, _})->
	"".

connecting_functions(Mod1, Mod2)->
	[{?Fun:name(Fun), ?Fun:name(Calls)}|| Fun<-?Query:exec(Mod1, ?Mod:locals()),
		 Funcalls <- refusr_cyclic_fun:funcalls(Fun),
		 Funcalls /= [],
		 Calls<-Funcalls,
		 get_func_mod(Calls) == Mod2].

%%connecting_modules(Fb1, Fb2)->
%%	[{_, Mods1}]=refusr_fb_regexp:mods_from_fbs([Fb1], []),
%%	[{_, Mods2}]=refusr_fb_regexp:mods_from_fbs([Fb2], []),
%%	[{Mod, CalledMod} || Mod <- Mods1,
%%	     CalledMod<-refusr_cyclic_mod:get_mod_modcall(Mod),
%%	     lists:member(CalledMod, Mods2) == true].



%% Formats the result of the get_relations/1 function for module/function levels.
print_rels(Result, Type)->
	io:format("~nThe meaning of the following result: {Node, Paths, Cycles}~n", []),
	[ 
	 {
	   hd(print_names([Element], Type)), %% Function/Module
	   print_names(Rels, Type), 	%%Relations
	   [print_names(Cycle, Type) || Cycle <- Cycles] %%Cycles
	 } 
	 || #row{number = _, element = Element, relations = Rels, cycles= Cycles} <- Result].
