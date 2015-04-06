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
%%% Portions created  by Eötvös Loránd University are  Copyright 2007-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Graph storage server. This is a NIF based implementation of
%%% the semantical graph server interface.
%%%
%%% @author Peter Felker <felker.peter88@gmail.com>

-module(refdb_nif).
-vsn("$Rev: 8243 $ ").
-behaviour(gen_refdb).


%%% ============================================================================
%%% Compiler options

-define(use_binary_wrapper, true).

%%% ============================================================================
%%% Exports

%% Client exports
-export([%start_link/1,
         init/1,
         handle_operation/3,
         code_change/2,
         terminate/2]).

%%% ============================================================================
%%% Debug

% the following are temporarily moved here from core_export.hrl

%% Prettyprints the value of a single expression with module/line information.
% -define(u2(Name, X), begin io:format("~4w ~s~n ~12s: ~p~n", [?LINE, atom_to_list(?MODULE), Name, X]), ?Graph:sleep(), X end).
% -define(u(X), ?u2(??X, X)).

%%% ============================================================================
%%% Structures

%% @type node().
%%  Represents a node in the graph.

%% @type data() = tuple().
%%  Represents the class and attributes of a node. This is essentially a
%%  record, the name of the record (or the first element of the tuple) is
%%  the class name, and the fields are the attributes.

%% @type path() = [PathElem]
%%       PathElem = Tag | {Tag, Index} | {Tag, Filter} | {intersect, Node, Tag}
%%                | Union | {PathElem | path(), unique}
%%       Union = [PathElem | path()]
%%       Tag = atom() | {atom(), back}
%%       Index = integer() | last | {integer(), last} | {integer(), integer()}
%%       Filter = {Filter, and, Filter} | {Filter, or, Filter}
%%              | {not, Filter} | {Attrib, Op, term()}
%%       Attrib = atom()
%%       Op = '==' | '/=' | '=<' | '>=' | '<' | '>'.
%% Indexes start at 1. `{Start, End}' interval means indexes `Ind' that satisfy
%% `Start =< Ind < End'.

%% @type schema() = [ClassDef]
%%       ClassDef = {ClassName::atom(), [Attrib::atom()], [Link]}
%%       Link = {Tag::atom(), ClassName::atom()}.
%%  Describes the schema of the graph.

-record(dbState, {has_schema  :: boolean() | schema_error,
                  schema      :: tuple(),
                  schema_list :: list()}).
-include("core.hrl").

%% @private
init(_InitArgs) ->
    Rel = erlang:system_info(otp_release),
    case re:run(Rel, "R(1[4-9]|[2-9][0-9])B.*") of
        nomatch ->
            io:format("(probably) incompatible release ~p~n", [Rel]),
            exit(-1);
        _ ->
            ok
    end,
    filelib:ensure_dir(filename:join(mnesia:system_info(directory), "dummy")),
    load_nif(),
    restore_envs(),
    {ok, #dbState{has_schema = false}}.

load_nif() ->
    %process_flag(trap_exit, true),
    Nif = filename:join(code:lib_dir(referl_core, priv), "nif_graph"),
    load_nif(Nif, get_datastore() ++ [mnesia:system_info(directory)]).

%% @doc Tries to get back the graph and persistence object pointers in a list.
%% If something is wrong (connection with NIF has not been established, etc),
%% then returns [0, 0]. In the list, the first element is the graph pointer,
%% while the second one is the persistence object pointer. If all of these are 0,
%% then a new persistence and graph object will be created at C++ level.
get_datastore() ->
    try
        nif_get_datastore()
    catch
        _ -> [0, 0]
    end.

load_nif(Path, LoadInfo) ->
    try
        ok = erlang:load_nif(Path, LoadInfo)
    catch
        _:_ ->
            try
                ok = erlang:load_nif(Path, LoadInfo)
            catch
                _:_ ->
                    io:format("~n" ++
                              "Error: RefactorErl cannot find the nif_graph shared library!~n"
                              "Solutions:~n" ++
                              "-Try to rebuild the tool.~n"
                              "-Install a g++ compiler (at least g++ 4.3.6) and then rebuild the tool.~n" ++
                              " (It is important that your PATH environment has to contain g++)~n" ++
                              "-Add write permission to <path-to-RefactorErl>/lib/referl_core/priv/ folder~n" ++
                              " and then rebuild the tool.~n"),
                    halt()
            end,
            io:format("The last used graph has been loaded, " ++
                      "but with an empty database, because it may be crashed!\n")
    end.

%% @private

-define(HandleNifError(Instruction, St),
            try
                Instruction
            catch
                Error -> {result, Error, St}
            end).

handle_operation({get_schema}, _Extra,
               #dbState{schema_list = SchemaList} = State) ->
    {result, SchemaList, State};
handle_operation({schema, Schema}, _Extra, State) ->
    ?HandleNifError(handle_schema(Schema, State), State);
handle_operation({reset_schema}, _Extra, #dbState{schema = Schema} = State) ->
    ?HandleNifError(handle_reset_schema(Schema), State);
handle_operation(Request, _Extra, #dbState{has_schema = true, schema = Schema} = State) ->
    Result = handle_req(Request, Schema),
    {result, Result, State};
handle_operation(_Request, _Extra, no_schema) ->
    {stop, no_schema, no_schema, no_schema};
handle_operation(_Request, _Extra, #dbState{has_schema = schema_error} = State) ->
    {stop, schema_error, schema_error, State}.

code_change(_OldVsn, _State) ->
    no_code_change.

terminate(_Reason, _State) ->
    ok.

handle_req({root}, _Schema) ->
    handle_root();

handle_req({create, Data, Prot}, Schema) ->
    handle_create(Data, Prot, Schema);

handle_req({update, Node, Data}, _Schema) ->
    handle_update(Data, Node);

handle_req({delete, Node}, _Schema) ->
    handle_delete(Node);

handle_req({data,   Node}, _Schema) ->
    handle_data(Node);

handle_req({mklink, From, Tag, To, Prot}, Schema) ->
    handle_mklink(From, Tag, To, Prot, Schema);

handle_req({rmlink, From, Tag, To}, Schema) ->
    handle_rmlink(From, Tag, To, Schema);

handle_req({links,  Node}, Schema) ->
    handle_links(Node, Schema);

handle_req({back_links,  Node}, Schema) ->
    handle_back_links(Node, Schema);

handle_req({path,   Node, Path},  Schema) ->
    handle_path(Node, Path, Schema);

handle_req({index,  From, Tag, To},  Schema) ->
    handle_index(From, Tag, To, Schema);

handle_req({setp,   Node,Key,Val}, _Schema) ->
    handle_setp(Node, Key, Val);

handle_req({getp,   Node, Key}, _Schema) ->
    handle_getp(Node, Key);

handle_req({getp,   Node}, _Schema) ->
    handle_getp(Node);

handle_req({delp,   Node, Key}, _Schema) ->
    handle_delp(Node, Key);

handle_req({erase_nodes}, _Schema) ->
    handle_erase();

%handle_req({reset_schema},  Schema) ->
%    handle_reset_schema(Schema);

handle_req({is_protected_node, Node}, _Schema) ->
    handle_is_protected_node(Node);

handle_req({backup, CommitLog},  _Schema) ->
    handle_backup(CommitLog);

handle_req({ls_backups}, _Schema) ->
    handle_ls_backups();

handle_req({backup_info, Backup}, _Schema) ->
    handle_backup_info(Backup);

handle_req({undo}, _Schema) ->
    handle_undo();

handle_req({remove_garbage}, _Schema) ->
    handle_remove_garbage();

handle_req({clean}, _Schema) ->
    handle_clean();

handle_req({create_graph, Name}, _Schema) ->
    handle_create_graph(Name);

handle_req({rename_graph, OldName, NewName}, _Schema) ->
    handle_rename_graph(OldName, NewName);

handle_req({ls_graphs}, _Schema) ->
    handle_ls_graphs();

handle_req({actual_graph}, _Schema) ->
    handle_actual_graph();

handle_req({load_graph, Name}, _Schema) ->
    handle_load_graph(Name);

handle_req({delete_graph, Name}, _Schema) ->
    handle_delete_graph(Name);

handle_req({delete_all_graphs}, _Schema) ->
    handle_delete_all_graphs();

handle_req({save, FileName}, _Schema) ->
    handle_save(FileName);

handle_req({restore, Backup}, _Schema) ->
    handle_restore(Backup);

handle_req({save_envs}, _Schema) ->
    save_envs();

handle_req(_, _Schema) ->
    throw(request_not_implemented).


%%% ----------------------------------------------------------------------------
%%% Schema operations

handle_erase() ->
    ok.

handle_reset_schema(Schema) ->
    propagate_reset(fun reset_schema/1, [Schema]).


%%% ----------------------------------------------------------------------------
%%% Node operations

handle_root() ->
    {ok, {?NODETAG, root, int_root()}}.

handle_data(Node = {?NODETAG, _Class, Id}) ->
    case int_data(Id) of
        {bad_node, _} ->
            {error, {bad_node, Node}};
        Result ->
            {ok, Result}
    end.

handle_is_protected_node({?NODETAG, _Class, Id}) ->
    {ok, int_is_protected_node(Id)}.

handle_create(Data, IsProtected, Schema) ->
    Class = get_class(Data),
    is_valid_class(Class, Schema),
    {ok, {?NODETAG, Class, int_create(Data, IsProtected)}}.

handle_update(Data, Node = {?NODETAG, NodeClass, NodeId}) ->
    case get_class(Data) =:= NodeClass of
        true ->
            case int_update(NodeId, Data) of
                ok ->
                    ok;
                {protected_node, _Id} ->
                    {ok, protected_node};
                {bad_node, _Id} ->
                    {error, {bad_node, Node}}
            end;
        false ->
            {error, {bad_class, NodeClass}}
    end.

handle_delete({?NODETAG, _Class, Id}) ->
    case int_delete(Id) of
        ok ->
            ok;
        {bad_node, Id} ->
            ok;
        {protected_node, _Id} ->
            {ok, protected_node}
    end.

%%% ----------------------------------------------------------------------------
%%% Link modifications

handle_mklink(FNode = {?NODETAG, FClass, FId}, TagInfo,
              TNode = {?NODETAG, _TClass, TId}, IsProtected, Schema) ->
    case TagInfo of
        {Tag, Idx} when is_atom(Tag) -> ok;
        Tag when is_atom(Tag)        -> Idx = last
    end,

    try
        get_next_class(fwd, FClass, Tag, Schema),
        case int_mklink(FId, Tag, Idx, TId, IsProtected) of
            ok ->
                ok;
            {bad_node, FId} ->
                {error, {bad_node, FNode}};
            {bad_node, TId} ->
                {error, {bad_node, TNode}}
        end
    catch
       throw:_ -> {error, {bad_link, FNode, TagInfo, TNode}}
    end.

handle_rmlink(FNode = {?NODETAG, FClass, FId}, Tag,
              TNode = {?NODETAG, _TClass, TId}, Schema) ->
    try
        get_next_class(fwd, FClass, Tag, Schema),

        case int_rmlink(FId, Tag, TId) of
            ok ->
                ok;
            {protected_link, _, _, _} ->
                {ok, protected_link};
            _ ->
                {error, {not_exists, FNode, Tag, TNode}}
        end
    catch
        throw:_ -> {error, {bad_link, FNode, Tag, TNode}}
    end.

%%% ----------------------------------------------------------------------------
%%% Link queries

handle_links({?NODETAG, _NodeClass, NodeId}, _Schema) ->
    check_links(int_links(NodeId)).

handle_back_links({?NODETAG, _NodeClass, NodeId}, _Schema) ->
    check_links(int_back_links(NodeId)).

check_links(Links) ->
    case is_list(Links) of
        true ->
            NodeList = [{Tag, {?NODETAG, int_class(Id), Id}}
                        || {Tag, Id} <- lists:usort(Links)],
            {ok, NodeList};
        false ->
            {ok, []}
    end.

handle_path(Node = {?NODETAG, Class, Id}, Path, Schema) ->
    try
        case do_path(Path, [Id], Class, Schema) of
            {error, bad_node, _} ->
                {ok, []};
            Result ->
                {ok, [{?NODETAG, int_class(ResultId), ResultId}
                        || ResultId <- Result]}
        end
    catch
        error:badarg -> {ok, []};
        throw:{overreaching_path_idx,_,_} -> {ok, []};
        throw:Msg -> {error, {Msg, Node, Path}}
    end.

handle_index(FNode = {?NODETAG, FClass, FId}, Tag,
             TNode = {?NODETAG, _TClass, TId}, Schema) ->
    try
        get_next_class(fwd, FClass, Tag, Schema),
        {ok, int_index(FId, Tag, TId)}
    catch
        throw:_ -> {error, {bad_link, FNode, Tag, TNode}}
    end.

%%% ----------------------------------------------------------------------------
%%% Persistence operations

handle_backup(CommitLog) ->
    {ok, {ok, int_backup(CommitLog)}}.

handle_ls_backups() ->
    {ok, int_ls_backups()}.

handle_backup_info(Backup) ->
    {ok, int_backup_info(Backup)}.

handle_undo() ->
    {ok, int_undo()}.

handle_remove_garbage() ->
    {ok, int_remove_garbage()}.

handle_clean() ->
    int_delete_all_backups().

handle_create_graph(Name) ->
    {ok, int_create_graph(Name)}.

handle_rename_graph(OldName, NewName) ->
    graph_op(fun int_rename_graph/2, [OldName, NewName]).

handle_ls_graphs() ->
    graph_op(fun int_ls_graphs/0, []).

handle_actual_graph() ->
    graph_op(fun int_actual_graph/0, []).

handle_load_graph(Name) ->
    graph_op(fun int_load_graph/1, [Name]).

handle_delete_graph(Name) ->
    graph_op(fun int_delete_graph/1, [Name]).

handle_delete_all_graphs() ->
    graph_op(fun int_delete_all_graphs/0, []).

handle_save(FileName) ->
    {ok, int_save(FileName)}.

handle_restore(Backup) ->
    {ok, int_restore(Backup)}.


graph_op(Fun, Args) ->
    case apply(Fun, Args) of
        ok -> ok;
        Error -> {ok, Error}
    end.

%%% ----------------------------------------------------------------------------
%%% Property functions

handle_setp({?NODETAG, _Class, _Id}, _Key, _Value) ->
    ok.

handle_getp({?NODETAG, _Class, _Id}, _Key) ->
    {ok, []}.

handle_getp({?NODETAG, _Class, _Id}) ->
    {ok, []}.

handle_delp({?NODETAG, _Class, _Id}, _Key) ->
    ok.

%%% ----------------------------------------------------------------------------
%%% Implementation details

%% Notify all servers about the pending reset that require it.
propagate_reset_to_servers() ->
    refcore_funprop:reset().

%% Resets the schema to the one given in the parameter.
reset_schema(Schema) ->
    int_reset_schema(),
    {ok, InitSt} = init(dummy_param),
    handle_schema([], InitSt#dbState{has_schema=true, schema=Schema}).

handle_schema(SchemaList, St = #dbState{has_schema=false}) ->
    SchemaETS = ets:new(referl_schema, [bag]),
    SchemaAttrETS = ets:new(referl_schema_attrs, [bag]),
    [ets:insert(SchemaETS, {FromClass, Tag, ToClass}) ||
        {FromClass, _Attribs, SLinks} <- SchemaList,
        {Tag, ToClass} <- SLinks],
    [ets:insert(SchemaAttrETS, {FromClass, Attribs}) ||
        {FromClass, Attribs, _} <- SchemaList],
    Schema = {SchemaETS, SchemaAttrETS},
    {result, {ok, init}, St#dbState{has_schema=true,
                                    schema=Schema,
                                    schema_list=SchemaList}};
handle_schema(_SchemaList, St = #dbState{has_schema=true,
                                         schema=_Schema}) ->
    restore_envs(),
    {result, {ok, match}, St}.

%% @doc Saves the environment configuration to `EnvConfFile'.
save_envs() ->
    EnvIds      = get_envs(),
    EnvDatas    = [Data || EnvId <- EnvIds, Data <- [int_data(EnvId)]],
    case EnvDatas of
        [] ->
            no_envs_saved;
        _ ->
            EnvConfFile = conf_file(),
            {ok, Dev} = file:open(EnvConfFile, [write]),
            io:format(Dev, "~p.~n", [EnvDatas]),
            file:close(Dev)
    end.
    
%% @doc Restores the environment nodes saved by `save_envs/0'.
restore_envs() ->
    del_envs(),
    EnvConfFile = conf_file(),
    Envs =
        case filelib:is_file(EnvConfFile) of
            true ->
                {ok, [Envs2]} = file:consult(EnvConfFile),
                Envs2;
            false ->
                [#env{name=appbase, value=code:lib_dir()},
                 #env{name=output, value=original}]
        end,
    {ok, {?NODETAG, _, RootId}} = handle_root(),
    EnvIds = [int_create(Env, false) || Env <- Envs],
    [int_mklink(RootId, env, last, EnvId, false) || EnvId <- EnvIds].
    
%% @doc Deletes all envs from the graph.
del_envs() ->
    EnvIds = get_envs(),
    [int_delete(EnvId) || EnvId <- EnvIds].
    
%% @doc Returns all envs' id in a list. 
get_envs() ->
    {ok, {?NODETAG, _, RootId}} = handle_root(),
    EnvIds = int_path([RootId], fwd, env),
    try (is_list(EnvIds) andalso (length(EnvIds) > 0)) of
        true  -> EnvIds;
        false -> []
    catch
        _:_   -> []
    end.

%% Returns the name of the configuration file.
conf_file() ->
    DataDir = mnesia:system_info(directory),
    EnvFileName = "refactorerl.configuration",
    filename:join(DataDir, EnvFileName).

do_path([], Nodes, _, _Schema) ->
    Nodes;
do_path([Elems|Rest], Nodes, Class, Schema) when is_list(Elems) ->
    % Elems is an union of tags, we walk them all
    NewNodes = lists:flatten([int_path(Nodes, fwd, Elem) || Elem <- Elems]),
    do_path(Rest, NewNodes, Class, Schema);
do_path([{Tag, unique}|Rest], Nodes, Class, Schema) ->
    NewNodes = lists:usort(int_path(Nodes, fwd, Tag)),
    do_path(Rest, NewNodes, Class, Schema);
do_path([{intersect, {?NODETAG, _, TestNode}, TestPathElem}|Rest], Nodes, Class, Schema) ->
    Dir =
        case TestPathElem of
            {Tag, back} -> fwd;
            Tag         -> back
        end,
    TestClass = get_next_class(Dir, Class, Tag, Schema),
    Tests = do_path([TestPathElem], [TestNode], TestClass, Schema),
    do_path(Rest, ?MISC:intersect(Tests, Nodes), Class, Schema);
do_path([{Tag, {FromIdx, ToIdx}} | Rest], Nodes, Class, Schema) when is_integer(FromIdx), is_integer(ToIdx) ->
    Nodes2 = do_path([Tag], Nodes, Class, Schema),
    NewNodes = lists:sublist(Nodes2, FromIdx, ToIdx - FromIdx),
    NextClass = get_next_class(fwd, Class, Tag, Schema),
    do_path(Rest, NewNodes, NextClass, Schema);
do_path([Elem | Rest], Nodes, Class, Schema) ->
    {Dir, Filter} =
        case Elem of
            {{Tag, back}, Filt} when is_atom(Tag) -> {back, Filt};
            {Tag,  back}        when is_atom(Tag) -> {back, {}};
            {Tag,         Filt} when is_atom(Tag) -> {fwd,  Filt};
            Tag                 when is_atom(Tag) -> {fwd,  {}};
            Tag                                   -> throw({bad_path, Elem})
        end,

    NextClass = get_next_class(Dir, Class, Tag, Schema),

    NewNodes =
        case Filter of
            {} ->
                int_path(Nodes, Dir, Tag);
            _ ->
                NewNodess  = [int_path([Node], Dir, Tag) || Node <- Nodes],
                lists:flatten([filter_ptrs(Filter, NNodes, NextClass, Schema)
                                    || NNodes <- NewNodess])
        end,
    do_path(Rest, NewNodes, NextClass, Schema).


%% Returns the name of the class reached from `Class' along `Tag'
%% in the direction `Dir'.
get_next_class(fwd, FromClass, Tag, {SchemaETS, _}) ->
    case ets:match_object(SchemaETS, {FromClass, Tag, '_'}) of
        [{_, _, To}] -> To;
        _            -> throw({bad_path, FromClass, Tag})
    end;
get_next_class(back, ToClass, Tag, {SchemaETS, _}) ->
    case ets:match_object(SchemaETS, {'_', Tag, ToClass}) of
        [{From, _, _}] -> From;
        _              -> throw({bad_path, ToClass, Tag})
    end;
    
get_next_class(_, Class, Tag, _) ->
    throw({bad_path, Class, Tag}).

is_valid_class(Class, {SchemaETS, _}) ->
    MatchedObjects = ets:match_object(SchemaETS, {Class, '_', '_'}) ++
      ets:match_object(SchemaETS, {'_', Class, '_'}),
    case length(MatchedObjects) of
        0 -> exit({aborted, {not_exists, Class}});
        _ -> ok
    end.

get_class(Data) ->
    element(1, Data).

nth(Idx, Nodes) ->
    try
        lists:nth(Idx, Nodes)
    catch
        error:function_clause ->
            throw({overreaching_path_idx, Idx, Nodes})
    end.

%% Filters a set of pointers.
filter_ptrs({}, _Nodes, _, _Schema) ->
    throw(is_handled_outside);
filter_ptrs(_, [], _, _Schema) ->
    [];
filter_ptrs(Idx, Nodes, _, _Schema) when is_integer(Idx) ->
    nth(Idx, Nodes);
filter_ptrs(last, Nodes, _, _Schema) ->
    lists:last(Nodes);
filter_ptrs({Idx, last}, Nodes, _, _Schema) when is_integer(Idx) ->
    {_, Part2} = lists:split(Idx, Nodes),
    Part2;
filter_ptrs({Idx1, Idx2}, Nodes, _, _Schema) when is_integer(Idx1), is_integer(Idx2) ->
    lists:sublist(Nodes, Idx1, Idx2 - Idx1);
    %{_, Part2}  = lists:split(Idx1 - 1, Nodes),
    %{Part21, _} = lists:split(Idx2 - Idx1 - 1, Part2),
    %Part21;
filter_ptrs(Filter, Nodes, NextClass, {_SchemaETS, SchemaAttrETS}) ->
    [{_, Attrs}] = ets:match_object(SchemaAttrETS, {NextClass, '_'}),
    [Node || Node <- Nodes, data_filter(Filter, int_data(Node), Attrs)].


data_filter({'not', Filter}, Data, Attrs) ->
    not data_filter(Filter, Data, Attrs);
data_filter({Filt1, 'and', Filt2}, Data, Attrs) ->
    data_filter(Filt1, Data, Attrs) andalso data_filter(Filt2, Data, Attrs);
data_filter({Filt1, 'or', Filt2}, Data, Attrs) ->
    data_filter(Filt1, Data, Attrs) orelse data_filter(Filt2, Data, Attrs);
data_filter({Attr, Op, Value}, Data, Attrs) when is_atom(Attr) ->
    OpF =
        case Op of
            '==' -> fun(A,B) -> A =:= B end;
            '/=' -> fun(A,B) -> A =/= B end;
            '<'  -> fun(A,B) -> A <   B end;
            '=<' -> fun(A,B) -> A =<  B end;
            '>'  -> fun(A,B) -> A >   B end;
            '>=' -> fun(A,B) -> A >=  B end
        end,
    Ind = indexof(Attr, Attrs) + 1,
    if
        Ind =:= 0 -> throw({bad_attribute, Attr});
        true      ->
            OpF(element(Ind, Data), Value)
    end;
data_filter(Cond, _, _) ->
    throw({bad_condition, Cond}).


%% todo Move to ?List
indexof(El, Lst) -> indexof(El, Lst, 1).

indexof(_,  [],        _)   -> -1;
indexof(El, [El | _],  Ind) -> Ind;
indexof(El, [_  | Tl], Ind) -> indexof(El, Tl, Ind+1).


%%% ============================================================================
%%% NIF calls

%% These macros are required in older versions of Erlang/OTP
%% that had no interface for transferring atoms and terms.
-ifdef(use_binary_wrapper).
    -define(term2gterm(X), term_to_binary(X)).
    -define(gterm2term(X), binary_to_term(X)).
    -define(atom2gatom(X), atom_to_binary(X, latin1)).
    -define(gatom2atom(X), binary_to_atom(X)).
-else.
    -define(term2gterm(X),      X).
    -define(gterm2term(X),      X).
    -define(atom2gatom(X),      X).
    -define(gatom2atom(X),      X).
-endif.

%int_is_protected_link(NodeId1, Tag, NodeId2) ->
%    nif_is_protected_link(NodeId1, ?atom2gatom(Tag), NodeId2).

int_is_protected_node(NodeId) ->
    nif_is_protected_node(NodeId).

int_root() ->
    nif_root().

int_create(Data, IsProtected) ->
    nif_create(?term2gterm(Data), ?atom2gatom(IsProtected)).

int_update(Node, Data) ->
    nif_update(Node, ?term2gterm(Data)).

int_data(Node) ->
    Result = nif_data(Node),
    case is_binary(Result) of
        true ->
            ?gterm2term(Result);
        false ->
            Result
    end.

int_delete(Node) ->
    nif_delete(Node).

int_mklink(FNode, Tag, last, TNode, IsProtected) ->
    nif_mklink(FNode, ?atom2gatom(Tag), -1, TNode, ?atom2gatom(IsProtected));

int_mklink(FNode, Tag, Idx, TNode, IsProtected) ->
    nif_mklink(FNode, ?atom2gatom(Tag), Idx, TNode, ?atom2gatom(IsProtected)).

int_rmlink(FNode, Tag, TNode) ->
    nif_rmlink(FNode, ?atom2gatom(Tag), TNode).

int_remove_garbage() ->
    nif_remove_garbage().

int_path(Nodes, Dir, Tag) ->
    PathFun =
        case Dir of
            fwd  -> fun nif_fwd_path/2;
            back -> fun nif_back_path/2
        end,
    NIFAtom = ?atom2gatom(Tag),
    lists:concat([PathFun(Node, NIFAtom) || Node <- Nodes]).

int_links(Node) ->
    nif_links(Node).

int_back_links(Node) ->
    nif_back_links(Node).

int_class(Node) ->
    get_class(int_data(Node)).

int_index(FNode, Tag, TNode) ->
    nif_index(FNode, ?atom2gatom(Tag), TNode).

int_reset_schema() ->
    nif_reset_schema().

int_ls_backups() ->
    lists:reverse(nif_ls_backups()).

int_backup_info(Backup) ->
    nif_backup_info(Backup).

int_delete_all_backups() ->
    nif_delete_all_backups().

int_backup(CommitLog) ->
    save(fun nif_backup/1, CommitLog).

int_save(FileName) ->
    save(fun nif_save/1, FileName).
    
int_restore(BackupName) ->
    propagate_reset(fun nif_restore/1, [BackupName]).

int_undo() ->
    propagate_reset(fun nif_undo/0, []).

int_create_graph(Name) ->
    nif_create_graph(Name).

int_rename_graph(OldName, NewName) ->
    nif_rename_graph(OldName, NewName).

int_ls_graphs() ->
    nif_ls_graphs().

int_actual_graph() ->
    nif_actual_graph().

int_load_graph(Name) ->
    propagate_reset(fun nif_load_graph/1, [Name]).

int_delete_graph(Name) ->
    nif_delete_graph(Name).

int_delete_all_graphs() ->
    propagate_reset(fun nif_delete_all_graphs/0, []).
    
save(Fun, Arg) ->
    save_envs(),
    % Save functions only have 1 parameter.
    Ret = Fun(Arg),
    restore_envs(),
    Ret.
    
propagate_reset(Fun, Args) ->
    save_envs(),
    propagate_reset_to_servers(),
    Ret = apply(Fun, Args),
    restore_envs(),
    Ret.
    
    
%%% ----------------------------------------------------------------------------
%%% NIF stubs

-define(NifLoadError, throw(nif_is_not_loaded)).

nif_is_protected_node(_NodeId)          -> ?NifLoadError.
%nif_is_protected_link(_NodeId1, _Tag,
%                      _NodeId2)         -> ?NifLoadError.
nif_root()                              -> ?NifLoadError.
nif_create(_Data, _IsProtected)         -> ?NifLoadError.
nif_update(_Node, _Data)                -> ?NifLoadError.
nif_delete(_Node)                       -> ?NifLoadError.
nif_data(_Node)                         -> ?NifLoadError.
nif_mklink(_FNode, _Tag, _Idx, _TNode,
           _IsProtected)                -> ?NifLoadError.
nif_rmlink(_FNode, _Tag, _TNode)        -> ?NifLoadError.
nif_remove_garbage()                    -> ?NifLoadError.
nif_fwd_path(_Nodes, _Tag)              -> ?NifLoadError.
nif_back_path(_Nodes, _Tag)             -> ?NifLoadError.
nif_links(_Node)                        -> ?NifLoadError.
nif_back_links(_Node)                   -> ?NifLoadError.
nif_index(_FNode, _Tag, _TNode)         -> ?NifLoadError.
nif_get_datastore()                     -> ?NifLoadError.
nif_reset_schema()                      -> ?NifLoadError.
nif_backup(_CommitLog)                  -> ?NifLoadError.
nif_backup_info(_Backup)                -> ?NifLoadError.
nif_ls_backups()                        -> ?NifLoadError.
nif_delete_all_backups()                -> ?NifLoadError.
nif_undo()                              -> ?NifLoadError.
nif_save(_FileName)                     -> ?NifLoadError.
nif_restore(_Backup)                    -> ?NifLoadError.
nif_create_graph(_Name)                 -> ?NifLoadError.
nif_rename_graph(_OldName, _NewName)    -> ?NifLoadError.
nif_ls_graphs()                         -> ?NifLoadError.
nif_actual_graph()                      -> ?NifLoadError.
nif_load_graph(_Name)                   -> ?NifLoadError.
nif_delete_graph(_Name)                 -> ?NifLoadError.
nif_delete_all_graphs()                 -> ?NifLoadError.
