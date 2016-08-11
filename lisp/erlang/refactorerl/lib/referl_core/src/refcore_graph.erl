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

%%% @doc Graph storage server. This is a mnesia based implementation of
%%% the semantical graph server interface.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refcore_graph).
-vsn("$Rev: 8204 $"). %% "
-behaviour(gen_server).

%%% ============================================================================
%%% Exports

%% Client exports
-export([start_link/0,
         schema/1, get_schema/0, reset_schema/0,
         erase_nodes/0,
         root/0, create/1, create_prot/1, update/2, delete/1, data/1, class/1,
         mklink/3, mklink_prot/3, rmlink/3, links/1, path/2, index/3,
         set_prop/3, get_prop/2, get_props/1, del_prop/2,
         remove_garbage/0, back_links/1]).

%% Compatibility interface to refcore_db
-export([backup/0, backup/1, restore/1, ls_backups/0, backup_info/1,
         undo/0, redo/0, clean/0]).
-export([is_gnode/1]).

-export([save/1, create_graph/1, rename_graph/2, ls_graphs/0, actual_graph/0,
         load_graph/1, delete_graph/1, delete_all_graphs/0]).

-export([save_envs/0]).
%-export([is_protected_node/1]).

%% gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%-export([async_db_reply/2]).

%% internal handlers
%-export([req_handler_loop/1]).

%%% ============================================================================
%%% Client functions

-include("core.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(GRAPH_TIMEOUT, infinity).

-define(Call(Request),
        begin
            %Request = list_to_tuple([Req | Args]),
            %case gen_server:call(?GRAPH_SERVER, Request, ?GRAPH_TIMEOUT) of
            case gen_refdb:sync_operation(Request, ?GRAPH_TIMEOUT) of
                ok             -> ok;
                {ok, Reply}    -> Reply;
                {error, Error} -> erlang:error(Error, Request)
            end
        end).

%% @type node().
%%  Represents a node in the graph.

%% @type data() = tuple().
%%  Represents the class and attributes of a node. This is essentially a
%%  record, the name of the record (or the first element of the tuple) is
%%  the class name, and the fields are the attributes.

%% @type path() = [PathElem]
%%       PathElem = Tag | {Tag, Index} | {Tag, Filter} | {intersect, Node, Tag}
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

%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @doc Starts the server. There will be no initial schema information,
%% {@link schema/1} must be called before the server can actually be used.
start_link() ->
    gen_server:start_link({local, ?GRAPH_SERVER}, ?MODULE, [], []).

%% @spec schema(schema()) -> init | match | mismatch
%% @doc Initialises the server with a given schema. Checks whether the
%% persistent storage has the same schema as given here, and initialises it
%% if neccessary.
schema(Schema) ->
    %gen_server:call(?GRAPH_SERVER, {schema, Schema}, ?GRAPH_TIMEOUT).
    ?Call({schema, Schema}).

%% @spec get_schema() -> schema()
%% @doc Initialises the server with a given schema. Checks whether the
%% persistent storage has the same schema as given here, and initialises it
%% if neccessary.
get_schema() ->
    ?Call({get_schema}).

%% @spec reset_schema() -> ok
%% @doc This function can be used to erase the current graph schema when a new
%% schema has to be used. Erases all data from the database.
%% @see erase_graph/0
reset_schema() ->
    ?Call({reset_schema}).

%% @spec erase_nodes() -> ok
%% @doc This function erases the contents of the database while retaining the
%% schema. This is a synchronous operation (opposed to {@link reset_schema/0}).
erase_nodes() ->
    ?Call({erase_nodes}).

%% @spec root() -> node()
%% @doc Returns the root node.
root() ->
    ?Call({root}).

%% @spec create(data()) -> node()
%% @doc Creates a new node. The class and attributes of the node are given by
%% Data.
create(Data) when is_tuple(Data) ->
    ?Call({create, Data, false}).

%% @spec create_prot(data()) -> node()
%% @doc Creates a protected new node.
create_prot(Data) when is_tuple(Data) ->
    ?Call({create, Data, true}).

%% @spec update(node(), data()) -> ok
%% @doc Updates the attributes of a node. The new attributes are given by
%% Data, which must have the same class as Node.
update(Node, Data) when ?IS_NODE(Node) ->
    ?Call({update, Node, Data}).

%% @spec delete(node()) -> ok
%% @doc Deletes the given node.
delete(Node) when ?IS_NODE(Node) ->
    ?Call({delete, Node}).

%% @spec data(node()) -> data()
%% @doc Returns the data associated with a node.
data(Node) when ?IS_NODE(Node) ->
    ?Call({data, Node}).

%% @spec class(node()) -> atom()
%% @doc Returns the node class of the node. This is equivalent to
%% `element(1, data(Node))' (but may be faster).
class({?NODETAG, Class, _Id}) ->
    Class.

%% @spec mklink(node(), atom() | {atom(), integer()}, node()) -> ok
%% @doc Creates a link between two nodes.
mklink(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To),
                           is_atom(Tag); is_tuple(Tag) ->
    ?Call({mklink, From, Tag, To, false}).

%% @spec mklink_prot(node(), atom() | {atom(), integer()}, node()) -> ok
%% @doc Creates a link between two nodes.
mklink_prot(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To),
                           is_atom(Tag); is_tuple(Tag) ->
    ?Call({mklink, From, Tag, To, true}).


%% @spec rmlink(node(), atom(), node()) -> ok
%% @doc Removes a link between two nodes.
rmlink(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To), is_atom(Tag) ->
    ?Call({rmlink, From, Tag, To}).

remove_garbage() ->
    ?Call({remove_garbage}).

%% @spec links(node()) -> [{atom(), node()}]
%% @doc Returns the links starting from a node.
links(Node) when ?IS_NODE(Node) ->
    ?Call({links, Node}).


back_links(Node) when ?IS_NODE(Node) ->
    ?Call({back_links, Node}).

%% @spec path(node(), path()) -> [node()]
%% @doc Evaluates a path expression starting from Node, and returns the
%% resulting nodes.
path(Node, []) -> [Node];
path(Node, Path) when ?IS_NODE(Node), is_list(Path) ->
    ?Call({path, Node, Path}).

%% @spec index(node(), atom(), node()) -> integer() | none
%% @doc Returns the index of a link. If there is a link between `From' and `To'
%% with a tag `Tag', then the result is the index of this link among the links
%% with tag `Tag'. Otherwise, the result is `none'.
index(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To), is_atom(Tag) ->
    ?Call({index, From, Tag, To}).

%% @spec set_prop(node(), atom(), term()) -> ok
%% @doc Set a node property.
set_prop(Node, Key, Value) when ?IS_NODE(Node), is_atom(Key) ->
    ?Call({setp, Node, Key, Value}).

%% @spec get_prop(node(), atom()) -> {ok, term()} | undefined
%% @doc Get the value of a node property.
get_prop(Node, Key) when ?IS_NODE(Node), is_atom(Key) ->
    ?Call({getp, Node, Key}).

%% @spec get_props(node()) -> [{atom(), term()}]
%% @doc Get all properties of a node.
get_props(Node) when ?IS_NODE(Node) ->
    ?Call({getp, Node}).

%% @spec del_prop(node(), atom()) -> ok
%% @doc Remove a property of a node.
del_prop(Node, Key) when ?IS_NODE(Node), is_atom(Key) ->
    ?Call({delp, Node, Key}).

%% @spec backup() -> {ok,backupfile} | error
%% @doc Creates a new checkpoint from mnesia tables and returns the
%% name of the checkpoint.
backup() ->
    backup("").

%% @spec backup(string()) -> any()
%% @doc Works as {@link ri:backup/0}, but you can attach a commit-log
%% to the backup.
backup(CommitLog) ->
    ?Call({backup, CommitLog}).

%% @doc Returns a list from the created backups.
ls_backups() ->
    ?Call({ls_backups}).

%% @spec backup_info(atom() | integer() | string()) -> any()
%% @doc Returns information about the given backup. That information
%% is actually a tuple, which first element is the backup name,
%% the second a string that contains, the time of creation of the backup,
%% and the third is the commit-log. If the commit-log has not been
%% specified, then the third element will be an empty string.
backup_info(Backup) when is_atom(Backup) or is_integer(Backup) ->
    ?Call({backup_info, Backup});
backup_info(Backup) when is_list(Backup) ->
    backup_info(list_to_atom(Backup)).

%% @spec save(atom() | string()) -> any()
%% @doc Saves the actual graph's state to the given file.
%% Note that you must specify only the name of the file,
%% the function always saves the state to the current graph's folder.
save(FileName) when is_atom(FileName) ->
    ?Call({save, FileName});
save(FileName) when is_list(FileName) ->
    save(list_to_atom(FileName)).

%% @spec restore(atom() | integer() | string()) -> any()
%% @doc Restores the given backup.
restore(Backup) when is_atom(Backup) or is_integer(Backup) ->
    ?Call({restore, Backup});
restore(Backup) when is_list(Backup) ->
    restore(list_to_atom(Backup)).

%% @spec undo() -> undo_is_ok | invalid_checkpoint_number
%% @doc Restores the previous state of the database from the last
%% checkpoint. The actual checkpoint numbers are
%% stored in the database.
undo() ->
    ?Call({undo}).

%% @spec redo() -> restored | invalid_checkpoint_number
%% @doc Restores the next state of the database from the last
%% checkpoint. The actual checkpoint numbers are
%% stored in the database.
redo() ->
    ?Call({redo}).

%% @spec clean() -> cleaned_all_backups
%% @doc Cleans all backup files from the mnesia storage
clean()->
    ?Call({clean}).

%% @spec is_gnode(term()) -> boolean()
%% @doc  Returns whether the argument is the representation of a graph node.
is_gnode({'$gn', _, _}) -> true;
is_gnode(_)             -> false.

%% @spec create_graph(atom()) -> any()
%% @doc Creates a graph, with name. That graph can be
%% restored later with the load_graph/1 function.
create_graph(Name) ->
    ?Call({create_graph, Name}).

%% @spec rename_graph(atom(), atom()) -> any()
%% @doc Renames a graph.
rename_graph(OldName, NewName) ->
    ?Call({rename_graph, OldName, NewName}).

%% @spec ls_graphs() -> list()
%% @doc Lists the created graphs.
ls_graphs() ->
     ?Call({ls_graphs}).

%% @spec actual_graph() -> atom()
%% @doc Returns the name atom of the actual graph.
actual_graph() ->
     ?Call({actual_graph}).

%% @spec load_graph(atom()) -> any()
%% @doc Restores a previously created graph. The only parameter is the
%% graph's name.
load_graph(Name) ->
    ?Call({load_graph, Name}).

%% @spec delete_graph(atom()) -> any()
%% @doc Deletes the given graph. The only parameter is the
%% graph's name.
delete_graph(Name) ->
    ?Call({delete_graph, Name}).

%% @spec delete_all_graphs() -> ok
%% @doc Removes all graphs.
delete_all_graphs() ->
    ?Call({delete_all_graphs}).

%% @spec save_envs() -> ok
%% @doc Saves the environment nodes.
save_envs() ->
    ?Call({save_envs}).

%%% ============================================================================
%%% Implementation

%%% ----------------------------------------------------------------------------
%%% Callback functions

-record(gstate, {dbmod :: atom()}).

%% @private
init(_) ->
    process_flag(trap_exit, true),
    {ok, #gstate{}}.

%% @private
handle_call(_, _, St) ->
    {noreply, St}.

handle_cast(_, St) ->
    {noreply, St}.

handle_info(_, St) ->
    {noreply, St}.

code_change(_, St, _) ->
    {ok, St}.

terminate(_Reason, _St)->
    ok.
