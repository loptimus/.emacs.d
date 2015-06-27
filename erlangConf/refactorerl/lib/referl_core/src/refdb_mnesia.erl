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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Graph storage server. This is a mnesia based implementation of
%%% the semantical graph server interface.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refdb_mnesia).
-vsn("$Rev: 8246 $ ").
-behaviour(gen_refdb).

%%% ============================================================================
%%% Exports

-export([%start_link/1,
         init/1,
         handle_operation/3,
         code_change/2,
         terminate/2]).

%% internal handlers
-export([req_handler_loop/1]).

%%% ============================================================================
%%% Implementation
%%%
%%% For each node class, there are two mnesia tables:
%%%  - one with the name of the class that stores the attributes
%%%  - one with '$lnk' appended to the class name that stores link information
%%%
%%% Every node has an integer ID, which is unique in its class. A node
%%% is represented by a {?NODETAG, Class, Id} tuple.
%%%
%%% The attribute table contains tuples of the form {Id, Data}, the key
%%% is the first element.
%%%
%%% The link table contains tuples of the form
%%%   {Class, {From, Tag}, Index, To},
%%% where Class is the name of the table, From and To are ID-s, Tag is the
%%% link tag and Index is the index of the link among the links from the
%%% same node and with the same tag. The (non-unique) key is the {From, Tag}
%%% element, and there is a mnesia index on the To element for backward
%%% links.


%%% ----------------------------------------------------------------------------
%%% Data types

%% Class table: stores the attribute names for every node class
-record(class, {name, attribs, links}).
%% Target table: stores the link target class name for every starting class,
%% link tag pair
-record(target, {start, next}).
%% Nextid table: stores the next available ID for every node class
-record(nextid, {class, id}).

%%% ----------------------------------------------------------------------------
%%% Callback functions

-define(Exec(Body), ?Db:exec(fun() -> Body end)).

-record(dbState, {has_schema      :: boolean() | schema_error,
                  schema          :: tuple(),
                  schema_list     :: list(),
                  req_handler_pid :: pid()}).

-record(rhstate, {running_queries = 0 :: integer()
                  % modified_nodes  = ets:new(modified_nodes, [])
                 }).

-include("core.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% @private
init(_) ->
    process_flag(trap_exit, true),
    Init = ?Db:init(),
    HasSchema = has_schema(Init),
    HasSchema orelse create_db_classes(),
    RHPid  = spawn(?MODULE, req_handler_loop, [#rhstate{}]), %% FIXME: no link?
    {ok, #dbState{has_schema=HasSchema, req_handler_pid=RHPid}}.

has_schema(Init) ->
    Tabs = [class, target, nextid, protected],
    ClassesExist = lists:all(fun(Tab) -> ?Db:exists(Tab) end, Tabs),
    Init == exists andalso ClassesExist.

create_db_classes() ->
    ?Db:create( class, record_info(fields,  class), []),
    ?Db:create(nextid, record_info(fields, nextid), []),
    ?Db:create(target, record_info(fields, target), []),
    ?Db:create(protected, record_info(fields, protected), [{type, bag}]).

handle_operation({get_schema}, _Extra, State) ->
    {result, {ok, get_schema_from_mnesia()}, State};
handle_operation({schema, Schema}, _Extra, State) ->
    handle_schema(Schema, State);
handle_operation({reset_schema}, _Extra, _State) ->
    handle_reset_schema();
handle_operation(_Request, _Extra, no_schema) ->
    {stop, no_schema, no_schema, no_schema};
handle_operation(_Request, _Extra, #dbState{has_schema = schema_error} = State) ->
    {stop, schema_error, schema_error, State};
handle_operation(Req, Extra, #dbState{has_schema = true, req_handler_pid = RHPid} = State) ->
    RHPid ! {add_req, is_query_req(Req), Extra, Req},
    {noresult, State}.

%% Runs queries in parallel, finishes them before a modification request.
req_handler_loop(St = #rhstate{running_queries=Qs}) ->
    NewQs =
        receive
            {add_req, true, From, Req} ->
                RHLoopPid = self(),
                spawn(
                    fun() ->
                        handle_single_req(From, Req),
                        RHLoopPid ! {query_done}
                    end),
                Qs + 1;
            {add_req, false, From, Req} ->
                [receive {query_done} -> ok end || _ <- lists:seq(1, Qs)],
                handle_single_req(From, Req),
                0;
            {query_done} ->
                Qs - 1
        end,
    req_handler_loop(St#rhstate{running_queries=NewQs}).

handle_single_req(Extra, Req) ->
    Answer = handle_req(Req),
    gen_refdb:result(Answer, Extra).

%% Returns whether the request is a query,
%% and thus runnable in parallel with other queries.
%% The requests `root', `data', `links', `path', `index' are queries by their nature.
%% `Create' can also fit there, since it is impossible to reference
%% the created node before the request returns.
is_query_req({root}           ) -> true;
is_query_req({data,   _}      ) -> true;
is_query_req({links,  _}      ) -> true;
is_query_req({path,   _, _}   ) -> true;
is_query_req({index,  _, _, _}) -> true;
is_query_req({create, _}      ) -> true;
is_query_req(_                ) -> false.


handle_req({root}                       ) -> handle_root();
handle_req({create, Data, Prot}         ) -> handle_create(Data, Prot);
handle_req({update, Node, Data}         ) -> handle_update(Data, Node);
handle_req({delete, Node}               ) -> handle_delete(Node);
handle_req({data,   Node}               ) -> handle_data(Node);
handle_req({mklink, From, Tag, To, Prot}) -> handle_mklink(From, Tag, To, Prot);
handle_req({rmlink, From, Tag, To}      ) -> handle_rmlink(From, Tag, To);
handle_req({remove_garbage}             ) -> handle_remove_garbage();
handle_req({links,  Node}               ) -> handle_links(Node);
handle_req({back_links, Node}           ) -> handle_back_links(Node);
handle_req({path,   Node, Path}         ) -> handle_path(Node, Path);
handle_req({index,  From, Tag, To}      ) -> handle_index(From, Tag, To);
handle_req({setp,   Node,Key,Val}       ) -> handle_setp(Node, Key, Val);
handle_req({getp,   Node, Key}          ) -> handle_getp(Node, Key);
handle_req({getp,   Node}               ) -> handle_getp(Node);
handle_req({delp,   Node, Key}          ) -> handle_delp(Node, Key);
handle_req({erase_nodes}                ) -> handle_erase();
handle_req({reset_schema}               ) -> handle_reset_schema();
handle_req({backup, _CommitLog}         ) -> handle_backup(); % fixme: commigLog ?
handle_req({save, before_transformation}) -> handle_backup(); % no save in mnesia!
handle_req({save, FileName}             ) -> handle_save(FileName);
handle_req({restore, Count}             ) -> handle_restore(Count);
handle_req({ls_backups}                 ) -> handle_ls_backups();
handle_req({backup_info, Backup}        ) -> handle_backup_info(Backup);
handle_req({undo}                       ) -> handle_undo();
handle_req({redo}                       ) -> handle_redo();
handle_req({clean}                      ) -> handle_clean();
handle_req({create_graph, Name}         ) -> handle_create_graph(Name);
handle_req({rename_graph, OName, NName} ) -> handle_rename_graph(OName, NName);
handle_req({ls_graphs}                  ) -> handle_ls_graphs();
handle_req({actual_graph}               ) -> handle_actual_graph();
handle_req({load_graph, Name}           ) -> handle_load_graph(Name);
handle_req({delete_graph, Name}         ) -> handle_delete_graph(Name);
handle_req({delete_all_graphs}          ) -> handle_delete_all_graphs();
handle_req({save_envs}                  ) -> save_envs().

code_change(_OldVsn, _State) ->
    no_code_change.

%% @private
terminate(_Reason, _State) ->
    %%delete_end_backups({1,up}),
    ok.


%%% ----------------------------------------------------------------------------
%%% Schema handling

%% Recovers the schema from the #class{} elements stored in Mnesia.
mnesia_get_schema() ->
    mnesia:foldl(
        fun(#class{name='$hash'}, Acc) -> Acc;
           (Elem,                 Acc) -> [Elem|Acc]
        end, [], class).

handle_reset_schema() ->
    save_envs(),
    Schema = get_schema_from_mnesia(),
    propagate_reset_to_servers(),
    ResetSchema = reset_schema(Schema),
    restore_envs(),
    ResetSchema.

%% Notify all servers about the pending reset that require it.
propagate_reset_to_servers() ->
    refcore_funprop:reset().

%% Resets the schema to the one given in the parameter.
reset_schema(Schema) ->
    ?Db:delete_all(),

    {ok, InitSt} = init(dummy_param),
    handle_schema(Schema, InitSt).


get_schema_from_mnesia() ->
    {atomic, SchemaClasses} = mnesia:transaction(fun() -> mnesia_get_schema() end),
    [{C, As, Ls} || #class{name=C, attribs=As, links=Ls} <- SchemaClasses].

%% Saves the environment configuration to `EnvConfFile'.
save_envs() ->
    EnvConfFile = conf_file(),
    {ok, Root} = handle_root(),
    {ok, Envs} = handle_path(Root, [env]),
    EnvDatas = [Data || Env <- Envs, {ok, Data} <- [handle_data(Env)]],
    case EnvDatas of
        [] ->
            no_envs_saved;
        _ ->
            {ok, Dev} = file:open(EnvConfFile, [write]),
            io:format(Dev, "~p.~n", [EnvDatas]),
            file:close(Dev)
    end.

%% Restores the environment nodes saved by `save_envs/0'.
%% @todo Should have the same behaviour as in refdb_nif:restore_envs/0.
%%       Env nodes may not be loaded from EnvConfFile, but from the
%%       previously saved graph.
restore_envs() ->
    {ok, Root} = handle_root(),
    {ok, OldEnvs} = handle_path(Root, [env]),
    case OldEnvs of
        [] ->
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
            {ok, Root} = handle_root(),
            EnvNodes = [handle_create(Env, false) || Env <- Envs],
            [handle_mklink(Root, env, Env, false) || {ok, Env} <- EnvNodes];
        _ ->
            envs_already_present
    end.

%% Returns the name of the configuration file.
conf_file() ->
    DataDir = mnesia:system_info(directory),
    EnvFileName = "refactorerl.configuration",
    filename:join(DataDir, EnvFileName).

handle_schema(Schema, St = #dbState{has_schema=false}) ->
    init_schema(lists:usort(Schema)),
    ?Exec(mnesia:write(#class{name='$hash', attribs=schema_hash(Schema)})),

    restore_envs(),

    {result, {ok, init}, St#dbState{has_schema=true}};

handle_schema(Schema, St = #dbState{has_schema=true}) ->
    case ?Exec(mnesia:read(class, '$hash', read)) of
        [#class{attribs=OldHash}] -> ok;
        _                         -> OldHash = unknown
    end,
    NewHash = schema_hash(Schema),
    case {OldHash =:= NewHash, ?autoreset_schema} of
        {true, _} ->
            {result, {ok, match}, St};
        {false, true} ->
            error_logger:info_msg("Schema is changed, resetting database.\n"),
            reset_schema(Schema),
            {result, {ok, init}, St};
        {false, false} ->
            error_logger:error_report(
              [{module, ?MODULE},
               {message,"Required and stored graph schemas are different.\n"}]),
            {result, {ok, init}, St}
    end.

schema_hash(Schema) ->
    erlang:phash2(lists:usort(Schema)).

init_schema(SchemaElems) when is_list(SchemaElems) ->
    [init_schema(Elem) || Elem <- SchemaElems];
init_schema({Class, Attribs, Links}) ->
    Class =/= root andalso
        ?Db:create(Class, [id, attribs, props], []),

    ?Db:create(linktab(Class), [id, ind, to], [{index, [to]}, {type,bag}]),

    ?Exec(
       begin
           mnesia:write(#class{name=Class, attribs=Attribs, links=Links}),
           [ mnesia:write(#target{start={Class, Tag}, next=To}) ||
               {Tag, To} <- Links]
       end).

handle_erase() ->
    Q = qlc:q([{Name, linktab(Name)} ||
                  #class{name=Name} <- mnesia:table(class),
                  Name =/= root, Name =/= '$hash']),
    ?Exec(lists:foreach(fun erase_class/1, qlc:e(Q))),
    ok.

erase_class({Class, Lnk}) ->
    erase_tab(Class),
    erase_tab(Lnk),
    mnesia:delete({nextid, Class}).

erase_tab(Tab) ->
    lists:foreach(fun mnesia:delete/1,
                  [{Tab, Key} || Key <- mnesia:all_keys(Tab)]).

%%% ----------------------------------------------------------------------------
%%% Node operations

handle_root() ->
    {ok, {?NODETAG, root, 0}}.

handle_data({?NODETAG, root, _}) ->
    {ok, {root}};
handle_data({?NODETAG, Class, Id}) ->
    case ?Exec(mnesia:read(Class, Id, read)) of
        [{_,_,Data,_}] -> {ok, Data};
        []             -> {error, bad_node}
    end.

handle_create(Data, IsProtected) ->
    Class = element(1, Data),
    Id = create_node(Class, Data),
    IsProtected andalso protect_node(Class, Id),
    {ok, {?NODETAG, Class, Id}}.

create_node(Class, Data) ->
    Id = mnesia:dirty_update_counter(nextid, Class, 1),
    ?Exec(mnesia:write({Class, Id, Data, []})),
    Id.

handle_update(Data, Node = {?NODETAG, Class, Id}) ->
    case is_protected_node(Node) of
        true ->
            % todo Reply with something other than 'ok'
            %      if the node is protected?
            ok;
        false ->
            if
                element(1, Data)=:=Class ->
                    ?Exec(
                       begin
                           [{Class, Id, _, Props}] = mnesia:read({Class, Id}),
                           mnesia:write({Class, Id, Data, Props})
                       end),
                    ok;
                true ->
                    {error, {bad_class, Class}}
            end
    end.

handle_delete(Node = {?NODETAG, Class, Id}) ->
    ?Exec(
        begin
            case mnesia:read({Class, Id}) of
                [{Class, Id, _, _Props}] -> ok;
                Other ->
                    throw({bad_delete, Node, Other})
            end,


            mnesia:delete({Class, Id}),
            delete_forward_links(Class, Id),
            delete_backward_links(Class, Id)
        end),
    ok.

delete_forward_links(Class, Id) ->
    Tags = [Tag || {Tag, _} <- link_targets(Class, fwd)],
    [ok = mnesia:delete({linktab(Class), {Id, Tag}}) ||
        Tag <- Tags].

delete_backward_links(Class, Id) ->
    Links =
        lists:foldl(
          fun
              ({FTag, FCl}, Lnk) ->
                  orddict:append(FCl, FTag, Lnk)
          end,
          [],
          link_targets(Class, back)),

    Reorders =
        [begin
            mnesia:delete_object(Rec),
            {linktab(FCl), From}
         end ||
            {FCl, Tags}             <- Links,
            Rec = {_,From = {_,FTag}, _,_} <-
                mnesia:index_match_object({linktab(FCl), '_', '_', Id}, 4),
            lists:member(FTag, Tags)],
    [begin
        Nodes = mnesia:match_object({CLnk, From, '_', '_'}),
        [ begin
            mnesia:delete_object(Obj),
            mnesia:write({CLnk, {FId, Tag}, Idx2, To})
          end
            ||  {Obj = {_CLnk, {FId, Tag}, Idx, To}, Idx2}  <- ?MISC:index_list(Nodes),
                Idx =/= Idx2]
     end ||
        {CLnk, From} <- lists:usort(Reorders)].


%%% ----------------------------------------------------------------------------
%%% Link queries

%% Note: the links need reordering, as Mnesia does not keep the order.
%% Since the links are unique, lists:usort is applicable.
handle_links({?NODETAG, Class, Id}) ->
    T = [{{Id, Tag}, TCl} || {Tag, TCl} <- link_targets(Class, fwd)],
    Q = qlc:q([ {Tag, {?NODETAG, TCl, To}} ||
                  {ST={_,Tag}, TCl} <- T,
                  {_, FL, _Ind, To} <- mnesia:table(linktab(Class)),
                  ST =:= FL]),
    Run = ?Exec(qlc:e(Q)),
    Reordered = lists:usort(Run),
    {ok, Reordered}.

handle_back_links(_Node) -> % TODO
    {ok, {not_implemented, ?MODULE, handle_back_links}}.

handle_index({?NODETAG, FCl, FId}, Tag, {?NODETAG, TCl, TId}) ->
    case link_target(FCl, Tag, fwd) of
        {class, TCl} ->
            case link_index(FCl, FId, Tag, TId) of
                []    -> {ok, none};
                [Ind] -> {ok, Ind};
                Multi when is_list(Multi) -> {ok, Multi}
            end;
        _ ->
            {error, bad_link}
    end.

handle_path({?NODETAG, Class, Id}, Path) ->
    try compile_path(Path, Class, [Id]) of
        Query ->
            {ok, ?Exec(qlc:e(Query))}
    catch
        throw:Msg ->
            {error, Msg}
    end.

%% Returns existing link indexes (normally only zero or one)
link_index(FCl, FId, Tag, TId) ->
    Q = qlc:q([Ind || {_, Key, Ind, To} <- mnesia:table(linktab(FCl)),
                      Key =:= {FId, Tag},
                      To =:= TId]),
    ?Exec(qlc:e(Q)).


%%% ----------------------------------------------------------------------------
%%% Link modifications

handle_mklink(From = {?NODETAG, FCl, FId}, TagInfo, To = {?NODETAG, TCl, TId}, IsProtected) ->
    {Tag, Ind} =
        if
            is_atom(TagInfo) -> {TagInfo, last};
            true             -> TagInfo
        end,
    case link_target(FCl, Tag, fwd) of
        {class, TCl} ->
            ?Exec(
               case {node_exists(FCl, FId), node_exists(TCl, TId)} of
                   {true, true} ->
                       do_mklink(FCl, FId, Tag, Ind, TId),
                       IsProtected andalso protect_link(FCl, FId, Tag, TId),
                       ok;
                   {false, true} ->
                       {error, {bad_node, From}};
                   {true, false} ->
                       {error, {bad_node, To}};
                   {false, false} ->
                       {error, {bad_nodes, From, To}}
               end);
        _ ->
            {error, bad_link}
       end.

do_mklink(FCl, FId, Tag, Ind, TId) ->
    Absent = link_index(FCl, FId, Tag, TId) =:= [],
    if
        not Absent -> ok;
        Ind =:= last ->
            append_link(linktab(FCl), FId, TId, Tag);
        true ->
            insert_link(linktab(FCl), FId, TId, Tag, Ind)
    end.

node_exists(root, 0)   -> true;
node_exists(Class, Id) -> mnesia:read(Class, Id) =/= [].

%% Only creates the new link
append_link(Links, From, To, Tag) ->
    Ind = length(mnesia:read(Links, {From, Tag}, read)) + 1,
    mnesia:write({Links, {From, Tag}, Ind, To}).

%% Replaces the set of links completely with the new set
insert_link(Links, From, To, Tag, Ind) ->
    LinkList = mnesia:read(Links, {From,Tag}, read),
    NewLinks =
        [ Lnk || Lnk = {_, _, I, _} <- LinkList, I < Ind ]++
        [ {Links, {From, Tag}, Ind, To} ] ++
        [ {L, F, I+1, T} || {L, F, I, T} <- LinkList, I >= Ind ],
    mnesia_replace_links(From, Tag, Links, NewLinks).


handle_rmlink(From = {?NODETAG, FCl, FId}, Tag, To = {?NODETAG, TCl, TId}) ->
    case is_protected_link(FCl, FId, Tag, TId) of
        true ->
            % todo Reply with something other than 'ok'
            %      if the node is protected?
            ok;
        false ->
            case link_target(FCl, Tag, fwd) of
                {class, TCl} ->
                    ?Exec(remove_link(linktab(FCl), FId, TId, Tag));
                _ ->
                    {error, {bad_link, From, Tag, To}}
            end
    end.

%% Replaces the set of links completely
remove_link(OldLinks, From, To, Tag) ->
    LinkList = mnesia:read(OldLinks, {From, Tag}),
    case  [I || {_, _, I, T} <- LinkList, T == To] of
        [Ind] ->
            NewLinks =
                [ Lnk || Lnk = {_, _, I, _} <- LinkList, I < Ind ] ++
                [ {L, F, I-1, T} ||
                    {L, F, I, T} <- LinkList, I > Ind ],
            mnesia_replace_links(From, Tag, OldLinks, NewLinks),
            ok;
        [] ->
            {error, not_exists};
        _ ->
            throw({multiple_links, Tag})
    end.

%% Removes the old links from Mnesia and inserts the new ones instead.
mnesia_replace_links(From, Tag, OldLinks, NewLinks) ->
    mnesia:delete({OldLinks, {From,Tag}}),
    [mnesia:write(Lnk) || Lnk <- NewLinks].

handle_remove_garbage() ->
    {ok, {not_implemented, ?MODULE, handle_rempve_garbage}}.

%%% ----------------------------------------------------------------------------
%%% Property functions

handle_setp({?NODETAG, Class, Id}, Key, Value) ->
    ?Exec(
       case node_exists(Class, Id) of
           true ->
               [{Class, Id, Data, Props}] = mnesia:read({Class, Id}),
               NewProps = [{Key, Value} | proplists:delete(Key, Props)],
               mnesia:write({Class, Id, Data, NewProps}),
               ok;
           false ->
               {error, bad_node}
       end).

handle_getp({?NODETAG, Class, Id}, Key) ->
    ?Exec(
       case node_exists(Class, Id) of
           true ->
               [{Class, Id, _Data, Props}] = mnesia:read({Class, Id}),
               Value =
                   case proplists:lookup(Key, Props) of
                       none -> undefined;
                       {Key, Val} -> {ok, Val}
                   end,
               {ok, Value};
           false ->
               {error, bad_node}
       end).

handle_getp({?NODETAG, Class, Id}) ->
    ?Exec(
       case node_exists(Class, Id) of
           true ->
               [{Class, Id, _Data, Props}] = mnesia:read({Class, Id}),
               {ok, lists:usort(Props)};
           false ->
               {error, bad_node}
       end).

handle_delp({?NODETAG, Class, Id}, Key) ->
    ?Exec(
       case node_exists(Class, Id) of
           true ->
               [{Class, Id, Data, Props}] = mnesia:read({Class, Id}),
               NewProps = proplists:delete(Key, Props),
               mnesia:write({Class, Id, Data, NewProps}),
               ok;
           false ->
               {error, bad_node}
       end).

%%% ----------------------------------------------------------------------------
%%% Path compiler

compile_path([], Class, Query) ->
    qlc:q([{?NODETAG, Class, Id} || Id <- Query]);

compile_path([S={intersect, {?NODETAG, SCl, SId}, Step} | Rest],
             Class, Query) ->
    {Tag, Dir} =
        if
            is_atom(Step) -> {Step, fwd};
            true          -> Step
        end,

    case link_target(SCl, Tag, Dir) of
        {class, Class} -> ok;
        {class, _}     -> throw({bad_class, S, Class});
        error          -> throw({bad_link, S, Class})
    end,

    Result =
        case Dir of
            fwd ->
                R =
                    qlc:q([{Ind, Id}
                            || {_, From, Ind, To} <- mnesia:table(linktab(SCl)),
                               From =:= {SId, Tag},
                               Id <- Query,
                               Id =:= To]),
                run_and_reindex(R);
            back ->
                qlc:q(
                  [Id || {_, From, _,To} <- mnesia:table(linktab(Class)),
                         SId =:= To,
                         Id <- Query,
                         From =:= {Id, Tag}])
        end,
    compile_path(Rest, Class, Result);

compile_path([Elem | Rest], Class, Query) ->
    {Dir, Filter} =
        case Elem of
            {{Tag, back}, Filt} when is_atom(Tag) -> {back, Filt};
            {Tag,  back}        when is_atom(Tag) -> {back, {}};
            {Tag,         Filt} when is_atom(Tag) -> {fwd,  Filt};
            Tag                 when is_atom(Tag) -> {fwd,  {}};
            Tag                                   -> throw({bad_path, Elem})
        end,

    NextClass =
        case link_target(Class, Tag, Dir) of
            {class, C} -> C;
            error      -> throw({bad_path, Class, Elem})
        end,

    Cond =
        case Filter of
            {} ->
                fun(_Fr, _Ind, _Id) -> true end;
            Index when is_integer(Index), Dir =:= fwd ->
                fun(_Fr, Ind, _Id) -> Ind =:= Index end;
            last when Dir =:= fwd ->
                fun
                    (Fr, Ind, _Id) ->
                        L = length(mnesia:read({linktab(Class), {Fr, Tag}})),
                        Ind =:= L
                end;
            {Ind1, last} when is_integer(Ind1), Dir =:= fwd ->
                fun(_Fr, Ind, _Id) -> Ind >= Ind1 end;
            {Ind1, Ind2} when is_integer(Ind1),
                              is_integer(Ind2),
                              Dir =:= fwd ->
                fun(_Fr, Ind, _Id) -> Ind >= Ind1 andalso Ind < Ind2 end;
            _ ->
                compile_filter(attribs(NextClass), NextClass, Filter)
        end,

    %% This query generates the result set. It depends on that objects for the
    %% same key preserve their insertion order, which gives the correct order
    %% in the result in case of forward links -- this needs to be checked in
    %% case of Mnesia. Backward links do not maintain order.
    Result =
        case Dir of
            fwd ->
                %% This query gives a result that enables QLC to recognise the
                %% lookup join in the next query
                Keys = qlc:q([{{Id, Tag}} || Id <- Query]),
                R =
                    qlc:q([{Ind, To} || {K1} <- Keys,
                                 {_, K2, Ind, To} <- mnesia:table(linktab(Class)),
                                 K1 =:= K2, Cond(element(1,K1), Ind, To)]),
                run_and_reindex(R);
            back ->
                Keys = qlc:q([{Id} || Id <- Query]),
                qlc:q([From || {_, {From, T}, _, To}
                                   <- mnesia:table(linktab(NextClass)),
                               {Id} <- Keys, To =:= Id, T =:= Tag,
                               Cond(Id, -1, From)])
        end,
    compile_path(Rest, NextClass, Result).

%% Runs the query and reorders the acquired table part by the indices.
%% This is needed because Mnesia does not maintain the table as sorted.
run_and_reindex(Query) ->
    Result = ?Exec(qlc:e(Query)),
    {_Idxs, Query2} = lists:unzip(lists:usort(Result)),
%    case length(_Idxs) /= length(Result) of
%        true -> throw(index_error);
%        false -> ok
%    end,
    Query2.

compile_filter(AttrInfo, Attrs, {'not', Filter}) ->
    F = compile_filter(AttrInfo, Attrs, Filter),
    fun(Fr, Ind, Id) -> not F(Fr, Ind, Id) end;
compile_filter(AttrInfo, Attrs, {Filt1, Op, Filt2})
  when Op =:= 'and' orelse Op =:= 'or' ->
    F1 = compile_filter(AttrInfo, Attrs, Filt1),
    F2 = compile_filter(AttrInfo, Attrs, Filt2),
    case Op of
        'and' -> fun(Fr, Ind, Id) -> F1(Fr,Ind,Id) andalso F2(Fr,Ind,Id) end;
        'or' ->  fun(Fr, Ind, Id) -> F1(Fr,Ind,Id) orelse  F2(Fr,Ind,Id) end
    end;
compile_filter(AttrInfo, Attrs, {Attr, Op, Value})
  when is_atom(Attr) ->
    case Op of
        '==' -> OpF = fun(A,B) -> A =:= B end;
        '/=' -> OpF = fun(A,B) -> A =/= B end;
        '<'  -> OpF = fun(A,B) -> A <   B end;
        '=<' -> OpF = fun(A,B) -> A =<  B end;
        '>'  -> OpF = fun(A,B) -> A >   B end;
        '>=' -> OpF = fun(A,B) -> A >=  B end
    end,
    Ind = indexof(Attr, AttrInfo) + 1,
    if
        Ind =:= 0 -> throw({bad_attribute, Attr});
        true ->
            fun (_Fr, _Ind, Id) ->
                    [{_,_,Data,_}] = mnesia:read(Attrs, Id, read),
                    OpF(element(Ind, Data), Value)
            end
    end;
compile_filter(_, _, Cond) ->
    throw({bad_condition, Cond}).

indexof(El, Lst) -> indexof(El, Lst, 1).

indexof(_,  [],        _)   -> -1;
indexof(El, [El | _],  Ind) -> Ind;
indexof(El, [_  | Tl], Ind) -> indexof(El, Tl, Ind+1).


%% TODO: this would probably be more efficient with a table stored in the
%% process state
linktab(Class) ->
    list_to_atom(atom_to_list(Class)++"$lnk").


%%% ----------------------------------------------------------------------------
%%% Node protection

protect_node(Class, Id) ->
    ?Exec(mnesia:dirty_write(#protected{type=node, infos={Class, Id}})).

protect_link(FCl, FId, Tag, TId) ->
    ?Exec(mnesia:dirty_write(#protected{type=link, infos={FCl, FId, Tag, TId}})).

is_protected_node({'$gn', Class, Id}) ->
    NodeInfo = #protected{type=node, infos={Class, Id}},
    [NodeInfo] == ?Exec(mnesia:match_object(NodeInfo)).

is_protected_link(FCl, FId, Tag, TId) ->
    LinkInfo = #protected{type=link, infos={FCl, FId, Tag, TId}},
    [LinkInfo] == ?Exec(mnesia:match_object(LinkInfo)).

%%% ----------------------------------------------------------------------------
%%% Schema queries

%% Maybe these would also be quicker with a table in the process state, it
%% should be measured

link_target(Class, Tag, fwd) ->
    case ?Db:exec(fun() -> mnesia:read(target, {Class, Tag}) end, rd) of
        []                  -> error;
        [#target{next=TCl}] -> {class, TCl}
    end;

link_target(Class, Tag, back) ->
    Q = qlc:q([FCl || #target{start={FCl, T}, next=TCl} <- mnesia:table(target),
                      TCl =:= Class, T =:= Tag]),
    case ?Db:exec(fun() -> qlc:e(Q) end, rd) of
        []    -> error;
        [TCl] -> {class, TCl}
    end.

link_targets(Class, fwd) ->
    Q = qlc:q([{Tag, TCl} ||
                  #target{start={FCl, Tag}, next=TCl} <- mnesia:table(target),
                  FCl =:= Class]),
    ?Db:exec(fun() -> qlc:e(Q) end, rd);

link_targets(Class, back) ->
    Q = qlc:q([{Tag, FCl} ||
                  #target{start={FCl, Tag}, next=TCl} <- mnesia:table(target),
                  TCl =:= Class]),
    ?Db:exec(fun() -> qlc:e(Q) end, rd).

attribs(Class) ->
    ?Db:exec(fun() ->
                     [#class{attribs=Attribs}] =
                         mnesia:read(class, Class),
                     Attribs
             end, rd).

%%%
%%% backup / restore / undo / redo

handle_backup() ->
    ?Db:backup().

handle_save(_FileName) ->
    {ok, {not_implemented, ?MODULE, handle_save}}.

handle_restore(Count) ->
    ?Db:restore(Count).

handle_ls_backups() ->
    ?Db:ls_backups().

handle_backup_info(_Backup) -> % TODO
    {ok, {not_implemented, ?MODULE, handle_backup_info}}.

handle_undo() ->
    {ok, ?Db:undo()}.

handle_redo() ->
    ?Db:redo().

handle_clean() ->
    ?Db:clean().

%%%
%%% todo other unimplemented stuff

handle_create_graph(_Name) ->
    multi_graph_error().
handle_rename_graph(_OName, _NName) ->
    multi_graph_error().
handle_ls_graphs() ->
    multi_graph_error().
handle_actual_graph() ->
    multi_graph_error().
handle_load_graph(_Name) ->
    multi_graph_error().
handle_delete_graph(_Name) ->
    multi_graph_error().
handle_delete_all_graphs() ->
    multi_graph_error().

multi_graph_error() ->
    {ok, "In mnesia there can be only one graph"}.
