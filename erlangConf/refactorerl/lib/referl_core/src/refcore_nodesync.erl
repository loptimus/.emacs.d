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

%%% @doc Node operation synchroniser. The goal of thi server is to synchronise
%%% semantic node operations between concurrently running analysers. These
%%% operations are finding semantic nodes, adding references, and removing
%%% references. When a semantic node has no more reference, it can be deleted;
%%% this server does this too.
%%%
%%% == Node specifications and references ==
%%%
%%% A node specification identifies a semantic node, for example by providing
%%% the name of the represented entity. Sometimes a specification makes it
%%% possible to create the node if it does not exist. Semantic nodes may be
%%% specified by themselves.
%%%
%%% A node reference is a link from another node. There are more kinds of
%%% references for each type of semantic node. Some kinds of references are
%%% unique (e.g. the definition of an entity), these references may be
%%% specified without the referring node.
%%%
%%% == Supported node types ==
%%%
%%% === Modules ===
%%% <dl>
%%%  <dt>Node type:</dt><dd>`module'</dd>
%%%  <dt>Specifications:</dt><dd>
%%%    <ul>
%%%     <li>{@type integer(-1)}: an opaque module (gets created)</li>
%%%     <li>{@type atom()}: module name (gets created)</li>
%%%     <li>{@type node()}: a `#file{}' node specifies the module defined by
%%%      the file (gets created), a `#module{}' node specifies itself</li>
%%%    </ul>
%%%  </dd>
%%%  <dt>References:</dt><dd>
%%%    <ul>
%%%     <li>{@type {ctx, node()@}}: a `modctx' link from a `#clause{}' node</li>
%%%     <li>{@type {ref, node()@}}: a `modref' link from a `#expr{}' node</li>
%%%     <li>{@type def}: a `moddef' link (only for deletion)</li>
%%%    </ul>
%%%  </dd>
%%% </dl>
%%%
%%% === Functions ===
%%% <dl>
%%%  <dt>Node type:</dt><dd>`func'</dd>
%%%  <dt>Specifications:</dt><dd>
%%%    <ul>
%%%     <li>{@type {Mod, {atom(), integer()@}@}}: `Mod' is a module
%%%      specification, the function in this module is specified by its
%%%      name and arity (gets created). If the arity is negative, then
%%%      an opaque function node is created.</li>
%%%     <li>{@type {Mod, {integer(-1), integer()@}@}}: `Mod' is a module
%%%     specification, the function name is opaque (an opaque function
%%%     node gets created). The arity might be negative as well.</li>
%%%     <li>{@type node()}: a `#form{}' node specifies the function defined by
%%%      the form (does not get created), a `#func{}' node specifies itself</li>
%%%    </ul>
%%%  </dd>
%%%  <dt>References:</dt><dd>
%%%    <ul>
%%%     <li>{@type {lref, node()@}}: a `funlref' link from a `#expr{}' node</li>
%%%     <li>{@type {eref, node()@}}: a `funeref' link from a `#expr{}' node</li>
%%%     <li>{@type {dynlref, node()@}}: a `dynfunlref' link from a
%%%     `#expr{}' node</li>
%%%     <li>{@type {dyneref, node()@}}: a `dynfuneref' link from a
%%%     `#expr{}' node</li>
%%%     <li>{@type {amblref, node()@}}: a `ambfunlref' link from a
%%%     `#expr{}' node</li>
%%%     <li>{@type {amberef, node()@}}: a `ambfuneref' link from a
%%%     `#expr{}' node</li>
%%%     <li>{@type {def, node()@}}: a `fundef' link from a `#form{}' node</li>
%%%    </ul>
%%%  </dd>
%%% </dl>
%%%
%%% === Preprocessor substitutions ===
%%% Not really semantic nodes, cannot be created or referenced here,
%%% their removal must be synchronised and it is best placed here.
%%% <dl>
%%%  <dt>Node type:</dt><dd>`subst'</dd>
%%%  <dt>Specifications:</dt><dd>
%%%    <ul>
%%%     <li>{@type node()}: a `#lex{}' node specifies itself</li>
%%%    </ul>
%%%  </dd>
%%%  <dt>References:</dt><dd>
%%%    <ul>
%%%     <li>{@type {atom(), node()@}}: an arbitrary link tag and source
%%%       node</li>
%%%    </ul>
%%%  </dd>
%%% </dl>
%%%
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>
%%% @author Melinda Toth <toth_m@inf.elte.hu>


-module(refcore_nodesync).
-vsn("$Rev: 8263 $ ").
-behaviour(gen_server).

-export([get_node/2, add_ref/3, del_ref/3, move_refs/4, clean/0]).

-define(NODESYNC_TIMEOUT, infinity).

%% gen_server exports

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("core.hrl").

%%% @type nodetype() = atom(). A node type name. See {@section Supported node
%%% types} for possible values.
%%%
%%% @type nodespec() = term(). A node specification. Must match the node type
%%% provided in the call, see {@section Supported node types} for possible
%%% values.
%%%
%%% @type noderef() = {atom(), node()} | atom(). A node reference. Must match
%%% the node type provided in the call, see {@section Supported node types}
%%% for possible values.

%% @spec get_node(nodetype(), nodespec()) -> gnode() | not_found
%% @doc Returns the node specified by `Spec'. If it does not exist and it does
%% not get created, `not_found' is returned.
get_node(Type, Spec) ->
    gen_server:call(?NODESYNC_SERVER, {get, Type, Spec}, ?NODESYNC_TIMEOUT).

%% @spec add_ref(nodetype(), noderef(), nodespec()) -> ok
%% @doc Adds reference `Ref' to the node specified by `Spec'.
add_ref(Type, Ref, Spec) ->
    gen_server:call(?NODESYNC_SERVER, {add, Type, Ref, Spec}, ?NODESYNC_TIMEOUT).

%% @spec del_ref(nodetype(), noderef(), nodespec()) -> ok
%% @doc Removes reference `Ref' to the node specified by `Spec'.
del_ref(Type, Ref, Spec) ->
    gen_server:call(?NODESYNC_SERVER, {del, Type, Ref, Spec}, ?NODESYNC_TIMEOUT).

%% @spec move_refs(nodetype(), [atom()], nodespec(), nodespec()) -> ok
%% @doc Removes a set of references from the node specified by `From' and add
%% them to the node specified by `To'. References are specified by their name
%% found in {@type noderef()}.
move_refs(Type, Refs, From, To) ->
    gen_server:call(?NODESYNC_SERVER, {move, Type, Refs, From, To}, ?NODESYNC_TIMEOUT).

%% @spec clean() -> ok
%% @doc Deletes all nodes that have been unreferenced since the last call of
%% this function and have no remaining references.
clean() ->
    gen_server:call(?NODESYNC_SERVER, clean, ?NODESYNC_TIMEOUT).

%% @private
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @doc Starts the server.
start_link() ->
    gen_server:start_link({local, ?NODESYNC_SERVER}, ?MODULE, [], []).

%% @private
init(_) ->
    {ok, ets:new(deref_nodes, [])}.

%% @private
handle_call({get,  T, S},    _From, St)  -> handle_get(T, S, St);
handle_call({add,  T, R, S}, _From, St)  -> handle_add(T, R, S, St);
handle_call({del,  T, R, S}, _From, St)  -> handle_del(T, R, S, St);
handle_call({move, T, R, Fr, To}, _, St) -> handle_move(T, R, Fr, To, St);
handle_call(clean, _, St)                -> handle_clean(St).

%%% @private
handle_cast(_, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_Old, State, _Extra) ->
    {ok, State}.


%%% ============================================================================
%%% Server implementation

handle_get(module, R, St)    -> {reply, get_module(R, St), St};
handle_get(func, R, St)      -> {reply, get_func(R, St), St};
handle_get(fret, R, St)      -> {reply, get_fret(R, St), St};
handle_get(fpar, R, St)      -> {reply, get_fpar(R, St), St}.

handle_add(module, R, S, St) -> {reply, add_modref(R, S, St), St};
handle_add(func,   R, S, St) -> {reply, add_funref(R, S, St), St};
handle_add(rec,    R, S, St) -> {reply, add_recref(R, S, St), St};
handle_add(field,  R, S, St) -> {reply, add_fldref(R, S, St), St}.

handle_del(module, R, S, St) -> {reply, del_modref(R, S, St), St};
handle_del(func,   R, S, St) -> {reply, del_funref(R, S, St), St};
handle_del(rec,    R, S, St) -> {reply, del_recref(R, S, St), St};
handle_del(field,  R, S, St) -> {reply, del_fldref(R, S, St), St};
handle_del(subst,  R, S, St) -> {reply, del_subref(R, S, St), St}.

handle_move(module, R, Fr, To, St) -> {reply, move_modrefs(R, Fr, To, St), St};
handle_move(func,   R, Fr, To, St) -> {reply, move_funrefs(R, Fr, To, St), St};
handle_move(rec ,   R, Fr, To, St) -> {reply, move_recrefs(R, Fr, To, St), St};
handle_move(field,  R, Fr, To, St) -> {reply, move_fldrefs(R, Fr, To, St), St}.

handle_clean(Drf) ->
    Nodes = [{?Graph:class(N), N} || {N} <- ets:tab2list(Drf)],
    [clean_field(F)  || {field, F}  <- Nodes],
    [clean_record(R) || {record, R} <- Nodes],
    [clean_func(F)   || {func, F}   <- Nodes],
    [clean_module(M) || {module, M} <- Nodes],
    ets:delete_all_objects(Drf),
    {reply, ok, Drf}.

%% -----------------------------------------------------------------------------
%% Module operations

get_module(-1, Drf) -> get_module('/opaque', Drf);
get_module(Name, Drf) when is_atom(Name) ->
    case ?Graph:path(?Graph:root(), [{module, {name, '==', Name}}]) of
        [Mod] -> Mod;
        []    -> create(?Graph:root(), module, #module{name=Name}, Drf)
    end;

get_module(Node, Drf) ->
    case ?Graph:class(Node) of
        file ->
            case ?Graph:path(Node, [moddef]) of
                [Mod] -> Mod;
                []    ->
                    Mod = create(Node, moddef, #module{name=[]}, Drf),
                    ?Graph:mklink(?Graph:root(), module, Mod),
                    Mod
            end;
        module ->
            Node
    end.


add_modref(Ref, Spec, Drf) ->
    Mod = get_module(Spec, Drf),
    ets:delete(Drf, Mod),
    case Ref of
        {ctx, Cls}     -> ok = ?Graph:mklink(Cls, modctx, Mod);
        {ref, Expr}    -> ok = ?Graph:mklink(Expr, modref, Mod);
        {exp, Attr} -> ok = ?Graph:mklink(Mod, funexp, Attr);
        {imp, Attr} -> ok = ?Graph:mklink(Mod, funimp, Attr)
    end.

del_modref(Ref, Spec, Drf) ->
    Mod = get_module(Spec, Drf),
    ets:insert(Drf, {Mod}),
    case Ref of
        {ctx, Cls}   -> ok = ?Graph:rmlink(Cls, modctx, Mod);
        {ref, Expr}  -> ok = ?Graph:rmlink(Expr, modref, Mod);
        def ->
            case ?Graph:path(Mod, [{moddef, back}]) of
                [] -> ok;
                [Def] -> ok = ?Graph:rmlink(Def, moddef, Mod)
            end
    end.

move_modrefs(Refs, From, To, Drf) ->
    lists:foreach(fun(R) -> move_modref(R, From, To, Drf) end, Refs).


move_modref(imp, From, To, Drf) ->
    move_refs({funimp, back}, fun get_module/2, From, To, Drf);
move_modref(exp, From, To, Drf) ->
    move_refs({funexp, back}, fun get_module/2, From, To, Drf);
move_modref(func, From, To, Drf) ->
    move_refs({func, back}, fun get_module/2, From, To, Drf);
move_modref(def, From, To, Drf) ->
    move_refs(moddef, fun get_module/2, From, To, Drf);
move_modref(ctx, From, To, Drf) ->
    move_refs(modctx, fun get_module/2, From, To, Drf);
move_modref(ref, From, To, Drf) ->
    move_refs(modref, fun get_module/2, From, To, Drf).


clean_module(Mod) ->
    clean_node(Mod, [{moddef, back}, {modref, back}, {modctx, back}, func,
                     funimp, funexp])
        andalso
        begin
            case ?Graph:path(Mod, [{module, back}]) of
                [Root] -> ?Graph:rmlink(Root, module, Mod);
                _    -> ok
            end,
            ?Graph:delete(Mod)
        end.

%% -----------------------------------------------------------------------------
%% Function operations

-define(DIRTY_BIFS, [{apply,2}, {apply,3}, {cancel_timer,1},
        {check_process_code,2}, {delete_module,1}, {demonitor,2},
        {demonitor,1}, {disconnect_node,1}, {erase,1}, {erase,0},
        {exit,2}, {exit,1}, {group_leader,2}, {group_leader,0},
        {halt,1}, {halt,0}, {link,1}, {load_module,2},
        {monitor_node,3}, {monitor_node,2}, {open_port,2},
        {port_close,1}, {port_command,3}, {port_command,2},
        {port_control,3}, {process_flag,3}, {process_flag,2},
        {processes,0}, {purge_module,1}, {put,2}, {register,2},
        {registered,0}, {resume_process,1}, {self,0}, {send,3},
        {send,2}, {send_after,3}, {send_nosuspend,2},
        {send_nosuspend,3}, {spawn,2}, {spawn,4}, {spawn,1},
        {spawn,3}, {spawn_link,2}, {spawn_link,4}, {spawn_link,1},
        {spawn_link,3}, {spawn_opt,2}, {spawn_opt,3}, {spawn_opt,4},
        {spawn_opt,5}, {spawn_opt,1}, {suspend_process,1},
        {suspend_process,2}, {system_flag,2}, {throw,1}, {trace,3},
        {trace_info,2}, {trace_pattern,3}, {trace_pattern,2},
        {unlink,1}, {unregister,1}, {yield,0}]).


%% Returns the function node belonging to Mod:Name/Ary. Creates the
%% node if it does not exist. When `Arity' is negative, the function
%% arity is opaque (cannot be inferred in compile-time), and the
%% number stands for a lower limit of the function arity.
get_func({Mod, {Name, Ary}}, Drf) when is_atom(Name) orelse Name =:= -1,
                                       is_integer(Ary) ->
    MN = get_module(Mod, Drf),
    case lookup_func({MN, Mod}, Name, Ary) of
        [Fun] when Ary >= 0, Name /= -1, Mod /= -1 ->
            Fun;
        Funs ->
            create_fun(Mod, Name, Ary, MN, Funs, Drf)
    end;
get_func(Node, _Drf) ->
    case ?Graph:class(Node) of
        form ->
            case ?Graph:path(Node, [fundef]) of
                [Fun] -> Fun;
                []    -> throw(not_found)
            end;
        func ->
            Node
    end.

get_fpar(Node, Drf) ->
    Fun = get_func(Node, Drf),
    case ?Graph:path(Fun, [fpar]) of
        [] -> 
            #func{arity=Ary} = ?Graph:data(Fun),
            FPars = [?Graph:create(#expr{type=fpar}) || _ <- lists:seq(1,Ary)],
            [?Graph:mklink(Fun, fpar, FPar) || FPar <- FPars], 
            FPars;
        FPars -> FPars
    end.   
%%    ?Graph:path(get_func(Node, Drf), [fpar]).

get_fret(Node, Drf) ->
    Fun = get_func(Node, Drf),
    case ?Graph:path(Fun, [fret]) of
        [] -> FRet = ?Graph:create(#expr{type=fret}),
              ?Graph:mklink(Fun, fret, FRet), 
              FRet;
        [FRet] -> FRet
    end.
%%    ?Graph:path(get_func(Node, Drf), [fret]).

create_fun(Mod, Name, Ary, MN, Funs, Drf) ->
    Opaque = case {Mod =:= -1, Name =:= -1, Ary < 0} of
                 {false, false, false} -> false;
                 {true,  false, false} -> module;
                 {false, true , false} -> name;
                 {false, false, true } -> arity;
                 %% TODO: should we support '/opaque':opaque/-1? etc
                 {_, _, _} -> throw("too many unknown function attributes")
             end,
    Node = create(MN, func, #func{name = case Name of
                                             -1 -> opaque;
                                             _  -> Name
                                         end,
                                  arity = Ary,
                                  opaque = Opaque}, Drf),
    [?Graph:mklink(Node, may_be, F) || F <- Funs],

    %% add_param_nodes(Node, Ary),
    %% add_return_node(Node),
    case Opaque of
        false -> update_opaques(Node), Node;
        _ -> {opaque, Node}
    end.

update_opaques(Fun) ->
    [update_opaque(O, Fun) ||
        O <- ?Graph:path(?Graph:root(), [module,{func,{opaque,'/=',false}}])].

update_opaque(OFun, Fun) ->
    [Mod]  = ?Graph:path(Fun, [{func, back}]),
    [OMod] = ?Graph:path(Fun, [{func, back}]),
    #func{name = Name, arity = Arity} = ?Graph:data(Fun),
    #func{name = OName, arity = OArity, opaque = Opaque} = ?Graph:data(OFun),
    update_opaque(Opaque, OMod, OFun, OName, OArity, Mod, Fun, Name, Arity).

%% TODO: cleanup
update_opaque(name, OMod, OFun, opaque, OArity, Mod, Fun, _Name, Arity)
  when OMod == Mod, OArity == Arity ->
    ?Graph:mklink(OFun, may_be, Fun);
update_opaque(arity, OMod, OFun, OName, -1, Mod, Fun, Name, _Arity)
  when OMod == Mod, OName == Name ->
    ?Graph:mklink(OFun, may_be, Fun);
update_opaque(arity, OMod, OFun, OName, OArity, Mod, Fun, Name, Arity)
  when OMod == Mod, OName == Name, abs(OArity)-1 =< Arity ->
    ?Graph:mklink(OFun, may_be, Fun);
update_opaque(module, _OMod, OFun, OName, OArity, _Mod, Fun, Name, Arity)
  when OName == Name, OArity == Arity ->
    ?Graph:mklink(OFun, may_be, Fun);
update_opaque(_, _, _, _, _, _, _, _, _) ->
    ok.

lookup_func({MN, Mod}, -1, Ary) when Mod /= -1, Ary >= 0 ->
    ?Graph:path(MN, [{funimp,{arity,'==',Ary}}]) ++
        ?Graph:path(MN, [{func,{arity,'==',Ary}}]);
lookup_func({MN, Mod}, Name, Ary) when Mod /= -1, Name /= -1 ->
    OP = if Ary >= 0 -> '==';
            true     -> '>='
         end,
    AAry = if Ary == -1 -> -1;
              Ary < 0   -> abs(Ary)-1;
              true      -> Ary
           end,
    ?Graph:path(MN, [{funimp,{{name,'==',Name},'and',{arity,OP,AAry}}}]) ++
        ?Graph:path(MN, [{func,{{name,'==',Name},'and',{arity,OP,AAry}}}]);
lookup_func({_, -1}, Name, Ary) when Name /= -1, Ary >= 0 ->
    ?Graph:path(?Graph:root(),
                [module, {func,{{name,'==',Name},'and',{arity,'==',Ary}}}]);
lookup_func(_, _, _) ->
    [].

add_funref(Ref, Spec, Drf) ->
    try get_func(Spec, Drf) of % {Mod, {Name, Ary}}
        Fun -> add_funref(Ref, Spec, Drf, Fun)
    catch _:_ ->
            ok
    end.

add_funref(Ref, Spec, Drf, Fun) ->
    ets:delete(Drf, Fun),
    case Ref of
        {exp, {Expr, ModSpec}} ->
            Mod = get_module(ModSpec, Drf),
            ok = ?Graph:mklink(Expr, funlref, Fun),
            ok = ?Graph:mklink(Mod, funexp, Fun);
        {imp, {Expr, ModSpec}} ->
            {_DefMod, {Name, Arity}} = Spec,
            Mod = get_module(ModSpec, Drf),
            case ?Graph:path(Mod, [{func, {{name, '==', Name}, 'and',
                                          {arity, '==', Arity}}}]) of
                [] ->
                    ok = ?Graph:mklink(Expr, funeref, Fun),
                    ok = ?Graph:mklink(Mod, funimp, Fun);
                [OldFun] ->
                    move_refs(funeref, fun get_func/2, OldFun, Fun, Drf),
                    move_refs(funlref, fun get_func/2, OldFun, Fun, Drf), %% TODO: dyn and ambdyn?
                    move_refs(fundef,  fun get_func/2, OldFun, Fun, Drf),
                    move_refs(funcall, fun get_func/2, OldFun, Fun, Drf),
                    move_funref(fret, OldFun, Fun, Drf),
                    move_funref(fpar, OldFun, Fun, Drf),
                    ets:insert(Drf, Fun),
                    ok = ?Graph:rmlink(Mod, func, OldFun),
                    ok = ?Graph:mklink(Expr, funeref, Fun),
                    ok = ?Graph:mklink(Mod, funimp, Fun)
            end;
        {lref, Expr}       -> add_funlref(Drf, Fun, Expr, funlref);
        {dynlref, Expr}    -> add_funlref(Drf, Fun, Expr, dynfunlref);
        {eref, Expr}       -> add_funeref(Fun, Expr, funeref);
        {dyneref, Expr}    -> add_funeref(Fun, Expr, dynfuneref);
        {def, Form} ->
            case ?Graph:index(Form, fundef, Fun) of
                none -> ok = ?Graph:mklink(Form, fundef, Fun);
                _    -> ok
            end;
        {localdef, Expr} ->
            case ?Graph:index(Expr, localfundef, Fun) of
                none -> ok = ?Graph:mklink(Expr, localfundef, Fun);
                _    -> ok
            end
    end.

add_funlref(Drf, {opaque, Fun}, Expr, dynfunlref) ->
    add_funlref(Drf, Fun, Expr, ambfunlref);
%%add_funlref(Drf, {heuristic, Fun}, Expr, dynfunlref) ->
%%    add_funlref(Drf, Fun, Expr, dynfunlref);
add_funlref(Drf, Fun, Expr, Lnk) ->
    #func{name=Name, arity=Ary} = ?Graph:data(Fun),
    #expr{type=Type}            = ?Graph:data(Expr),
    case erl_internal:bif(Name, Ary) orelse
        (Type =:= guard andalso erl_internal:type_test(Name, Ary)) of
        true ->
            ets:insert(Drf, {Fun}),
            Bif = get_func({erlang, {Name, Ary}}, Drf),
            case lists:member({Name, Ary}, ?DIRTY_BIFS) of
                false -> ?Graph:update(Bif, (?Graph:data(Bif))#func{dirty=no});
                _     -> ok
            end,
            ets:delete(Drf, {Bif}),
            ok = ?Graph:mklink(Expr, Lnk, Bif);
        false -> ok = ?Graph:mklink(Expr, Lnk, Fun)
    end.

add_funeref({opaque, Fun}, Expr, dynfuneref) ->
    add_funeref(Fun, Expr, ambfuneref);
%%add_funeref({heuristic, Fun}, Expr, dynfuneref) ->
%%    add_funeref(Fun, Expr, dynfuneref);
add_funeref(Fun, Expr, Lnk) ->
    ok = ?Graph:mklink(Expr, Lnk, Fun).

del_funref(Ref, Spec, Drf) ->
    try get_func(Spec, Drf) of
        Fun ->
            ets:insert(Drf, {Fun}),
            case Ref of
                {imp,  File} ->
                    case ?Graph:path(File, [moddef]) of
                        [Mod] -> ok = ?Graph:rmlink(Mod,  funimp,  Fun);
                        _ -> ok
                    end;
                {exp,  Mod} ->
                    ok = ?Graph:rmlink(Mod,  funexp,  Fun);
                {lref, Expr} ->
                    ok = ?Graph:rmlink(Expr, funlref, Fun);
                {dynlref, Expr} ->
                    ok = ?Graph:rmlink(Expr, dynfunlref, Fun);
                {amblref, Expr} ->
                    ok = ?Graph:rmlink(Expr, ambfunlref, Fun);
                {eref, Expr} ->
                    ok = ?Graph:rmlink(Expr, funeref, Fun);
                {dyneref, Expr} ->
                    ok = ?Graph:rmlink(Expr, dynfuneref, Fun);
                {amberef, Expr} ->
                    ok = ?Graph:rmlink(Expr, ambfuneref, Fun);
                {def,  Form} ->
                    ok = ?Graph:rmlink(Form, fundef,  Fun)
            end
    catch
        not_found -> ok;
        _:_ -> ok
    end.


clean_func(Func) ->
    FunDefs    = [{fundef, back}, {funeref, back}, {funlref, back}],
    DynFunDefs = [{dynfuneref, back}, {dynfunlref,back},
                  {ambfuneref, back}, {ambfunlref,back}],
    AllFunDefs = FunDefs ++ DynFunDefs,

    clean_node(Func, AllFunDefs) andalso
        begin
            ?FunProp:remove(Func),
            ?Graph:delete(Func)
        end.


move_funrefs(Refs, From, To, Drf) ->
    lists:foreach(fun(R) -> move_funref(R, From, To, Drf) end, Refs).

move_funref(fret, From, To, Drf) ->
    move_refs(flow, fun get_fret/2, From, To, Drf),
    move_refs({ret,back}, fun get_fret/2, From, To, Drf);
move_funref(fpar, From, To, Drf) ->
    move_refs({flow, back}, fun get_fpar/2, From, To, Drf),
    move_refs(call, fun get_fpar/2, From, To, Drf);
move_funref(def, From, To, Drf) ->
    move_refs(fundef, fun get_func/2, From, To, Drf);
move_funref(eref, From, To, Drf) ->
    move_refs(funeref, fun get_func/2, From, To, Drf);
move_funref(lref, From, To, Drf) ->
    move_refs(funlref, fun get_func/2, From, To, Drf);
move_funref({call, forw}, From, To, Drf) ->
    move_refs(funcall, fun get_func/2, From, To, Drf);
move_funref({call, back}, From, To, Drf) ->
    move_refs({funcall, back}, fun get_func/2, From, To, Drf);
move_funref(call, From, To, Drf) ->
    move_refs(funcall, fun get_func/2, From, To, Drf),
    move_refs({funcall, back}, fun get_func/2, From, To, Drf);
move_funref(exp, From, To, Drf) ->
    move_refs(funexp, fun get_func/2, From, To, Drf);
move_funref(imp, From, To, Drf) ->
    move_refs(funimp, fun get_func/2, From, To, Drf);

move_funref({def, Node}, From, To, Drf) ->
    move_refs({fundef, Node}, fun get_func/2, From, To, Drf);
move_funref({eref, Node}, From, To, Drf) ->
    move_refs({funeref, Node}, fun get_func/2, From, To, Drf);
move_funref({lref, Node}, From, To, Drf) ->
    move_refs({funlref, Node}, fun get_func/2, From, To, Drf).

%% -----------------------------------------------------------------------------
%% Record operations

get_rec({File, Name}, Drf) when is_atom(Name) ->
    case ?Graph:path(File, [incl, {record, {name, '==', Name}}]) of
        [Rec] -> Rec;
        []    -> create(File, record, #record{name=Name}, Drf)
    end;

get_rec(Node, _Drf) ->
    case ?Graph:class(Node) of
        form ->
            case ?Graph:path(Node, [recdef]) of
                [Rec] -> Rec;
                []    -> throw(not_found)
            end;
        record ->
            Node
    end.

add_recref(Ref, Spec, Drf) ->
    Rec = get_rec(Spec, Drf),
    ets:delete(Drf, Rec),
    case Ref of
        {ref, Expr} -> ok = ?Graph:mklink(Expr, recref, Rec);
        {def, Form} ->
            case ?Graph:index(Form, recdef, Rec) of
                none -> ok = ?Graph:mklink(Form, recdef, Rec);
                _    -> ok
            end
    end.

del_recref(Ref={Link, Node}, _Spec = {File, _Name}, Drf) ->
    case Link of
        ref -> Rec = ?Graph:path(Node, [recref]);
        def -> Rec = ?Graph:path(Node, [recdef])
    end,
    case Rec of
        [Record] ->
            ets:insert(Drf, {Record}),
            case Ref of
                {ref, Expr} -> ok = ?Graph:rmlink(Expr, recref, Record);
                {def, Form} -> ok = ?Graph:rmlink(Form, recdef, Record),
                               ok = ?Graph:rmlink(File, record, Record)
            end;
        _ -> ok
    end.

move_recrefs(Refs, From, To, Drf) ->
    lists:foreach(fun(R) -> move_recref(R, From, To, Drf) end, Refs).


move_recref(def, From, To, Drf) ->
%% move field
    FN = get_rec(From, Drf),
    Refs = ?Graph:path(FN, [field]),
    OldF = [{(?Graph:data(F))#field.name, F} || F <- Refs],
    [?Graph:rmlink(FN, field, F) || F <- Refs],
    ets:insert(Drf, {FN}),
    TN = get_rec(To, Drf),
    New = ?Graph:path(TN, [field]),
    NewF = [{(?Graph:data(F))#field.name, F} || F <- New],
    ExistN = [(?Graph:data(F))#field.name || F <- New],
    [?Graph:mklink(TN, field, F) || F <- Refs,
        not lists:member((?Graph:data(F))#field.name, ExistN)],
    [begin
         {_, FromField} = lists:keyfind(N, 1, OldF),
         {_, ToField}   = lists:keyfind(N, 1, NewF),
         move_fldrefs([def, ref], FromField, ToField, Drf)
     end || F <- Refs, lists:member(N=(?Graph:data(F))#field.name, ExistN)],
    ets:delete(Drf, TN),
%% move refdef
    move_refs(recdef, fun get_rec/2, From, To, Drf).

clean_record(Rec) ->
    clean_node(Rec, [{recdef, back}, {recref, back}])
        andalso
        begin
            case ?Graph:path(Rec, [{record, back}]) of
                [File] -> ?Graph:rmlink(File, record, Rec);
                _    -> ok
            end,
            ?Graph:delete(Rec)
        end.

%% -----------------------------------------------------------------------------
%% Record field operations

get_fld({{File, RecName}, Name}, Drf) when is_atom(Name) ->
    Rec = get_rec({File, RecName}, Drf),
    case ?Graph:path(Rec, [{field, {name, '==', Name}}]) of
        [Fld] -> Fld;
        []    ->
            create(Rec, field, #field{name=Name}, Drf)
    end;

get_fld(Node, _Drf) ->
    case ?Graph:class(Node) of
        typexp ->
            case ?Graph:path(Node, [fielddef]) of
                [Fld] -> Fld;
                []    -> throw(not_found)
            end;
        field ->
            Node
    end.

add_fldref(Ref, Spec, Drf) ->
    Fld = get_fld(Spec, Drf),
    ets:delete(Drf, Fld),
    case Ref of
        {ref, Expr} -> ok = ?Graph:mklink(Expr, fieldref, Fld);
        {def, TypExp} ->
            case ?Graph:index(TypExp, fielddef, Fld) of
                none -> ok = ?Graph:mklink(TypExp, fielddef, Fld);
                _    -> ok
            end
    end.

del_fldref(Ref={Link, Node}, _Spec, Drf) ->
    case Link of
        ref -> Fld = ?Graph:path(Node, [fieldref]);
        def -> Fld = ?Graph:path(Node, [fielddef])
    end,
    case Fld of
        [Field] ->
            ets:insert(Drf, {Field}),
            case Ref of
                {ref, Expr} -> ok = ?Graph:rmlink(Expr, fieldref, Field);
                {def, Form} -> ok = ?Graph:rmlink(Form, fielddef, Field)
            end;
        _ -> ok
    end.

move_fldrefs(Refs, From, To, Drf) ->
    lists:foreach(fun(R) -> move_fldref(R, From, To, Drf) end, Refs).


move_fldref(def, From, To, Drf) ->
    move_refs(fielddef, fun get_fld/2, From, To, Drf);
move_fldref(ref, From, To, Drf) ->
    move_refs(fieldref, fun get_fld/2, From, To, Drf).

clean_field(Fld) ->
    clean_node(Fld, [{fielddef, back}, {fieldref, back}])
        andalso
        begin
            case ?Graph:path(Fld, [{field, back}]) of
                [Rec] -> ?Graph:rmlink(Rec, field, Fld);
                _     -> ok
            end,
            ?Graph:delete(Fld)
        end.

%% -----------------------------------------------------------------------------
%% Lexical substitution operations

del_subref({Tag, Ref}, Subst, Drf) ->
    ?Graph:rmlink(Ref, Tag, Subst),
    Parents = lists:flatmap(fun(T) -> ?Graph:path(Subst, [{T, back}]) end,
                            [elex, flex, clex, llex]),
    if
        Parents == [] ->
            [del_subchild(Child, Drf) || {llex, Child} <- ?Graph:links(Subst)],
            ?Graph:delete(Subst);
        true ->
            ok
    end.

del_subchild(Node, Drf) ->
    [case ?Graph:data(Child) of
         #lex{type=Type} when Type == subst; Type == incl ->
             del_subref({llex, Node}, Child, Drf);
         _ ->
             del_subchild(Child, Drf)
     end || {llex, Child} <- ?Graph:links(Node)],
    ?Graph:delete(Node).


%% -----------------------------------------------------------------------------
%% Generic helper functions

create(Src, Tag, Data, Drf) ->
    Node = ?Graph:create(Data),
    %%io:format("create: ~p -~p-> ~p~n", [Src, Tag, Node]),
    ?Graph:mklink(Src, Tag, Node),
    ets:insert(Drf, {Node}),
    Node.

move_refs(P, Get, From, To, Drf) when is_function(Get) ->
    try {Get(From, Drf), Get(To, Drf)} of
        {FN, TN} -> do_move_refs(P, FN, TN, Drf)
    catch _:_ ->
            ok
    end.

do_move_refs(_, [], _, _) ->
    ok;
do_move_refs(_, _, [], _) ->
    ok;
do_move_refs(T, [FN | Tail1], [TN | Tail2], Drf)->
    do_move_refs(T, FN, TN, Drf),
    do_move_refs(T, Tail1, Tail2, Drf);
do_move_refs({Tag, back}, FN, TN, Drf) ->
    Refs = ?Graph:path(FN, [Tag]),
    [?Graph:rmlink(FN, Tag, Src) || Src <- Refs],
    ets:insert(Drf, {FN}),
    [?Graph:mklink(TN, Tag, Src) || Src <- Refs],
    ets:delete(Drf, TN),
    ok;
do_move_refs({Tag, Node}, FN, TN, Drf) ->
    ?Graph:rmlink(Node, Tag, FN),
    ets:insert(Drf, {FN}),
    ?Graph:mklink(Node, Tag, TN),
    ets:delete(Drf, TN),
    ok;
do_move_refs(Tag, FN, TN, Drf) ->
    Refs = ?Graph:path(FN, [{Tag, back}]),
    [?Graph:rmlink(Src, Tag, FN) || Src <- Refs],
    ets:insert(Drf, {FN}),
    [?Graph:mklink(Src, Tag, TN) || Src <- Refs],
    ets:delete(Drf, TN),
    ok.


clean_node(Node, Tags) ->
    lists:all(fun(Tag) -> [] == ?Graph:path(Node, [Tag]) end, Tags).
