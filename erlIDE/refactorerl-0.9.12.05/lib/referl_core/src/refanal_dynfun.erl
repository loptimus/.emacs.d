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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2007-2011,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Standalone static analyser for dynamic call constructs.
%%%
%%% @author Dániel Horpácsi <daniel_h@inf.elte.hu>

-module(refanal_dynfun).
-vsn("$Rev: 8309 $").

-export([analyse/0, clean/0]).

-include("core.hrl").
-include_lib("referl_lib/include/lib_export.hrl").

-define(CallAnal, refcore_callanal).

-define(timeout, 10000).
%-define(mnesia, true).

%% TODO: what if these special references are given not as tuples but,
%% for example, as case expressions?

%% TODO: "counting" is somewhat strange due to the special references...

%% TODO: analysis of apply/2 calls

%% TODO: matchspecs: ambrefs should not be considered

-record(match_spec, {function :: {atom(), atom()},
                     pattern  :: list(),
                     mapping  :: list(tuple(tuple(), tuple()))
                    }).

-type gnode(A) :: {atom(), A, integer()}.

%%  3: exactly 3
%% -1: unknown
%% -4: at least 3
-type dyn_arity() :: integer().

-record(funref, {expr  :: gnode(expr),
                 id    :: {?CallAnal:id(), ?CallAnal:id()},
                 arity :: dyn_arity() | [dyn_arity()]
                }).

-define(links, [{dynfuneref,back}, {dynfunlref,back},
                {ambfuneref,back}, {ambfunlref,back},
                {may_be, back},
                {dyncall, back}, {ambcall, back}]).

-define(out(S),    ?out(S, [])).
-define(out(S, A), io:format("~n" ++ S, A)).

%% =============================================================================
%% Cleaning

%% TODO how to delete modref links?
%% TODO how to delete funs created by dynamic analysis?
clean() ->
    [clean(Fun) || Fun <- ?Query:exec(?Query:seq([?Mod:all(), ?Mod:locals()]))],
    ?NodeSync:clean(),
    ?FunProp:reset().
clean(Fun) ->
    [clean(Fun, Link) || Link <- ?links].
clean(Fun, {Link,back}) ->
    [?Graph:rmlink(Expr, Link, Fun) || Expr <- ?Graph:path(Fun, [{Link,back}])].

%% =============================================================================
%% Analysis process

read_spec_refs() ->
    {ok, Terms} = file:consult("dynfunref.conf"),
    [#match_spec{function = MF, pattern = PS, mapping = MS} ||
        {MF, PS, MS} <- Terms, legal_patterns(PS)].

legal_patterns(PS) when is_list(PS) ->
    Res = lists:all(fun legal_pattern/1, PS),
    Res orelse io:format("Illegal function arguments pattern: ~p~n", [PS]),
    Res.
legal_pattern(P) when is_atom(P) -> legal_pattern(atom_to_list(P));
legal_pattern("_") -> true;
legal_pattern([$$, N]) -> N >= 48 andalso N =< 57;
legal_pattern(_) -> false.

analyse() ->
    analyse_local_funexprs(),

    SpecRefs =
        try read_spec_refs()
        catch
            _:_ -> []
        end,
    try
        anal_dynrefs(inspect_dynrefs(get_dynrefs(all_call_exprs(), SpecRefs)))
    catch
        no_calls ->
            ?out("No function calls found in the database.");
        no_dynamic ->
            ?out("No dynamic references found in the database.")
    end,
    ?out("~nAnalysis completed.~n").

%% Note that the former version returns all the application
%% expressions, while the latter one only gives those inside modules.
-ifdef(mnesia). %................................................. ifdef(mnesia)

all_call_exprs() -> all_exprs(fun mnesia_get_applications/0).

all_exprs(Selector) ->
    {atomic, Res} = mnesia:transaction(Selector),
    lists:map(fun(N) -> {'$gn', expr, N} end, Res).

mnesia_get_applications() ->
    Match = {expr, '$1', #expr{type=application, _='_'}, '_'},
    mnesia:select(expr, [{Match, [], ['$1']}]).

-else. %................................................................... else

all_call_exprs() -> all_exprs(fun(Expr) -> ?Expr:type(Expr) == application end).

all_exprs(Filter) ->
    AllExprs = ?Query:exec(?Query:seq([?Mod:all(),
                                       ?Mod:locals(),
                                       ?Fun:definition(),
                                       ?Form:clauses(),
                                       ?Clause:body(),
                                       ?Expr:deep_sub()])),
    lists:filter(Filter, AllExprs).

-endif. %................................................................. endif

get_dynrefs([], _) -> throw(no_calls);
get_dynrefs(AllCalls, SpecRefs) ->
    ?out("~p function calls found in the database.", [length(AllCalls)]),
    ?out("Looking for dynamic references..."),
    [{Ref, Expr} || Expr <- AllCalls,
                    Ref <- funrefs(Expr, SpecRefs),
                    Ref /= static].

numlength(0) -> 1;
numlength(I) -> integer_to_list(trunc(math:log(I)/math:log(10)) + 1).

inspect_dynrefs([]) -> throw(no_dynamic);
inspect_dynrefs(DynRefs) ->
    NumL = numlength(length(DynRefs)),
    ?out("~p dynamic function references found.", [length(DynRefs)]),
    ?out("Inspecting dynamic references...", []),
    S1 = "\rInspecting dynamic references... (~"++NumL++"w/~"++NumL++"w)",
    S2 = "~nIdentification of ~p dynamic references timed out.~n",
    Info1 = parallel(fun inspect_funref/2, DynRefs,
                     fun(X, Y) -> io:format(S1, [X, Y]) end,
                     fun(X) -> io:format(S2, [X]) end),
    Info = lists:flatten(Info1),
    ?out("~p dynamic references successfully analysed.", [length(Info)]),
    Info.

anal_dynrefs(Info) ->
    NumL = numlength(length(Info)),
    ?out("Storing dynamic references..."),
    lists:mapfoldl(
      fun(X, I) ->
              store(X),
              update_funprop(X#funref.expr),
              io:format("\rStoring dynamic references... (~"++NumL++"w/~"++NumL++"w)", [I, length(Info)]),
              {ok, I+1}
      end, 1, Info).

parallel(Fun, Xs, O1, O2) ->
    Self = self(),
    Pids = [spawn(fun() -> Fun(Self, X) end) || X <- Xs],
    receive_results(Pids, [], length(Pids), O1, O2).
receive_results([], Results, _C, _, _) ->
    Results;
receive_results(Pids, Results, C, O1, O2) ->
    receive {Pid, Rep} ->
            exit(Pid, normal),
            O1(C-length(Pids)+1, C),
            receive_results(Pids -- [Pid], Results ++ [Rep], C, O1, O2)
    after ?timeout ->
            [exit(Pid, kill) || Pid <- Pids],
            O2(length(Pids)),
            receive_results([], Results, C, O1, O2)
    end.

%% =============================================================================
%% Storing dyamic references into the database

%% TODO: eliminating the synchronisation?

store(#funref{expr = Expr, id = ID, arity = Arities}) when is_list(Arities) ->
    [store(#funref{expr = Expr, id = ID, arity = Arity}) || Arity <- Arities];

store(#funref{expr = Expr, id = ID, arity = Arity}) ->
    case ID of

        {local, {ModName, FunName}} ->
            ?NodeSync:add_ref(func, {dynlref, Expr}, {ModName, {FunName, Arity}});

        %% Unambiguous identifiers / possibly opaque arity

        {{_ModRef, ModName}, {_FunRef, FunName}} ->
            %%?NodeSync:add_ref(module, {ref, ModRef}, ModName),
            ?NodeSync:add_ref(func, {dyneref, Expr}, {ModName, {FunName, Arity}});

        %% Dynamic reference with opaque module name

        {undefined, {_FunRef, FunName}} ->
            ?NodeSync:add_ref(func, {dyneref, Expr}, {-1, {FunName, Arity}});

        %% Dynamic reference with opaque function name

        {{_ModRef, ModName}, undefined} ->
            %%?NodeSync:add_ref(module, {ref, ModRef}, ModName),
            ?NodeSync:add_ref(func, {dyneref, Expr}, {ModName, {-1, Arity}});

        %% Fully ambiguous references --- skipped!

        undefined              -> ok;
        {undefined, undefined} -> ok;

        %% Multiple references handled one by one (recursion!)

        {ModIDs, FunIDs} when is_list(ModIDs); is_list(FunIDs) ->
            [store(#funref{expr = Expr, id = {M, F}, arity = Arity}) ||
                M <- lists:flatten([ModIDs]), F <- lists:flatten([FunIDs])];
        IDs when is_list(IDs) ->
            [store(#funref{expr = Expr, id = I, arity = Arity}) || I <- IDs]
    end.

%% =============================================================================

funrefs(CallExpr, SpecRefs) ->
    case ?Graph:path(CallExpr, [funlref]) ++ ?Graph:path(CallExpr, [funeref]) of
        [] ->
            [FunId, _] = ?Query:exec(CallExpr, ?Expr:children()),
            case ?Graph:data(FunId) of
                %% TODO should we support deprecated tuple-syntax funs?
                #expr{type=infix_expr, value=':'} ->
                    [dynamic_mfa];
                #expr{} ->
                    [funexpr]
            end;
        [Fun] ->
            [Mod] = ?Graph:path(Fun, ?Fun:module()),
            case {?Graph:data(Mod), ?Graph:data(Fun)} of
                {#module{name=erlang}, #func{name=apply, arity=3}} ->
                    [static, apply_3];
                {#module{name=M}, #func{name=F, arity=A}} ->
                    [static] ++
                        [{spec_ref, Spec} || Spec = #match_spec{function = {MS, FS}, pattern = PS} <- SpecRefs,
                                             MS == M, FS == F, length(PS) == A];
                _ -> [static]
            end;
        _ -> [static]
    end.

lookup_funexpr_via_dataflow(N, ArgCnt) ->
    Ns = ?Dataflow:reach_1st([N], [back]),

    L = [N2 || N2 <- Ns, N2 /= N, ?Graph:class(N2) == expr,
               ?Expr:type(N2) == fun_expr orelse ?Expr:type(N2) == implicit_fun],

    LRef = lists:usort(lists:flatten([?Graph:path(N2, [funlref])         || N2 <- L])),
    ERef = lists:usort(lists:flatten([?Graph:path(N2, [funeref])         || N2 <- L])),
    LDef = try lists:usort(lists:flatten([?Graph:path(N2, [localfundef]) || N2 <- L]))
           catch error:{bad_path,expr,localfundef} -> [] end,

    funexpr_refs(ERef, external, ArgCnt) ++ funexpr_refs(LRef++LDef, local, ArgCnt).

funexpr_refs(Refs, Type, ArgCnt) ->
    case Refs of
        [] ->
            [undefined];
        Nodes when is_list(Nodes) ->
            [funexpr_ref(Node, Type, ArgCnt) || Node <- Nodes]
    end.

funexpr_ref(Node, Type, ArgCnt) ->
    case ArgCnt == ?Fun:arity(Node) of
        true ->
            [Mod] = ?Graph:path(Node, [{func, back}]),
            case Type of
                external ->
                    {{undefined, ?Mod:name(Mod)}, {undefined, ?Fun:name(Node)}};
                local ->
                    {local, {?Mod:name(Mod), ?Fun:name(Node)}}
            end;
        false ->
            undefined
    end.

inspect_funref(Parent, {funexpr, CallExpr}) ->
    [FunId, ArgLst] = ?Query:exec(CallExpr, ?Expr:children()),
    Args = [A || {esub, A} <- ?Syn:children(ArgLst)],
    Id = lookup_funexpr_via_dataflow(FunId, length(Args)),
    Ref = [#funref{expr = CallExpr, arity = length(Args), id = Id}],
    Parent ! {self(), Ref};

inspect_funref(Parent, {dynamic_mfa, CallExpr}) ->
    [FunId, ArgLst] = ?Query:exec(CallExpr, ?Expr:children()),
    Args = [A || {esub, A} <- ?Syn:children(ArgLst)],
    [Mod, Fun] = ?Query:exec(FunId, ?Expr:children()),
    Id = ?CallAnal:lookup_IDs(Mod, Fun, undefined),
    Ref = [#funref{expr = CallExpr, arity = length(Args), id = Id}],
    Parent ! {self(), Ref};

inspect_funref(Parent, {{spec_ref, #match_spec{pattern = Pat, mapping = Map}}, CallExpr}) ->
    [_, Args] = ?Query:exec(CallExpr, ?Expr:children()),
    try
        Match = lists:flatten(match_args(Args, Pat)),
        MFAs = [specialize_mapping(Match, Mapping) || Mapping <- Map],
        Parent ! {self(), [mfa_ref(Expr, M, F, A) || {Expr, {M, F, A}} <- MFAs]}
    catch
        error:function_clause ->
            %% zipwith f [] [a,b,c] crashes
            Parent ! {self(), []}
    end;

inspect_funref(Parent, {apply_3, CallExpr}) ->
    [_, ApplyArgs] = ?Query:exec(CallExpr, ?Expr:children()),
    [ApplyModRef, ApplyFunRef, ListOfArgs] = ?Query:exec(ApplyArgs, ?Expr:children()),
    Ref = mfa_ref(CallExpr, ApplyModRef, ApplyFunRef, ListOfArgs),
    Parent ! {self(), Ref}.

match_args(Exprs, Spec) when is_list(Exprs), is_list(Spec) -> lists:zipwith(fun match_args/2, Exprs, Spec);
match_args(_Exp, '_')                      -> [];
match_args(Expr, Tag)  when is_atom(Tag)   -> {Tag, Expr};
match_args(Expr, Spec) when is_list(Spec)  -> [{Spec, Expr} | match_args(?Query:exec(Expr, ?Expr:children()), Spec)];
match_args(Expr, Spec) when is_tuple(Spec) -> [{Spec, Expr} | match_args(?Query:exec(Expr, ?Expr:children()), tuple_to_list(Spec))].

specialize_mapping(Match, {T, {M, F, A}}) ->
    Replace = fun(Key) -> proplists:get_value(Key, Match, Key) end,
    {Replace(T), {Replace(M), Replace(F), Replace(A)}}.

mfa_ref(Expr, ModRef, FunRef, ListOfArgs) ->
    ID = {lookup_ref(ModRef), lookup_ref(FunRef)},
    Arity = lookup_arity(ListOfArgs),
    [#funref{expr = Expr, arity = Arity, id = ID}].

lookup_ref(Name) when is_atom(Name) -> [{undefined_node, Name}];
lookup_ref(Expr) -> ?CallAnal:lookup_ID(Expr, undefined).
lookup_arity(Arity) when is_integer(Arity) -> [Arity];
lookup_arity(Expr) ->
    case ?CallAnal:listcons_length(Expr) of
        incalculable -> -1;
        Is -> [-1*I-1 || {I} <- Is] ++ [I || I <- Is, is_integer(I)]
    end.

%% =============================================================================
%% Copied from refanal_fun and slightly modified

update_funprop(Node) ->
    case ?Graph:data(Node) of
        #form{type=func} -> update_form_funprop(Node);
        #form{} -> ok;
        _D ->
            case ?Syn:parent(Node) of
                [{_,P}] -> update_funprop(P);
                _ -> ok
            end
    end.

update_form_funprop(Form) ->
    %% TODO: maybe ?NodeSync:get_node support
    case ?Graph:path(Form, [fundef]) of
        [Fun] -> ?FunProp:update(Fun, funprops(Form));
        []    -> ok
    end.

funprops(Form) ->
    Exprs = ?Query:exec(Form, ?Query:seq([?Form:clauses(), ?Clause:body(), ?Expr:deep_sub()])),
    lists:foldl(fun node_funprop/2, node_funprop(Form, {true, []}), Exprs).

node_funprop(Node, {Pure, Calls}) ->
    node_funprop(Node, ?Graph:data(Node), {Pure, Calls}).

node_funprop(Node, #expr{type=Type}, {Pure, Calls}) ->
    {Pure andalso Type /= send_expr andalso Type /= receive_expr,
     if
         Type == application -> add_calls(Node, Calls);
         true -> Calls
     end};

node_funprop(_, _, Props) -> Props.

add_calls(Node, Calls) ->
    FunCalls = [{funcall, F} || {T, F} <- ?Graph:links(Node), lists:member(T, [funeref, funlref])],
    DynCalls = [{dyncall, F} || {T, F} <- ?Graph:links(Node), lists:member(T, [dynfuneref, dynfunlref])],
    AmbCalls = [{ambcall, F} || {T, F1} <- ?Graph:links(Node), lists:member(T, [ambfuneref, ambfunlref]),
                                F <- ?Graph:path(F1, [may_be])],
    ordsets:union(Calls, lists:usort(FunCalls++DynCalls++AmbCalls)).

%% =============================================================================
%% Analysis of local fun definitions (fun (...) -> ...; (...) -> ... end)

-ifdef(anal_fun_exprs).
analyse_local_funexprs() ->
    [fun_exprs_of_fun(Mod, Fun) || Mod <- ?Query:exec(?Mod:all()),
                                   Fun <- ?Query:exec(Mod, ?Mod:locals())].
-else.
analyse_local_funexprs() -> ok.
-endif.

fun_exprs_of_fun(Mod, Fun) ->
    {ParentName, ParentArity} = {?Fun:name(Fun), ?Fun:arity(Fun)},

    Exprs = ?Query:exec(Fun, ?Query:seq([?Fun:definition(), ?Form:clauses(), ?Clause:body()])),
    FunExprs = lists:flatten([fun_exprs(Expr) || Expr <- Exprs]),

    [begin
         {Name, Arity} = fun_expr_name_arity(Expr, ParentName, ParentArity, N),
         ?NodeSync:add_ref(func, {localdef, Expr}, {Mod, {Name, Arity}})
     end || {N, Expr} <- lists:zip(lists:seq(0,length(FunExprs)-1), FunExprs)].

fun_expr_name_arity(Expr, ParentName, ParentArity, N) ->
    S = io_lib:format("-~p/~p-fun-~p-", [ParentName, ParentArity, N]),
    Name = list_to_atom(lists:flatten(S)),
    [Clause | _] = ?Query:exec(Expr, [exprcl]),
    Arity = length(?Query:exec(Clause, [pattern])),
    {Name, Arity}.

fun_exprs(Node = {'$gn', Class, _}) when Class == expr; Class == clause ->
    Children = [N || {_, N} <- ?Syn:children(Node)],
    [fun_exprs(Child) || Child <- Children]
        ++ [Node || ?Graph:class(Node) == expr, ?Expr:type(Node) == fun_expr];
fun_exprs(_) -> [].
