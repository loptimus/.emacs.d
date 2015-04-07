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

%%% @author Jimmy <>
%%%
%%% @doc Erlang GS user interface utility functions for referl_gs.
%%%

-module(referl_gs_sup).

-svn("$Rev$ ").

%% Exports
-export([root/0, children/1, info/1, text/1, find_node/1,
         tr_buttons/1, tool_buttons/0]).

%% Includes
-include("ui.hrl").
-include("gs.hrl").
-include_lib("referl_core/include/core_export.hrl").

%% Node index
-define(Index(X), element(3, X)). % TODO: find a function!!!

%% Missing queries
-define(QFile, [file]).
-define(RecDef, [recdef]).
-define(RecDefBack, [{recdef, back}]).
-define(Field, [field]).
-define(FieldDef, [{fielddef, back}]).
-define(RecAttr, [attr]).
-define(VarBinds, [varbind]).
-define(VarRefs, [varref]).

%% Macros
-define(ia(IAA), list_to_atom(integer_to_list(IAA))).

%% The `undefined' problem
-ifndef(show_undefined).
-define(undefined(UD),
        case UD of
           undefined -> undefined;
           UDElse -> ?MISC:to_list(UDElse)
        end).
-else.
-define(undefined(UD), ?MIDC:to_list(UD)).
-endif.

%% Implementation

%% @doc Returns width a tag record, that represents the root node of the graph.
root() ->
    #tag{class=root, node=?Graph:root()}.

%% @doc Returns width the children of a given node.
children(Tag) ->
    children(?Syn:class(Tag#tag.node), Tag#tag.node).

children(root, _) ->
    [ gather_file(X) || X <- ?Query:exec(?QFile)];
children(module, Module) ->
    Forms = ?Query:exec(Module, ?Query:seq([?Mod:file(), ?File:forms()])),
    [ gather_form(X) || X <- Forms ,
                        lists:member(?Form:type(X), [macro, record, func]) ];
children(func, Node) ->
    case ?Syn:class(Node) of
        func ->
            Clauses = 
                ?Query:exec(Node, 
                    ?Query:seq([?Fun:definition(), ?Form:clauses()])),
            [ #tag{class=clause,
                   type=?Clause:var(Clause),
                   kind=?Clause:type(Clause),
                   name=integer_to_list(X),
                   id=integer_to_list(?Index(Clause)),
                   node=Clause} ||
                {Clause, X} <- lists:zip(Clauses,
                                         ?MISC:seq2(1, length(Clauses)))];
        _ -> []
    end;
children(clause, Clause) ->
    Patterns = ?Query:exec(Clause, ?Clause:patterns()),
    Guard = ?Query:exec(Clause, ?Clause:guard()),
    Bodys = ?Query:exec(Clause, ?Clause:body()),
    Nodes = lists:flatten([Patterns, Guard, Bodys]),
    [#tag{class=expr,
          type=?Expr:role(X),
          kind=?Expr:type(X),
          id=?Index(X),
          name=?undefined(?Expr:value(X)),
          node=X} || X <- Nodes ];
children(expr, Expr) ->
    VarBinds = ?Query:exec(Expr, ?VarBinds),
    VarRefs = ?Query:exec(Expr, ?VarRefs),
    Children = ?Query:exec(Expr, ?Expr:children()),
    Clauses = ?Query:exec(Expr, ?Expr:clauses()),
    VarTags = [ #tag{class=variable,
                     id=integer_to_list(?Index(X)),
                     name=?Var:name(X),
                     node=X} || X <- VarBinds ++ VarRefs ],
    ExprTags = [ #tag{class=expr,
                      type=?Expr:role(X),
                      kind=?Expr:type(X),
                      id=integer_to_list(?Index(X)),
                      name=?undefined(?Expr:value(X)),
                      node=X} || X <- Children ],
    ClauseTags = [ #tag{class=clause,
                        type=?Clause:var(Clause),
                        kind=?Clause:type(Clause),
                        id=integer_to_list(?Index(Clause)),
                        node=Clause} || Clause <- Clauses ],
    lists:flatten([VarTags, ExprTags, ClauseTags]);
children(record, Rec) ->
    Attributes = 
        ?Query:exec(Rec, ?Query:seq([?Rec:fields(), ?RecField:references()])),
    [ #tag{class=?Syn:class(Node), name=?undefined(?Expr:value(Node)),
           type=?Expr:role(Node), kind=?Expr:type(Node),
           id=integer_to_list(?Index(Node)), node=Node} ||
        Node <- Attributes ];
children(_, _) ->
    [].

gather_file(File) ->
    Type = ?File:type(File),
    case Type of
        module ->
            [Node] = ?Query:exec(File, ?File:module()),
            Name = ?MISC:to_list(?Mod:name(Node));
        header ->
            Node = File,
            Name = filename:basename(?File:path(File))
    end,
    #tag{class=Type, id=integer_to_list(?Index(Node)),
         name=Name, node=Node}.

gather_form(Form) ->
    Type = ?Form:type(Form),
    case Type of
        macro ->
            Node = Form,
            Name = ?Macro:name(Node);
        record ->
            [Node] = ?Query:exec(Form, ?RecDef),
            Name = ?MISC:to_list(?Rec:name(Node));
        func ->
            [Node] = ?Query:exec(Form, ?Form:func()),
            Name =
                ?MISC:to_list(?Fun:name(Node)) ++ "/" ++
                integer_to_list(?Fun:arity(Node))
    end,
    #tag{class=Type, id=integer_to_list(?Index(Node)),
         name=Name, node=Node}.

%% @doc 
find_node(History) ->
    find_node(lists:reverse(History), []).

find_node([], [A | B]) -> {A, B};
find_node([Tag | Tail], NewHistory) ->
    try ?Graph:data(Tag#tag.node) of
        _ -> find_node(Tail, [Tag | NewHistory])
    catch
        error:bad_node ->
            [A | B] = NewHistory,
            {A , B}
    end.

%% @doc Returns width a list, that contains information about a node.
%% This informations will be appear in the information text field.
info(Tag) ->
    info(Tag#tag.class, Tag#tag.node).

info(root, _) ->
    Files = ?Query:exec(?QFile),
    FileNum = integer_to_list(length(Files)),
    [{files, {string, FileNum}}];
info(module, Mod) ->
    ModName = atom_to_list(?Mod:name(Mod)),
    [File] = ?Query:exec(Mod, ?Mod:file()),
    FileName = ?File:path(File),
    Locals = ?Query:exec(Mod, ?Mod:locals()),
    LocalNames = [ atom_to_list(X) ++ "/" ++ integer_to_list(Y) ||
                      {X, Y} <- [ {?Fun:name(F), ?Fun:arity(F)} ||
                                    F <- Locals ] ],
    ExportedNames = [ atom_to_list(X) ++ "/" ++ integer_to_list(Y) ||
                        {X, Y} <- [ {?Fun:name(F), ?Fun:arity(F)} ||
                                      F <- Locals,
                                      ?Fun:is_exported(F) ] ],
    Macros = ?Query:exec(File, ?File:macros()),
    MacroNames = [ ?Macro:name(X) || X <- Macros],
    Records = ?Query:exec(File, ?File:records()),
    RecordNames = [ atom_to_list(?Rec:name(X)) || X <- Records],
    [{name, {string, ModName}},
     {file, {string, FileName}},
     {functions, {list, LocalNames}},
     {exported, {list, ExportedNames}},
     {macros, {list, MacroNames}},
     {records, {list, RecordNames}}];
info(macro, Macro) ->
    Name = ?Macro:name(Macro),
    [{name, {string, Name}}];
info(record, Rec) ->
    Name = atom_to_list(?Rec:name(Rec)),
    Fields = ?Query:exec(Rec, ?Field),
    FieldsNum = integer_to_list(length(Fields)),
    FieldNames =
        [atom_to_list(?RecField:name(Field)) || Field<-Fields],
    [{name, {string, Name}},
     {fieldnum, {string, FieldsNum}},
     {fieldnames, {list, FieldNames}}];
info(func, Fun) ->
    Name = atom_to_list(?Fun:name(Fun)),
    Arity = integer_to_list(?Fun:arity(Fun)),
    Exported = atom_to_list(?Fun:is_exported(Fun)),
    FunDef = ?Query:exec(Fun, ?Fun:definition()),
    Clauses = ?Query:exec(FunDef, ?Form:clauses()),
    ClauseNum = integer_to_list(length(Clauses)),
    [{name, {string, Name}},
     {arity, {string, Arity}},
     {exported, {string, Exported}},
     {clausenum, {string, ClauseNum}}];
info(clause, Clause) ->
    HasGuard = atom_to_list([] /= ?Query:exec(Clause, ?Clause:guard())),
    LineNum = integer_to_list(length(?Query:exec(Clause, ?Clause:body()))),
    [{hasguard, {string, HasGuard}},
     {linenum, {string, LineNum}}];
info(variable, Var) ->
    VarName = ?Var:name(Var),
    Bindings = ?Query:exec(Var, ?Var:bindings()),
    BindingNum = integer_to_list(length(Bindings)),
    Refs = ?Query:exec(Var, ?Var:references()),
    RefNum = integer_to_list(length(Refs)),
    [{name, {string, VarName}},
     {varbindingnum, {string, BindingNum}},
     {varrefnum, {string, RefNum}}];
info(_, _) ->
    [].

%% @doc Returns width the source code belonging to the given node.
%% This will be appear in the source code text field.
text(Tag) ->
    text(Tag#tag.class, Tag#tag.node).

text(root, _) -> [];
text(variable, _) -> [];
text(module, Mod) ->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    ?Syn:tree_text(File);
text(func, Fun) ->
    [Form] = ?Query:exec(Fun, ?Fun:definition()),
    ?Syn:tree_text(Form);
text(record, Rec) ->
    [Form] = ?Query:exec(Rec, ?Rec:form()),
    ?Syn:tree_text(Form);
text(_, Node) ->
    ?Syn:tree_text(Node).

%% @doc Returns width a list of tr records representing the tool 
%% buttons, that are always available from the interface.
tool_buttons() ->
    Undo =
        fun(_) ->
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{undo,[]}),
            ReqID
        end,
    Refresh =
        fun(_) ->
            reload
        end,
    Add = 
        fun(Args) ->
            FilePath = proplists:get_value(filename, Args),
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{add_dir, FilePath}),
            ReqID
        end,
    Query =
        fun(Args) ->
            StartNode = proplists:get_value(root, Args),
            QueryStr = proplists:get_value(querystr, Args),
            FullQueryStr = 
                case StartNode#tag.class of
                    module -> "@mod";
                    func -> "@fun";
                    variable -> "@var";
                    record -> "@rec";
                    macro -> "@macro";
                    expr -> "@expr";
                    _ -> "mods"
                end ++ 
                case QueryStr of
                    [] -> [];
                    _ -> "."++QueryStr
                end,
            StartOpt = 
                case StartNode#tag.class of
                    root -> [];
                    clause -> [];
                    _ -> 
                        [{node_list, [StartNode#tag.node]}]
                end,
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{transform, semantic_query, 
                               [{display_opt,[{positions,linecol}]},
                                {start_opt,StartOpt},
                                {querystr,FullQueryStr}]}),
            ReqID
        end,
    [#tr{label="Add module",
         desc="Add module:",
         back=false,
         func=Add,
         map=true,
         image="add.jpg",
         prop=[{entry, filename, "Filepath: "}]},
    #tr{label="Undo",
        back=undo,
        func=Undo,
        kind=undo,
        image="undo.jpg"},
    #tr{label="Refresh",
        back=false,
        func=Refresh,
        map=false,
        image="refresh.jpg"},
    #tr{label="Run query",
        desc="Run query",
        back=false,
        func=Query,
        map=true,
        prop=[{entry, querystr, "Query: "}],
        image="query.jpg"}].

%% @doc Returns width a list of tr records representing optional 
%% buttons, that are available in different nodes depending on its 
%% class and node types and some other properties.
tr_buttons(#tag{class=variable, node=Var}) ->
    [Token] = ?Query:exec(Var, {seq, ?Var:bindings(), [elex]}),
    [File] = ?Query:exec(Token, ?Token:file()),
    FileName = ?File:path(File),
    {Pos, _} = ?Token:pos(Token),
    Rename =
        fun(Args) ->
            Name = proplists:get_value(varname, Args),
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{transform, rename_var,
                               [{ask_missing, false}, {file, FileName}, 
                                {position, Pos}, {varname, Name}]}),
            ReqID
        end,
    Eliminate =
        fun(_) ->
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{transform, elim_var,
                               [{ask_missing, false}, {file, FileName}, 
                                {position, Pos}]}),
            ReqID
        end,
    [#tr{label="Rename",
         map=true,
         desc="Rename Variable",
         prop=[{entry, varname, "New name: "}],
         back=true,
         func=Rename,
         image="rename.jpg"},
     #tr{label="Eliminate",
         back=true,
         func=Eliminate,
         image="eliminate.jpg"}];
tr_buttons(#tag{class=func, node=Fun}) ->
    [Mod] = ?Query:exec(Fun, ?Fun:module()),
    ModName = ?Mod:name(Mod),
    FileName = ?File:path(hd(?Query:exec(Mod, ?Mod:file()))),
    FunName = ?Fun:name(Fun),
    Arity = ?Fun:arity(Fun),
    FirstClause = 
        hd(?Query:exec(Fun, {seq, ?Fun:definition(), ?Form:clauses()})),
    Patterns = ?Query:exec(FirstClause, ?Clause:patterns()),
    Rename =
        fun(Args) ->
            Name = proplists:get_value(name, Args),
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{transform, rename_fun,
                               [{ask_missing, false}, {module, ModName}, 
                                {function, FunName}, {arity, Arity}, 
                                {name, Name}]}),
            ReqID
        end,
    Move =
        fun(Args) ->
            Name = proplists:get_value(name, Args),
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{transform, move_fun,
                               [{ask_missing, false}, {file, FileName}, 
                                {funlist, [{FunName, Arity}]}, {name, Name}]}),
            ReqID
        end,
    Reorder =
        fun(Args) ->
            Order = [ case proplists:get_value(?ia(X), Args) of
                          [] -> 0;
                              N -> list_to_integer(N)
                          end || X <- ?MISC:seq2(1, Arity) ],
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{transform, reorder_funpar,
                               [{ask_missing, false}, {module, ModName}, 
                                {function, FunName}, {arity, Arity}, 
                                {order, Order}]}),
            ReqID
        end,
    Tuple =
        fun(_) ->
            P1 = hd(Patterns),
            Pn = lists:last(Patterns),
            {First, _} = ?Token:pos(hd(?Query:exec(P1, [elex]))),
            {_, Last} = ?Token:pos(hd(?Query:exec(Pn, [elex]))),
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{transform, tuple_funpar,
                               [{ask_missing, false}, {file, FileName}, 
                                {posrange, {First, Last}}]}),
            ReqID
        end,
    [#tr{label="Rename",
         map=true,
         desc="Rename Function",
         prop=[{entry, name, "New name: "}],
         back=true,
         func=Rename,
         image="rename.jpg"},
     #tr{label="Move",
         map=true,
         desc="Move Function",
         prop=[{entry, name, "Module: "}],
         back=true,
         func=Move,
         image="move.jpg"}] ++
        if
            length(Patterns) < 2 -> [];
            true ->
                [#tr{label="Reorder",
                     map=true,
                     desc="Reorder Function Parameters",
                     prop=
                        [ {entry, ?ia(X), "Argument " 
                            ++ integer_to_list(X) 
                            ++ "'s new pos.: "} 
                        || X <- ?MISC:seq2(1, Arity)],
                     func=Reorder,
                     image="reorder.jpg"}] 
        end ++
        case length(Patterns) of
            0 -> [];
            _ ->
                [#tr{label="Tuple",
                     back=true,
                     func=Tuple,
                     image="tuple.jpg"}]
        end;
tr_buttons(#tag{class=macro, node=Macro}) ->
    File = ?File:path(hd(?Query:exec(Macro, ?Macro:file()))),
    MacName = ?Macro:name(Macro),
    Rename =
        fun(Args) ->
            Name = proplists:get_value(name, Args),
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{transform, rename_mac,
                               [{ask_missing, false}, {file, File}, 
                                {macro, MacName}, {macname, Name}]}),
            ReqID	
        end,
    [#tr{label="Rename",
         map=true,
         desc="Rename macro",
         prop=[{entry, name, "New name: "}],
         back=true,
         func=Rename,
         image="rename.jpg"}];
tr_buttons(#tag{class=module, node=Mod}) ->
    FileName = ?File:path(hd(?Query:exec(Mod, ?Mod:file()))),
	ModName = ?Mod:name(Mod),
    Rename =
        fun(Args) ->
            Name = proplists:get_value(name, Args),
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{transform, rename_mod,
                               [{ask_missing, false}, {file, FileName}, 
                                {module, ModName}, {name, Name}]}),
            ReqID
        end,
    Drop =
        fun(_Args) -> 
            ReqID = ?UI:getid(),
            ?UI:request(ReqID, {drop, FileName}),
            ReqID
        end,
    [#tr{label="Rename",
         map=true,
         desc="Rename Module",
         prop=[{entry, name, "New name: "}],
         back=true,
         func=Rename,
         image="rename.jpg"},
     #tr{label="Drop",
         map=false,
         back=true,
         func=Drop,
         image="remove.jpg"}];
tr_buttons(#tag{class=expr, kind=implicit_fun, node=Expr}) ->
    [File] = ?Query:exec(Expr,
                       ?Query:seq([
                          ?Query:seq([
                             ?Expr:clause(),
                             ?Clause:form()]),
                          ?Form:file()])),
    FileName = ?File:path(File),
    Token = hd(?Query:exec(Expr, [elex])),
    {Pos, _} = ?Token:pos(Token),
    Expand =
        fun(_) ->
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{transform, expand_funexpr,
                               [{ask_missing, false}, {file, FileName}, 
                                {position, Pos}]}),
            ReqID
        end,
    [#tr{label="Expand",
         func=Expand,
         image="expand.jpg"}];
tr_buttons(#tag{class=expr, kind=application, node=Expr}) ->
    [File] = ?Query:exec(Expr,
                       ?Query:seq([
                          ?Query:seq([
                             ?Expr:clause(),
                             ?Clause:form()]),
                          ?Form:file()])),
    FileName = ?File:path(File),
    Token = hd(?Query:exec(Expr, {seq, ?Expr:children(), [elex]})),
    {Pos, _} = ?Token:pos(Token),
    Inline =
        fun(_) ->
            ReqID = ?UI:getid(),
            ?UI:request(ReqID,{transform, inline_fun,
                               [{ask_missing, false}, {file, FileName}, 
                                {position, Pos}]}),
            ReqID
        end,
    [#tr{label="Inline",
         back=true,
         func=Inline,
         image="inline.jpg"}];
tr_buttons(_) ->
    [].
