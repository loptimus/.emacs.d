-module(refcore_gen_syn).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 135).

-export([file/2]).

-include_lib("referl_gen/include/gen_export.hrl").

-record(cattr, {name, value}).
-record(tattr, {name, token, index=1}).
-type attrib() :: #cattr{} | #tattr{}.

-record(term,     {token :: atom()}).
-record(nonterm,  {name :: atom(), link :: atom()}).
-record(repeat,   {rule :: [_]}).
-record(optional, {rule :: [_]}).
-type rule_elem() :: #term{} | #nonterm{} | #repeat{} | #optional{}.

-record(rule, {lhs   :: atom(),
               class :: atom(),
               attrs :: [attrib()],
               rhs   :: [rule_elem()]}).
-record(chain, {ref  :: atom()}).

-record(symrules, {lhs   :: atom(),
                   rules :: [rule_elem()]}).

-record(keywset, {name :: atom(),
                  keywords :: [{string(), atom()} | atom()]}).

value({Type,  _Line})        -> Type;
value({_Type, _Line, Value}) -> Value.

-record(vattr, {name, index, eindex}).
-record(struct, {data :: {atom(), [#cattr{}]},
                 head, structure, attribs}).

file(File, Prefix) ->
    {ok, Text} = file:read_file(File),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Text)),
    {ok, {Params, Syntax, Lexicals, Keywords, KwTrans}} = parse(Tokens),

    ets:new(rules,     [bag, named_table]),
    ets:new(terminals, [set, named_table]),
    ets:new(structure, [set, named_table, {keypos, #struct.data}]),
    ets:new(synschema, [bag, named_table]),

    try
        syntax(Params, Syntax),
        normalize_schema(),

        append_file(
          Prefix ++ "scanner.erl",
          fun(Dev) ->
                  print_scanner(Dev, Params, Lexicals, Keywords, KwTrans)
          end),
        prepend_file(
          Prefix ++ "parser.yrl",
          fun (Dev) -> print_parser(Dev, Params) end),
        append_file(
          Prefix ++ "nodes.erl",
          fun (Dev) -> print_nodes(Dev, Params) end),
        save_file(
          Prefix ++ "schema.hrl",
          fun (Dev) -> print_schema(Dev) end)

    after
        ets:delete(rules),
        ets:delete(terminals),
        ets:delete(structure),
        ets:delete(synschema)
    end.

save_file(Name, Printer) ->
    write_file(Name, "", Printer, "").

append_file(Name, Printer) ->
    write_file(Name, src_text(Name), Printer, "").

prepend_file(Name, Printer) ->
    write_file(Name, "", Printer, src_text(Name)).

write_file(Name, Head, Printer, Tail) ->
    case file:open(Name, [write]) of
        {ok, Dev} ->
            ok = file:write(Dev, Head),
            Printer(Dev),
            ok = file:write(Dev, Tail),
            ok = file:close(Dev);
        {error, Reason} ->
            io:format("~s: ~s~n", [Name, file:format_error(Reason)]),
            throw(Reason)
    end.

src_text(Name) ->
    case file:read_file(Name ++ "Src") of
        {ok, Text} -> Text;
        {error, Reason} ->
            io:format("~s: ~s~n", [Name++"Src",
                                   file:format_error(Reason)]),
            throw(Reason)
    end.


%%% ============================================================================
%%% Syntax analysis (result: parser rules, node structure)

syntax(Params, Syntax) ->
    {'LexLinks', LexLinks} = proplists:lookup('LexLinks', Params),
    lists:foreach(fun (SR) -> sym_rules(SR, LexLinks) end, Syntax).

sym_rules(#symrules{lhs=LHS, rules=Rules}, LL) ->
    lists:foreach(fun (R) -> sym_rule(LHS, R, LL) end, Rules).


sym_rule(LHS, #chain{ref=RHS}, _) ->
    ets:insert(
      rules,
      {LHS, atom_to_list(RHS), {atom, field(1)}}),

    ets:insert(synschema, {LHS, {ref, RHS}});

sym_rule(LHS, #rule{class=Class, attrs=Attrs, rhs=RHS}, LexLinks) ->
    {Class, Lex} = proplists:lookup(Class, LexLinks),
    DN = atom_to_list(LHS) ++ attr_dn(Attrs),
    CA = [A || A=#cattr{} <- Attrs],
    VA = [A || A=#tattr{} <- Attrs],
    try
        {Struct, Rules, Aux, Values} = proc_rule(RHS, DN, VA, 1, 1),
        validate_struct(Struct),

        ets:insert(
          rules,
          [{LHS, rule_text(Rule), builder(Class, Lex, CA++Values, Rule)} ||
              Rule <- Rules]),

        ets:insert(
          rules,
          [{Sym, rule_text(Rule), children(Rule, Lex)} || {Sym, Rule} <- Aux]),

        ets:insert(terminals, terminals(Struct)),

        ets:insert(
          synschema,
          [{LHS, {class, Class}} |
           lists:flatten([schema(Class, Elem, Aux)
                          || Rule <- Rules, Elem <- Rule])]),

        Str = #struct{data      = {Class, CA},
                      head      = data(Class, CA),
                      structure = {abstract, Struct},
                      attribs   = attr_value(Class, Lex, Values)},

        ets:insert_new(structure, Str) orelse
           ets:lookup(structure, Str#struct.data) =:= [Str] orelse
              throw({ambiguous_struct, Class, CA})
    catch
        throw:Err ->
            throw({DN, Err})
    end.

data(Class, Attrs) ->
    {record_expr,
     {atom, Class},
     [{record_field,
       {atom, Name},
       {abstract, Value}} || #cattr{name=Name, value=Value} <- Attrs] ++
     [{record_field,
       {atom, Name},
       {application, {atom, tv}, [{atom, field(EInd)}]}} ||
         #vattr{name=Name, eindex=EInd} <- Attrs]
    }.

builder(Class, Lex, Attrs, Rule) ->
    {application,
     {atom, build},
     [data(Class, Attrs), children(Rule, Lex)]
    }.

children(Rule, Lex) ->
    {list, [childref(Child, Lex) || Child <- Rule]}.

childref({Ind, #term{}}, Lex) ->
    {tuple, [{atom, Lex}, {application, {atom, tn}, [{atom, field(Ind)}]}]};
childref({Ind, #nonterm{link=Link}}, _) ->
    {tuple, [{atom, Link}, {atom, field(Ind)}]};
childref({Ind, _}, _) ->
    {atom, field(Ind)}.

attr_value(Class, Lex, Values) ->
    {list,
     [{tuple, [{tuple, [{atom, Lex}, {integer, Index}]},
               {record_index_expr, {atom, Class}, {atom, Name}}]} ||
         #vattr{name=Name, index=Index} <- Values]}.

rule_text([]) ->
    "'$empty'";
rule_text(Rule) ->
    string:join([name(Elem) || Elem <- Rule], " ").

name({_, #term{token=Token}}) -> io_lib:write(Token);
name({_, #nonterm{name=Sym}}) -> atom_to_list(Sym);
name({_, Name}) when is_atom(Name) -> atom_to_list(Name).

field(Ind) ->
    "$"++integer_to_list(Ind).

attr_dn(Attrs) ->
    lists:flatten([["_", atom_to_list(Val)] || #cattr{value=Val} <- Attrs]).

terminals([{token, T} | Tail]) -> [{T} | terminals(Tail)];
terminals([{optional, Opt}|Tail]) -> terminals(Opt)++terminals(Tail);
terminals([{repeat, Sep, _}|Tail]) -> [{Sep}|terminals(Tail)];
terminals([_|Tail]) -> terminals(Tail);
terminals([]) -> [].

schema(_,   {_, #term{}}, _) -> [];
schema(Cls, {_, #nonterm{link=Lnk, name=Sym}}, _) -> {{Cls, Lnk}, Sym};
schema(Cls, {_, Ref}, Aux) ->
    {value, {Ref, Rule}, Aux1} = lists:keytake(Ref, 1, Aux),
    [schema(Cls, E, Aux1) || E <- Rule].


-type index() :: pos_integer().
-type flat_rule() :: [{index(), #term{} | #nonterm{} | atom()}].
-spec proc_rule([rule_elem()], string(), [#tattr{}], index(), index()) ->
    {[tuple()], [flat_rule()], [{atom(), flat_rule()}], [#vattr{}]}.
proc_rule([#optional{rule=Opt} | Tail],
          DN, VA, ElemInd, LexInd) ->
    {OptStruct, _, OptAux, OptValues} =
        proc_rule(Opt, DN, VA, ElemInd, LexInd),
    OptValues == [] orelse throw(opt_values),

    {TailStruct, TailRules, TailAux, TailValues} =
        proc_rule(Tail, DN, VA, ElemInd, LexInd),

    {_, OptRules, _, _} =
        proc_rule(Opt ++ Tail, DN, VA, ElemInd, LexInd),

    {[{optional, OptStruct} | TailStruct],
     OptRules ++ TailRules,
     OptAux ++ TailAux, TailValues};

proc_rule([#nonterm{name=Sym, link=Link},
           #repeat{rule=[#term{token=Sep},
                         #nonterm{name=Sym, link=Link}]} | Tail],
          DN, VA, ElemInd, LexInd) ->
    {Struct, Rules, Aux, Values} = proc_rule(Tail, DN, VA, ElemInd+1, LexInd),
    AuxName = list_to_atom(DN ++ "@rep_" ++ atom_to_list(Link)),
    {[{repeat, Sep, Link} | Struct],
     [[{ElemInd, AuxName} | Rule] || Rule <- Rules],
     [{AuxName, [{1, AuxName},
                 {2, #term{token=Sep}},
                 {3, #nonterm{name=Sym, link=Link}}]},
      {AuxName, [{1, #nonterm{name=Sym, link=Link}}]} | Aux],
     Values};

proc_rule([#nonterm{link=Link} = Elem | Tail],
          DN, VA, ElemInd, LexInd) ->
    {Struct, Rules, Aux, Values} = proc_rule(Tail, DN, VA, ElemInd+1, LexInd),
    {[{symbol, Link} | Struct],
     [[{ElemInd, Elem} | Rule] || Rule <- Rules],
     Aux, Values};

proc_rule([#term{token=Token} = Elem | Tail],
          DN, VA, ElemInd, LexInd) ->
    {NewVal, NextVA} =
        case lists:keyfind(Token, #tattr.token, VA) of
            #tattr{index=1, name=Name} = F ->
                {[#vattr{name=Name, index=LexInd, eindex=ElemInd}], VA -- [F]};
            #tattr{index=N} = F ->
                {[], [F#tattr{index=N-1} | VA -- [F]]};
            _ ->
                {[], VA}
        end,
    {Struct, Rules, Aux, Values} =
        proc_rule(Tail, DN, NextVA, ElemInd+1, LexInd+1),
    {[{token, Token} | Struct],
     [[{ElemInd, Elem} | Rule] || Rule <- Rules],
     Aux,
     NewVal ++ Values};

proc_rule([Other | _], _, _, _, _) ->
    throw({bad_struct, Other});

proc_rule([], _, _, _, _) ->
    {[], [[]], [], []}.


%% Substitutes chain rule structure, checks inconsistent node classes
normalize_schema() ->
    Missing = ets:match(synschema, {'$1', {ref, '$2'}}),
    Known = [{Sym, Ref, Class} ||
                [Sym, Ref] <- Missing,
                {_, {class, Class}} <- ets:lookup(synschema, Ref)],
    lists:foreach(
      fun({Sym, Ref, Class}) ->
              ets:delete_object(synschema, {Sym, {ref, Ref}}),
              ets:insert(synschema, {Sym, {class, Class}}),
              Check = ets:match(synschema, {Sym, {class, '$1'}}),
              Check =:= [[Class]] orelse
                  throw({ambiguous_class, Sym, Check})
      end,
      Known),
    if
        Missing =:= [] ->
            normalize_links();
        Known =:= [] ->
            throw({unknown_class, Missing});
        true ->
            normalize_schema()
    end.

%% Replaces link target symbol names with link target classes, checks missing
%% symbol definitions
normalize_links() ->
    lists:foreach(
      fun([Class, Link, Sym]) ->
              case ets:lookup(synschema, Sym) of
                  [{Sym, {class, Target}}] ->
                      ets:delete_object(synschema, {{Class, Link}, Sym}),
                      ets:insert(synschema, {{Class, Link}, Target});
                  [] ->
                      throw({unknown_class, Sym})
              end
      end,
      ets:match(synschema, {{'$1', '$2'}, '$3'})).

%% Checks optional tokens
%% Checks invalid link order
validate_struct(Struct) ->
    Links = ?MISC:uniq(struct_links(Struct)),
    case lists:sort(Links) == lists:usort(Links) of
        false -> throw({multiple_link_slice, Links});
        true  -> ok
    end.

struct_links(Struct) ->
    lists:flatmap(
        fun
            ({token, _}) -> [];
            ({symbol, Lnk}) -> [Lnk];
            ({repeat, _, Lnk}) -> [Lnk];
            ({optional, Opt}) ->
                case struct_links(Opt) of
                    [] -> throw(opt_no_link);
                    OptLst -> OptLst
                end
        end,
        Struct).

%%% ============================================================================
%%% Source code generation


%% Parser

print_parser(Dev, Params) ->
    {'RootSym', RootSym} = proplists:lookup('RootSym', Params),
    io:put_chars(
      Dev,
      ["Nonterminals\n",
       prettypr:format(
         prettypr:nest(
           2, prettypr:text_par(
                string:join(
                  [atom_to_list(NT) || NT <- ets_keys(rules)], " ")))),
       ".\n\n",
       "Terminals\n",
       prettypr:format(
         prettypr:nest(
           2, prettypr:text_par(
                string:join([io_lib:write(Term) ||
                                {Term} <- ets:tab2list(terminals)], " ")))),
       ".\n\n",
       "Rootsymbol ", atom_to_list(RootSym), ".\n\n"]),
    lists:foreach(
      fun ({LHS, Children, Expr}) ->
              io:format(Dev,
                        "~s ->~n    ~s :~n~s.~n",
                        [LHS, Children, code(4, Expr)])
      end,
      ets:tab2list(rules)),
    io:put_chars(Dev, ["Erlang code.\n\n"]).

%% Node structure info

print_nodes(Dev, Params) ->
    Struct =
        {function,
         {atom, structure},
         [{clause, [Head], none, [Struct]} ||
             #struct{head=Head, structure=Struct} <- ets:tab2list(structure)] ++
         [{clause, [{variable, 'D'}], none,
           [{application, {module_qualifier, {atom, erlang}, {atom, error}},
             [{tuple, [{atom, unknown_structure},{variable,'D'}]}]}]}]},
    Attrs =
        {function,
         {atom, attribs},
         [{clause, [Head], none, [Att]} ||
             #struct{head=Head, attribs=Att} <- ets:tab2list(structure),
             Att =/= {list, []}] ++
         [{clause, [{underscore}], none, [{list, []}]}]},

    Lex =
        {function,
         {atom, lexlink},
         [{clause, [{atom, Class}], none, [{atom, L}]} ||
             {Class, L} <- proplists:get_value('LexLinks', Params)] ++
         [{clause, [{variable, 'C'}], none,
           [{application, {module_qualifier, {atom, erlang}, {atom, error}},
             [{tuple,[{atom, unknown_class},{variable,'C'}]}]}]}]},

    Links =
        lists:foldl(
          fun ({Child, Link}, P) -> orddict:append(Child, Link, P) end,
          [],
          lists:usort([{Child, Link} || {_, Link, Child} <- schema_links()])),

    Parent =
        {function,
         {atom, parentlink},
         [{clause, [{atom, Class}], none,
           [{abstract, proplists:get_value(Class, Links, [])}]} ||
             {Class, _} <- proplists:get_value('LexLinks', Params)] ++
         [{clause, [{variable,'C'}], none,
           [{application, {module_qualifier, {atom, erlang}, {atom, error}},
             [{tuple, [{atom, unknown_class},{variable,'C'}]}]}]}]},

    io:put_chars(
      Dev,
      [code(Struct), "\n\n",
       code(Attrs), "\n\n",
       code(Lex), "\n\n",
       code(Parent), "\n"]).

%% Graph schema

print_schema(Dev) ->
    Links = schema_links(),
    Schema =
        {attribute, {atom, define},
         [{variable, 'SYNTAX_SCHEMA'},
          {list, [{tuple, [{atom, root}, {list, []},
                           {list, [{tuple, [{atom, file},{atom,file}]}]}]} |
                  schema_desc([{file, form, form} | lists:sort(Links)])]}]},
    io:put_chars(Dev, [code(Schema), "\n"]).

schema_desc([{FCl, _, _} | _] = Lst) ->
    {Desc, Tail} = schema_desc(FCl, Lst, []),
    [{tuple, [{atom, FCl},
              {application,
               {atom, record_info},
               [{atom, fields}, {atom, FCl}]},
              {abstract, Desc}]} | schema_desc(Tail)];
schema_desc([]) ->
    [].

schema_desc(FCl, [{FCl, Lnk, TCl} | Tail], Lst) ->
    schema_desc(FCl, Tail, [{Lnk, TCl} | Lst]);
schema_desc(_C, Tail, Lst) ->
    {lists:reverse(Lst), Tail}.


schema_links() ->
    [case ets:lookup(synschema, Key) of
         [{_, To}] -> {FC, Lnk, To};
         Clash -> throw({ambiguous_schema, FC, Lnk, [To || {_,To} <- Clash]})
     end ||
        {FC, Lnk}=Key <- ets_keys(synschema)].


%% Scanner

print_scanner(Dev, Params, Lexicals, Keywords, KwTrans) ->
    Table =
        refgen_scanc:table(
          [{lex_name(Name), lists:flatten(regex_text(Regex))} ||
              {Name, Regex} <- Lexicals]),

    {'KeywordStart', Start} = proplists:lookup('KeywordStart', Params),
    io:put_chars(
      Dev,
      [code(keyword_fun(Keywords)), "\n",
       code(next_fun(KwTrans)), "\n",
       code({function, {atom, start},
             [{clause, [], none, [{atom, Start}]}]}), "\n",
       code({function, {atom, table},
             [{clause, [], none, [{abstract, Table}]}]}), "\n"
      ]).



lex_name(Name) ->
    case atom_to_list(Name) of
        S=[C|_] when C >= $A, C =< $Z -> S;
        _ -> Name
    end.

regex_text(Branches) ->
    string:join([piece_text(Piece) || Piece <- Branches], "|").
piece_text(Seq) ->
    [re_elem_text(El) || El <- Seq].

re_elem_text({rep,    RE}) -> [re_elem_text(RE), "*"];
re_elem_text({rep1,   RE}) -> [re_elem_text(RE), "+"];
re_elem_text({opt,    RE}) -> [re_elem_text(RE), "?"];
re_elem_text({text,   T})  -> escape(T);
re_elem_text({any,    Cl}) -> ["[",  chrclass(Cl), "]"];
re_elem_text({anybut, Cl}) -> ["[^", chrclass(Cl), "]"];
re_elem_text({paren,  RE}) -> ["(",  regex_text(RE), ")"];
re_elem_text({var,    V})  -> ["{", atom_to_list(V), "}"];
re_elem_text({empty,  []}) -> "".

escape([Head | Tail]) ->
    [escape(Head) | escape(Tail)];
escape([]) -> [];
escape(Char) when Char < 32; Char > 127 ->
    io_lib:format("\\~3.8.0b", [Char]);
escape(Char)
  when Char =:= $*; Char =:= $+; Char =:= $\\;
       Char =:= $.; Char =:= $$; Char =:= $^;
       Char =:= $(; Char =:= $); Char =:= $?;
       Char =:= $[; Char =:= $]; Char =:= $|;
       Char =:= ${; Char =:= $};
       Char =:= $\" -> [$\\, Char];
escape(Char) -> Char.

chrclass(Lst) when is_list(Lst) ->
    [chrclass(C) || C <- Lst];
chrclass({From, To}) ->
    [chrclass(From), "-", chrclass(To)];
chrclass(Char)  when Char < 32; Char > 127 ->
    io_lib:format("\\~3.8.0b", [Char]);
chrclass(Char) when Char =:= $\\; Char =:= $-; Char =:= $^; Char =:= $] ->
    [$\\, Char];
chrclass(Char) -> Char.


keyword_fun(KeySets) ->
    {function,
     {atom, keyword},
     [{clause,
       [{atom, Set}, {underscore}, {string, Text}], none, [{atom, Name}]} ||
         #keywset{name=Set} <- KeySets,
         {Text, Name} <- lists:flatten(keyword_set(Set, KeySets))] ++
     [{clause,
       [{underscore}, {variable, 'T'}, {underscore}], none, [{variable, 'T'}]
      }]
    }.

keyword_set(Set, KeySets) ->
    #keywset{keywords=Keywords} = lists:keyfind(Set, #keywset.name, KeySets),
    [case Kw of
         {_, _} -> Kw;
         Ref -> keyword_set(Ref, KeySets)
     end || Kw <- Keywords].


next_fun(KwTrans) ->
    {function, {atom, next},
     [next_clause(State, Trans) ||
         {State, Transitions} <- KwTrans,
         Trans <- Transitions]}.

next_clause(State, {Token, Next}) ->
    {clause, [{atom, State}, {atom, Token}], none, [{atom, Next}]};
next_clause(State, Next) ->
    {clause, [{atom, State}, {underscore}], none, [{atom, Next}]}.


ets_keys(Tab) -> ets_keys(Tab, ets:first(Tab)).
ets_keys(_Tab, '$end_of_table') -> [];
ets_keys(Tab, Key) -> [Key | ets_keys(Tab, ets:next(Tab, Key))].


%%% ============================================================================
%%% Erlang source code generation

%% Code description format: {FunName, Arg1, ..., ArgN}
%%
%% `FunName' is an atom, `ArgI' is either a constant or a code description
%% (possibly nested in lists).
%%
%% `referl_syntax:FunName(Arg1, ..., ArgN)' is called recursively on thi
%% structure to obtain a syntax tree, which is then pretty printed.

-type code_desc() :: tuple().

-spec code(code_desc()) -> string().
code(Desc) -> code(0, Desc).

-spec code(non_neg_integer(), code_desc()) -> string().
code(Ind, Desc) ->
    prettypr:format(
      prettypr:nest(Ind,
                    erl_prettypr:best(construct_code(Desc)))).

construct_code({abstract, Data}) ->
    erl_syntax:abstract(Data);
construct_code(Call) when is_tuple(Call) ->
    [Type | Args] = tuple_to_list(Call),
    apply(erl_syntax, Type, construct_code(Args));
construct_code(Lst) when is_list(Lst) ->
    [construct_code(El) || El <- Lst];
construct_code(Data) ->
    Data.


-file("/usr/local/lib/erlang/lib/parsetools-2.0.7/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try
        {text, Str} = erl_scan:token_info(Token, text),
        {line, Line} = erl_scan:token_info(Token, line),
        Parts = re:split(Str, "\n"),
        Dline = length(Parts) - 1,
        Yline = Line + Dline,
        case erl_scan:token_info(Token, column) of
            {column, Column} ->
                Col = byte_size(lists:last(Parts)),
                {Yline, Col + if Dline =:= 0 -> Column; true -> 1 end};
            undefined ->
                Yline
        end
    catch _:_ ->
        yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    case catch erl_scan:token_info(Token, text) of
        {text, Txt} -> Txt;
        _ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    case catch erl_scan:token_info(Token, location) of
        {location, Loc} -> Loc;
        _ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_unicode_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~p",[Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("./lib/referl_core/src/refcore_gen_syn.erl", 794).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2_1(1, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_1(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_2(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccpars2_19(_S, Cat, [2 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_3(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_3(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_4(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_5(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_6(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_7(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 'yeccgoto_\'ParamVal\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 'yeccgoto_\'ParamVal\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 'yeccgoto_\'ParamVal\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_11(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccpars2_12(12, Cat, [11 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_12(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_13(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_14: see yeccpars2_6

yeccpars2_15(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 yeccpars2_16(_S, Cat, [15 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_16_(Stack),
 'yeccgoto_\'ParamMap\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_17_(Stack),
 'yeccgoto_\'ParamVal\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_18_(Stack),
 'yeccgoto_\'Param\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_19_(Stack),
 'yeccgoto_\'Params\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_20(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_21(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 'yeccgoto_\'Syntax\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_22(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_23(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_24_(Stack),
 'yeccgoto_\'Ruleset\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_25(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 'yeccgoto_\'Rules\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 'yeccgoto_\'Rule\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_28(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_29(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccpars2_30(30, Cat, [29 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_30(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Attrs0\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 'yeccgoto_\'Attrs\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_33(S, '<-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_34(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_35(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_36_(Stack),
 'yeccgoto_\'Attr\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 'yeccgoto_\'Const\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 'yeccgoto_\'Const\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 'yeccgoto_\'Const\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_40(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_40_(Stack),
 'yeccgoto_\'Attr\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_41_(Stack),
 'yeccgoto_\'Attr\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_42(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_(Stack),
 'yeccgoto_\'Attrs\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_44(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_45(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_46(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_47(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 'yeccgoto_\'Elements\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_48: see yeccpars2_45

yeccpars2_49(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 'yeccgoto_\'Element\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_50: see yeccpars2_45

yeccpars2_51(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 'yeccgoto_\'Element\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_53(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_(Stack),
 'yeccgoto_\'Element\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_55(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_(Stack),
 'yeccgoto_\'Element\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_57_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_58_(Stack),
 'yeccgoto_\'Rule\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_59: see yeccpars2_23

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_60_(Stack),
 'yeccgoto_\'Rules\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_61_(Stack),
 'yeccgoto_\'Syntax\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_62(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_62_(Stack),
 yeccpars2_98(98, Cat, [62 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_63(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_63_(Stack),
 'yeccgoto_\'Lexicals\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_64(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_65(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_66_(Stack),
 'yeccgoto_\'Lexical\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_67(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_67_(Stack),
 'yeccgoto_\'Branch\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_68(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_68_(Stack),
 'yeccgoto_\'Regex\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_69(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Piece\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_70(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_71_(Stack),
 'yeccgoto_\'Atom\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_72(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_73_(Stack),
 'yeccgoto_\'Atom\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_74_(Stack),
 'yeccgoto_\'Atom\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 'yeccgoto_\'Atom\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 'yeccgoto_\'Atom\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_77(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_78(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 'yeccgoto_\'Classes\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_79(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_80(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_80_(Stack),
 'yeccgoto_\'Class\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 'yeccgoto_\'Class\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_82(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_83_(Stack),
 'yeccgoto_\'Class\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_84(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_85_(Stack),
 'yeccgoto_\'Atom\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_86_(Stack),
 'yeccgoto_\'Classes\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_87_(Stack),
 'yeccgoto_\'Atom\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_88(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_89_(Stack),
 'yeccgoto_\'Atom\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_90_(Stack),
 'yeccgoto_\'Atom\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_91_(Stack),
 'yeccgoto_\'Piece\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_92_(Stack),
 'yeccgoto_\'Piece\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_93_(Stack),
 'yeccgoto_\'Piece\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_94: see yeccpars2_65

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_95_(Stack),
 'yeccgoto_\'Regex\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_96_(Stack),
 'yeccgoto_\'Branch\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_97_(Stack),
 'yeccgoto_\'Lexicals\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_98(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 yeccpars2_111(_S, Cat, [98 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_99(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_100(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_100_(Stack),
 yeccpars2_101(101, Cat, [100 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_101(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_102(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 yeccpars2_105(_S, Cat, [102 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_103(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 yeccpars2_104(_S, Cat, [103 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_104_(Stack),
 'yeccgoto_\'KwSet\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 'yeccgoto_\'KwSet\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_107(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 yeccpars2_108(_S, Cat, [107 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_108_(Stack),
 'yeccgoto_\'KwSet\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_109(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccpars2_110(_S, Cat, [109 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_110_(Stack),
 'yeccgoto_\'Keywords\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_111_(Stack),
 'yeccgoto_\'Graph\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_112(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_113(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_114(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_115(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_116(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_117(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_118(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_119: see yeccpars2_114

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_120_(Stack),
 'yeccgoto_\'Transitions\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_121(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_(Stack),
 'yeccgoto_\'Transitions\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_123(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_123_(Stack),
 yeccpars2_124(_S, Cat, [123 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_124_(Stack),
 'yeccgoto_\'KwTrans\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

'yeccgoto_\'Atom\''(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Atom\''(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Atom\''(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Atom\''(94, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Attr\''(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Attr\''(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Attrs\''(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Attrs\''(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Attrs0\''(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Branch\''(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(68, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Branch\''(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Branch\''(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(68, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Branch\''(94, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(68, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Class\''(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Class\''(78, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Class\''(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Classes\''(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Classes\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Classes\''(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Const\''(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Element\''(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Element\''(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Element\''(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Element\''(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Elements\''(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(47=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Graph\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Keywords\''(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(98, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Keywords\''(109=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'KwSet\''(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(101, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'KwSet\''(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'KwSet\''(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'KwSet\''(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'KwTrans\''(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'KwTrans\''(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Lexical\''(20, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Lexical\''(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Lexicals\''(20, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Lexicals\''(63=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Param\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Param\''(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'ParamMap\''(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ParamMap\''(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'ParamVal\''(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ParamVal\''(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Params\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Params\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Piece\''(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Piece\''(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Piece\''(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Piece\''(94, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Regex\''(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Regex\''(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Regex\''(94=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Rule\''(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(25, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Rule\''(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(25, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Rules\''(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Rules\''(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Ruleset\''(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(21, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ruleset\''(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(21, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Syntax\''(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Syntax\''(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Transitions\''(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Transitions\''(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_0_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 32).
yeccpars2_0_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_2_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 32).
yeccpars2_2_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_8_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 37).
yeccpars2_8_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_9_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 38).
yeccpars2_9_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 39).
yeccpars2_10_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_11_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 42).
yeccpars2_11_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_15_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 42).
yeccpars2_15_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_16_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 43).
yeccpars2_16_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { value ( __1 ) , __3 } | __4 ]
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 40).
yeccpars2_17_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 35).
yeccpars2_18_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { value ( __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 33).
yeccpars2_19_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 45).
yeccpars2_21_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 48).
yeccpars2_24_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # symrules { lhs = value ( __1 ) , rules = __3 }
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 50).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 55).
yeccpars2_27_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # chain { ref = value ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 57).
yeccpars2_29_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_32_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 60).
yeccpars2_32_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 63).
yeccpars2_36_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # cattr { name = value ( __1 ) , value = __3 }
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 76).
yeccpars2_37_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 77).
yeccpars2_38_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 78).
yeccpars2_39_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 64).
yeccpars2_40_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # tattr { name = value ( __1 ) , token = value ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 66).
yeccpars2_41_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # tattr { name = value ( __1 ) , token = value ( __3 ) , index = value ( __4 ) }
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 61).
yeccpars2_43_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 68).
yeccpars2_47_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 71).
yeccpars2_49_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # term { token = value ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 73).
yeccpars2_52_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # repeat { rule = __2 }
  end | __Stack].

-compile({inline,yeccpars2_54_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 72).
yeccpars2_54_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # nonterm { name = value ( __3 ) , link = value ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_56_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 74).
yeccpars2_56_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # optional { rule = __2 }
  end | __Stack].

-compile({inline,yeccpars2_57_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 69).
yeccpars2_57_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_58_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 54).
yeccpars2_58_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # rule { class = value ( __2 ) , attrs = __4 , rhs = __7 }
  end | __Stack].

-compile({inline,yeccpars2_60_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 51).
yeccpars2_60_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_61_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 46).
yeccpars2_61_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 115).
yeccpars2_62_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_63_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 81).
yeccpars2_63_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_66_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 84).
yeccpars2_66_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { value ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 89).
yeccpars2_67_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_68_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 86).
yeccpars2_68_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 103).
yeccpars2_71_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { any , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 99).
yeccpars2_73_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { text , [ value ( __1 ) ] }
  end | __Stack].

-compile({inline,yeccpars2_74_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 102).
yeccpars2_74_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { any , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_75_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 98).
yeccpars2_75_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { text , value ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 97).
yeccpars2_76_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { var , value ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_78_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 107).
yeccpars2_78_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_80_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 111).
yeccpars2_80_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ value ( __1 ) ]
  end | __Stack].

-compile({inline,yeccpars2_81_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 110).
yeccpars2_81_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 112).
yeccpars2_83_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { value ( __1 ) , value ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_85_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 101).
yeccpars2_85_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { anybut , __3 }
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 108).
yeccpars2_86_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 100).
yeccpars2_87_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { any , __2 }
  end | __Stack].

-compile({inline,yeccpars2_89_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 105).
yeccpars2_89_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { empty , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_90_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 104).
yeccpars2_90_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { paren , __2 }
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 93).
yeccpars2_91_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { rep , __1 }
  end | __Stack].

-compile({inline,yeccpars2_92_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 94).
yeccpars2_92_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { rep1 , __1 }
  end | __Stack].

-compile({inline,yeccpars2_93_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 95).
yeccpars2_93_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { opt , __1 }
  end | __Stack].

-compile({inline,yeccpars2_95_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 87).
yeccpars2_95_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 90).
yeccpars2_96_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 82).
yeccpars2_97_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 124).
yeccpars2_98_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_100_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 119).
yeccpars2_100_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 119).
yeccpars2_102_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 119).
yeccpars2_103_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_104_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 122).
yeccpars2_104_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ value ( __1 ) | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 120).
yeccpars2_105_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ { value ( __1 ) , list_to_atom ( value ( __1 ) ) } | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 119).
yeccpars2_107_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_108_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 121).
yeccpars2_108_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { value ( __1 ) , value ( __3 ) } | __4 ]
  end | __Stack].

-compile({inline,yeccpars2_109_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 115).
yeccpars2_109_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_110_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 117).
yeccpars2_110_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ # keywset { name = value ( __2 ) , keywords = __3 } | __5 ]
  end | __Stack].

-compile({inline,yeccpars2_111_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 30).
yeccpars2_111_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __2 , __3 , __4 , __5 }
  end | __Stack].

-compile({inline,yeccpars2_120_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 128).
yeccpars2_120_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { value ( __1 ) , value ( __3 ) } | __4 ]
  end | __Stack].

-compile({inline,yeccpars2_122_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 127).
yeccpars2_122_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ value ( __3 ) ]
  end | __Stack].

-compile({inline,yeccpars2_123_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 124).
yeccpars2_123_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_124_/1}).
-file("./lib/referl_core/src/refcore_gen_syn.yrl", 125).
yeccpars2_124_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { value ( __2 ) , __4 } | __6 ]
  end | __Stack].


-file("./lib/referl_core/src/refcore_gen_syn.yrl", 741).
