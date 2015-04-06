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

%%% ============================================================================
%%% Module information

%%% @doc

%%% @author Zsofia Arvay <zsofia.arvay@gmail.com>
-module(refusr_dupcode).
-vsn("$Rev: 8415 $").

%%nif function
-export([search_dupcode/4]).
%%search functions
-export([search_initial_clones/1]).
%%utility functions
-export([const_var_diff/2, get_result_dets/1, stored_results/0, 
         trim_search_result_ri/3, extend_result_ri/2]).

%-export([generate_name_by_time/0, extract_parameters/1, save_result_dets/3, 
%         check_exist_search/1, get_valid_files/2]).

%-export([trim_search_result/2, extend_result/1]).

-include("user.hrl").

%-define(DEBUG, true).
 
-ifdef(DEBUG).
-define(debug(__String, __Args), io:format(__String, __Args)).
-else.
-define(debug(__String, __Args), ok).
-endif.

%%temporary file to store temporary data
-define(TEMPFILE, "temp").
%%-------------------------------------
-define(LEXLINKS, [flex,clex,elex,llex]).
-define(DUPFORMTYPES, [func,macro,record]).

%% Default parameter values
-define(MINLEN,10).
-define(MINNUM,2).
-define(OVERLAP,0).

-define(TABNAME, dupcode).

-define(FORCE_ADD, false).

%%------------------------------------------------------------------------------
%% completed functions
%%------------------------------------------------------------------------------

%%----------------------------------------------------------
%% get_all_files/0 - get all files from database
%%----------------------------------------------------------
get_all_files() ->
    ReqID = ?UI:getid(),
    ?UI:request(ReqID, {filelist}),
    receive
        {ReqID, reply, DBres} ->
            {ok, FileList} = DBres,
            [Path||{Path,_}<-FileList];
        _ -> []
    end.
extend_all_files() ->
    DBFiles = get_all_files(),
    lists:map(
        fun(DBFile)->
            [File] = ?Query:exec(?Graph:root(),?File:find(DBFile)),
              {DBFile, ?MISC:file_hash(File)}
        end, DBFiles).

%%---------------------------------------------------------
%% is_string/1 - checks whether a list is a string
%%---------------------------------------------------------
is_string([])-> true;
is_string([X|T])-> 
    is_integer(X) andalso 
    X>=0 andalso 
    X=<255 andalso 
    is_string(T);
is_string(_)-> false.

%%---------------------------------------------------------
%% get_valid_files/1
%%---------------------------------------------------------
get_file_by_atom(Modname)->
    Files = 
        ?Query:exec(?Graph:root(),
                    ?Query:seq([?Mod:find(Modname),
                                ?Mod:file()])),
    case Files of
        [] -> io:format("Warning: ~p ignored (not present in the database).~n",
                        [Modname]),
              [];
        [FileNode] -> [{?File:path(FileNode),?MISC:file_hash(FileNode)}]
    end.

get_file_by_string(PathOrRegexp)->
    DBFiles = get_all_files(),
    case lists:member(PathOrRegexp, DBFiles) of
        true -> 
            [FileNode] = ?Query:exec(?Graph:root(),?File:find(PathOrRegexp)),
            [{PathOrRegexp, ?MISC:file_hash(FileNode)}];
        _ ->
            [{PathOrRegexp, Mods}] = 
                refusr_fb_regexp:re([{type, list},{regexp, [PathOrRegexp]}]),
            FileNodes = 
                lists:flatten([?Query:exec(Mod, ?Mod:file())||Mod<-Mods]),
            Files = lists:flatmap(
                fun(FileNode)->
                    Path = ?File:path(FileNode),
                        case lists:member(Path, DBFiles) of
                            false -> [];
                            true -> [{Path, ?MISC:file_hash(FileNode)}]
                        end
                end, FileNodes),
            case Files of
                [] -> io:format("Warning: ~s ignored (not present in the database).~n",
                                [PathOrRegexp]);
                _ -> ok
            end,
            Files
    end.

get_valid_files([], Res)->
    Res;
get_valid_files([DAFHead|DAFTail], Res)->
    Files = 
        case is_string(DAFHead) of
            true -> 
                get_file_by_string(DAFHead);
            false when is_atom(DAFHead) -> 
                get_file_by_atom(DAFHead);
            _ -> 
                io:format("Warning: ~p ignored (neither string nor atom).~n", 
                          [DAFHead]),
                []
        end,
    get_valid_files(DAFTail, lists:umerge(Res, Files)).

%%----------------------------------------------------------
%% get_file_tokens/1 - get tokens of the files from the graph
%%----------------------------------------------------------
get_file_tokens(VAllFiles)->
    FileNodes =
        lists:flatmap(
            fun(Path)-> ?Query:exec(?File:find(Path)) end,
            VAllFiles),
    _TokenNodes =
        lists:flatmap(
            fun(Node)-> ?Syn:leaves(Node) end,
            FileNodes).

%%----------------------------------------------------------
%% alphabet/0 - the token's alphabet
%%----------------------------------------------------------
alphabet() ->
    [{atom, "A"},{'and', "B"},{char, "C"},{'orelse', "D"},{export, "E"},
     {'andalso', "F"},{import, "G"},{integer, "I"},{module, "M"},{'not', "N"},
     {'or', "O"},{'div', "P"},{record, "R"},{string, "S"},{spec, "T"},
     {variable, "V"},{'_', "V"},
     {'after', "a"},{'begin', "b"},{'case', "c"},{'catch', "d"},{'end', "e"},
     {'==', "f"},{'>=', "g"},{'=<', "h"},{'if', "i"},{'++', "j"},{'--', "k"},
     {'||', "l"},{'=/=', "m"},{'=:=', "n"},{stop, "o"},{'of', "p"},{'/=', "q"},
     {'receive', "r"},{'>>', "s"},{'try', "t"},{'<<', "u"},{'<-', "v"},
     {'when', "w"},{'fun', "x"},{'::',"y"},{'->', "z"},
     {'(', "("},{')', ")"},{'{', "{"},{'}', "}"},{'[', "["},{']', "]"},
     {'.', "."},{',', ","},{':', ":"},{';', ";"},{'!', "!"},{'?', "?"},
     {'#', "#"},{'|', "|"},
     {'+', "+"},{'-', "-"},{'*', "*"},{'/', "/"},{'<', "<"},{'>', ">"},{'=', "="}].

%%----------------------------------------------------------
%%all token type substituted with one letter
%%----------------------------------------------------------
process_tokens([])-> [];
process_tokens([Lex|Tail])->
    {lex,token,{token, Token, _, _, _}} = ?Graph:data(Lex),
    case lists:keysearch(Token, 1, alphabet()) of
        false -> ?debug("~nUNDEFINED: ~p~n",[Token]),"u";
        {value,{Token, Value}} -> Value
    end ++ process_tokens(Tail).

%%----------------------------------------------------------
%%load nif
%%----------------------------------------------------------
nif_load() ->
    Nif = filename:join(code:lib_dir(referl_user, priv), "suffix_tree"),
    erlang:load_nif(Nif, 0).

%%----------------------------------------------------------
%% get tokens from all tokens by first and last index
%%----------------------------------------------------------
get_tokens_by_first_last_idx(Tokens,First,Last) ->
    lists:sublist(Tokens,First,Last-First+1).

%%----------------------------------------------------------
%% gets two node minimal common ancestor and its next child
%%----------------------------------------------------------
minimal_common_ancestor_with_children(TokenList)->
    Left = ?Syn:root_path(hd(TokenList),left),
    Right = ?Syn:root_path(lists:last(TokenList),right),
    case ?MISC:common_prefix(Left,Right) of
        [] -> {{root, ?Graph:root()},{hd(Left),hd(Right)}};
        Prefix ->
            %%TODO
            PLen =
                lists:min([length(Prefix),
                           length(Left)-1,
                           length(Right)-1]),
            {lists:last(Prefix),
             {lists:nth(PLen+1,Left),lists:nth(PLen+1,Right)}}
    end.

%%----------------------------------------------------------
%% gets a node all syntactical children between two one
%%----------------------------------------------------------
all_nonlexical_children(Node,First,Last)->
    Children = ?Syn:children(Node),
    ChildrenInterval =
        ?MISC:separate_interval(Children,
                                First,
                                Last),
    SynChildren =
        lists:filter(
            fun({CLink,_})->
                not lists:member(CLink,?LEXLINKS)
            end,ChildrenInterval),
    SynChildren.

%%----------------------------------------------------------
%% returns with the tokenlist of a not fully node
%%----------------------------------------------------------
get_edge_node_leaves(EdgeNode,TokenList)->
    {Link,Node} = EdgeNode,
    Leaves = ?Syn:leaves(Node),
    {Link,Node,?MISC:intersect(Leaves,TokenList)}.

%%----------------------------------------------------------
%% returns with the longest node of a list
%%----------------------------------------------------------
max_length_node_1([],MaxNode,_) -> [MaxNode];
%% left or right edge of the children list (not full node)
max_length_node_1([Head|Tail],MaxNode,MaxLen) when tuple_size(Head) == 3 ->
    {_,Node,List} = Head,
    if
        length(List) > MaxLen ->
            max_length_node_1(Tail,Node,length(List));
        true -> max_length_node_1(Tail,MaxNode,MaxLen)
    end;
max_length_node_1([Head|Tail],MaxNode,MaxLen) when tuple_size(Head) == 2->
    {_,Node} = Head,
    Leaves = ?Syn:leaves(Node),
    if
        length(Leaves) > MaxLen ->
            max_length_node_1(Tail,Node,length(Leaves));
        true -> max_length_node_1(Tail,MaxNode,MaxLen)
    end.
max_length_node(LLeaves,RLeaves,AllNodes)->
    _MaxChild =
        case length(AllNodes) of
            0 -> [];
            1 ->
                [{_,Node}] = AllNodes,
                trim_token_list_up_down(?Syn:leaves(Node));
            2 -> max_length_node_1([LLeaves,RLeaves],nil,0);
            Len -> max_length_node_1([LLeaves]
                                   ++lists:sublist(AllNodes,2,Len-2)
                                   ++[RLeaves],nil,0)
        end.

%%----------------------------------------------------------
%% returns with one syntactical unit corresponding to the
%% tokenlist by trimming
%%----------------------------------------------------------
trim_root_up_down({LFiChild,RFiChild},{LFoChild,RFoChild},TokenList)->
    RootNode = ?Graph:root(),
    Files = ?MISC:separate_interval(?Syn:children(RootNode),
                                    LFiChild,
                                    RFiChild),
    AllForms =
        lists:flatmap(
            fun({_,Node})->
                ?Syn:children(Node)
            end,Files),
    Forms = ?MISC:separate_interval(AllForms,LFoChild,RFoChild),

    %LLeaves = get_edge_node_leaves(LFoChild,TokenList),
    %RLeaves = get_edge_node_leaves(RFoChild,TokenList),
    %MaxChild =
    %    max_length_node(LLeaves,RLeaves,Forms),
    %{{_,LNode,LList},{_,RNode,RList}} = {LLeaves,RLeaves},
    %case MaxChild of
    %    [LNode] -> trim_token_list_up_down(LList);
    %    [RNode] -> trim_token_list_up_down(RList);
    %    _ -> MaxChild
    %end.
    process_forms(Forms,LFoChild,RFoChild,TokenList).
%%----------------------------------------------------------
trim_file_up_down(File,LChild,RChild,TokenList)->
    Forms = ?MISC:separate_interval(?Syn:children(File),
                                    LChild,
                                    RChild),
    %LLeaves = get_edge_node_leaves(LChild,TokenList),
    %RLeaves = get_edge_node_leaves(RChild,TokenList),
    %MaxChild =
    %    max_length_node(LLeaves,RLeaves,Forms),
    %{{_,LNode,LList},{_,RNode,RList}} = {LLeaves,RLeaves},
    %case MaxChild of
    %    [LNode] -> trim_token_list_up_down(LList);
    %    [RNode] -> trim_token_list_up_down(RList);
    %    _ -> MaxChild
    %end.
    process_forms(Forms,LChild,RChild,TokenList).
process_forms(Forms,LChild,RChild,TokenList)->
    {_,FormNodes} = lists:unzip(Forms),
    {_,LNode,LList} = LLeaves = get_edge_node_leaves(LChild,TokenList),
    {_,RNode,RList} = RLeaves = get_edge_node_leaves(RChild,TokenList),
    FullLChild = 
        case lists:prefix(?Syn:leaves(LNode),TokenList) of
            true -> [LNode];
            _ -> []
        end,
    FullRChild = 
        case lists:suffix(?Syn:leaves(RNode),TokenList) of
            true -> [RNode];
            _ -> []
        end,
    FilteredForms = 
        if
            length(FormNodes)>2 -> 
                {greater,filter_forms(FullLChild ++
                lists:sublist(FormNodes,2,length(FormNodes)-2) ++
                FullRChild)};
            length(FormNodes)==2 andalso (length(FullLChild)>0 orelse length(FullRChild)>0) ->
                {equal, filter_forms(FullLChild ++ FullRChild)};
            true -> {nothing, filter_forms(FormNodes)}
        end,
    case FilteredForms of
        {_,[]} -> [];
        {Atom, Result} when Atom==greater orelse Atom==equal -> Result;
        {nothing, [LNode]} -> trim_token_list_up_down(LList);
        {nothing, [RNode]} -> trim_token_list_up_down(RList);
        {nothing, _} ->
            MaxChild =
                max_length_node(LLeaves,RLeaves,Forms),
            case MaxChild of
                [LNode] -> trim_token_list_up_down(LList);
                [RNode] -> trim_token_list_up_down(RList)
            end
    end.
filter_forms(Forms)->
    lists:dropwhile(
        fun(Form)->
            ?debug("Type: ~p~n",[?Form:type(Form)]),
            not lists:member(?Form:type(Form),?DUPFORMTYPES)
        end, Forms).
%%----------------------------------------------------------
trim_expr_up_down(Expr, LChild, RChild, TokenList)->
    case ?Syn:leaves(Expr) == TokenList of
        true -> [Expr];
        _ -> trim_expr_up_down_1(Expr,LChild,RChild,TokenList)
    end.
trim_expr_up_down_1(Expr,LChild,RChild,TokenList)->
    SynChildren = all_nonlexical_children(Expr,LChild,RChild),
    LLeaves = get_edge_node_leaves(LChild,TokenList),
    RLeaves = get_edge_node_leaves(RChild,TokenList),
    MaxChild =
        max_length_node(LLeaves,RLeaves,SynChildren),

    {{_,LNode,LList},{_,RNode,RList}} = {LLeaves,RLeaves},
    case MaxChild of
        [LNode] -> trim_token_list_up_down(LList);
        [RNode] -> trim_token_list_up_down(RList);
        _ -> MaxChild
    end.
%%----------------------------------------------------------
trim_token_list_up_down([])-> [];
trim_token_list_up_down(TokenList)->
    {{_,NAncestor},{LChild,RChild}} =
        minimal_common_ancestor_with_children(TokenList),
    {_,AL,AI} = NAncestor,
    {_,{_,LL,LI}} = LChild,
    {_,{_,RL,RI}} = RChild,
    case ets:match(dupcode,{AL,AI,LL,LI,RL,RI}) of
        [] -> ets:insert(dupcode,{AL,AI,LL,LI,RL,RI}),
              Res = trim_token_list_up_down_case(NAncestor,LChild,RChild,TokenList),
              ets:match_delete(dupcode,{AL,AI,LL,LI,RL,RI}),
              Res;
        _ -> ets:match_delete(dupcode,{AL,AI,LL,LI,RL,RI}),
             []
    end.
trim_token_list_up_down_case(NAncestor,LChild,RChild,TokenList)->
    case ?Graph:class(NAncestor) of
        root ->
            LFoChild = lists:nth(2,?Syn:root_path(hd(TokenList),left)),
            RFoChild =
                lists:nth(2, ?Syn:root_path(lists:last(TokenList),
                                            right)),
            trim_root_up_down({LChild,RChild},
                              {LFoChild,RFoChild},
                              TokenList);
        file ->
            trim_file_up_down(NAncestor,LChild,RChild,TokenList);
        form ->
            case ?Form:type(NAncestor) of
                Type when Type == func ->
                    trim_expr_up_down(NAncestor,LChild,RChild,TokenList);
                _ -> []
            end;
        clause ->
            case ?Clause:type(NAncestor) of
                fundef ->
                    trim_fundef_up_down(NAncestor,LChild,RChild,TokenList);
                _ ->
                    trim_expr_up_down(NAncestor,LChild,RChild,TokenList)
            end;
        expr ->
            trim_expr_up_down(NAncestor,LChild,RChild,TokenList);
        _ -> []
    end.
%%----------------------------------------------------------
trim_fundef_up_down(Fundef,LChild,RChild,TokenList)->
    case ?Syn:leaves(Fundef) == TokenList of
        true -> [Fundef];
        _ -> trim_fundef_up_down_1(Fundef,LChild,RChild,TokenList)
    end.
trim_fundef_up_down_1(Fundef,LChild,RChild,TokenList)->
    {{LLink,LNode},{RLink,RNode}} = {LChild,RChild},
    Bodies =
        lists:filter(
            fun({Link,Node})->
                Link == body andalso Node /= LNode andalso Node /= RNode
            end, all_nonlexical_children(Fundef,LChild,RChild)),
    case length(Bodies) of
        0 ->
            {_,_,LList} = LLeaves =
                case LLink of
                    body -> get_edge_node_leaves(LChild,TokenList);
                    _ -> {LLink,LNode,[]}
                end,
            {_,_,RList} = RLeaves =
                case RLink of
                    body -> get_edge_node_leaves(RChild,TokenList);
                    _ -> {RLink,RNode,[]}
                end,
            MaxChild =
                max_length_node(LLeaves,RLeaves,[]),
            case MaxChild of
                [LNode] -> trim_token_list_up_down(LList);
                _ -> trim_token_list_up_down(RList)
            end;
        _ ->
            {_,BodyNodes} = lists:unzip(Bodies),
            BodyNodes
    end.

%%----------------------------------------------------------
%% gets the amount of lengths corresponding to the nodes
%%----------------------------------------------------------
get_nodes_length([],Acc)-> Acc;
get_nodes_length([Node|Tail],Acc)->
    get_nodes_length(Tail,length(?Syn:leaves(Node))+Acc).

%%----------------------------------------------------------
%% filter the clones by clone numbers and clone lengths
%%----------------------------------------------------------
filter_clones(Cs,Minlen,Minclones)->
    TCs = 
        lists:map(
            fun(CPs)->
                lists:filter(
                    fun(C)->
                        get_nodes_length(C,0) >= Minlen
                    end, CPs)
            end, Cs),
    lists:filter(
        fun(TCPs)->
            length(TCPs) >= Minclones
        end, TCs).

%%----------------------------------------------------------
%% extend clones with filepath, start and end position
%%----------------------------------------------------------
%extend_result(ICs)->
%    lists:map(fun extend_result_1/1, ICs).
extend_result_1([])->[];
extend_result_1([ICPE|Tail])->
    Hd = hd(ICPE),
    [File] = ?Syn:get_file(Hd),
    Path = {filepath, ?File:path(File)},
    HdLexList = ?Syn:leaves(Hd),
    StartLex = hd(HdLexList),
    {StartPos,_} = ?Token:linecol(StartLex),
    Start = {startpos, StartPos},
    EndLex =
        case length(ICPE) of
            1 ->
                lists:last(HdLexList);
            _ ->
                Last = lists:last(ICPE),
                LastLexList = ?Syn:leaves(Last),
                lists:last(LastLexList)
        end,
    {_,EndPos} = ?Token:linecol(EndLex),
    End = {endpos, EndPos},
    Nodes = {nodes, ICPE},
    [[Path,Start,End,Nodes]]++extend_result_1(Tail).

extend_result_ri(From, ICPs)->
    ER = extend_result_1(ICPs),
    From ! {value, ER}.

%%----------------------------------------------------------
%% get parameters from option list
%%----------------------------------------------------------
extract_parameters(Options)->
    Files = 
        case proplists:get_value(files, Options) of
            undefined -> extend_all_files();
            [] -> extend_all_files();
            [Head|Tail] when is_atom(Head) orelse is_list(Head) -> 
                get_valid_files([Head|Tail],[]);
            _ -> throw({error, no_list})
        end,
    ?debug("Files: ~p~n",[Files]),
    Minlen = 
        case proplists:get_value(minlen, Options) of
            undefined -> ?MINLEN;
            Ml when is_integer(Ml) -> max(Ml, ?MINLEN);
            _ -> throw({error, ni_len})
        end,
    Minnum = 
        case proplists:get_value(minnum, Options) of
            undefined -> ?MINNUM;
            Mn when is_integer(Mn) -> max(Mn, ?MINNUM);
            _ -> throw({error, ni_num})
        end,
    Overlap = 
        case proplists:get_value(overlap, Options) of
            undefined -> ?OVERLAP;
            Ol when is_integer(Ol) -> max(Ol, ?OVERLAP);
            _ -> throw({error, ni_ol})
        end,
    Enforce = 
        case proplists:get_value(enforce, Options) of
            undefined -> false;
            Boolean when is_boolean(Boolean) -> Boolean;
            Other1 -> io:format("Error: bad enforce argument ~p~n",[Other1]),
                     false
        end,
    Output = 
        case proplists:get_value(output, Options) of
            undefined -> undefined;
            String -> 
                case is_string(String) of
                    true -> String;
                    Other2 -> throw({error, bad_arg, Other2})
                end
        end,
    Name = 
        case proplists:get_value(name, Options) of
            undefined -> generate_name_by_time();
            Atom when is_atom(Atom) -> Atom;
            Other3 -> throw({error, bad_arg, Other3})
        end,
    [{files, Files},{minlen, Minlen},{minnum, Minnum},{overlap, Overlap},
     {enforce, Enforce},{output, Output},{name, Name}].

%%----------------------------------------------------------
%% gets different constants and variables between two nodes
%%----------------------------------------------------------
const_var_diff(LNodes,RNodes)->
    LList = lists:flatmap(fun(E)->?Syn:leaves(E) end, LNodes),
    RList = lists:flatmap(fun(E)->?Syn:leaves(E) end, RNodes),
    if
        length(LList) == length(RList) ->
            const_var_diff_1(LList,RList,[],[]);
        true -> []
    end.
const_var_diff_1([],[],DConsts,DVars)-> {DConsts,DVars};
const_var_diff_1([LE|LTail],[RE|RTail],DConsts,DVars)->
    {lex,token,{token,Type,LVal,_,_}} = ?Graph:data(LE),
    {lex,token,{token,Type,RVal,_,_}} = ?Graph:data(RE),
    case {Type,{LVal,RVal}} of
        {variable,{LVal,RVal}} when LVal /= RVal ->
            const_var_diff_1(LTail,
                        RTail,
                        DConsts,
                        DVars++
                            [{element(2,hd(?Syn:parent(LE))),
                              element(2,hd(?Syn:parent(RE)))}]);
        {Const,{LVal,RVal}} when (Const == atom orelse 
                                 Const == integer orelse 
                                 Const == float orelse 
                                 Const == string) andalso 
                                 LVal /= RVal ->
            const_var_diff_1(LTail,
                        RTail,
                        DConsts++
                            [{element(2,hd(?Syn:parent(LE))),
                              element(2,hd(?Syn:parent(RE)))}],
                        DVars);
        _ ->
            const_var_diff_1(LTail,RTail,DConsts,DVars)
    end.

%%------------------------------------------------------------------------------
%%code detection
%%------------------------------------------------------------------------------

prepare_search(Files)->
    Tokens = get_file_tokens(Files),
    TokenString = process_tokens(Tokens),
    %% Write tokenstring to a file.
    case file:write_file(?TEMPFILE, TokenString) of
        {error, Rfwe} -> throw({file_write_error, Rfwe});
        ok -> ok
    end,
    Tokens.

do_search(Minlen,Minclones,Overlap)->
    %%Load nif
    case nif_load() of
        {error, Rnle} -> throw({nif_load_error, Rnle});
        ok -> ok
    end,
    %% Do the initial clone search
    _ICs = 
        refusr_dupcode:search_dupcode(?TEMPFILE,Minlen,Minclones,Overlap).

%%----------------------------------------------------------
%%search initial clones
%%[{files,[]},{minlen,integer()},{minnum,integer()},{overlap,integer()}]
%%----------------------------------------------------------
search_initial_clones(Options)->
    Properties = extract_parameters(Options),

    Match = check_exist_search(Properties),
    {Name, EFTICs} =
        case Match of
            [] -> search_initial_clones_1(Properties);
            [[ResName, Result]] -> {ResName, Result}
        end,

    try
        case proplists:get_value(output, Properties) of
            undefined -> ok;
            FileName -> 
                Numbered = lists:zip(lists:seq(1,length(EFTICs)), EFTICs),
                file:write_file(FileName, io_lib:format("Name: ~p~n~p",[Name,Numbered]))
        end
    catch
        _ -> io:format("Error: can't write file, result will not be saved!",[])
    end,

    {Name, EFTICs}.

search_initial_clones_1(Properties)->
    {Files, _} = lists:unzip(proplists:get_value(files, Properties)),
    Tokens = prepare_search(Files),

    Minlen = proplists:get_value(minlen, Properties),
    Minnum = proplists:get_value(minnum, Properties),
    Overlap = proplists:get_value(overlap, Properties),
    
    io:format("~nInitial clone detection started.~n",[]),
    ICs = do_search(Minlen,Minnum,Overlap),
    ?debug("Length: ~p~n",[length(ICs)]),
    io:format("Initial clone detection finished.~n",[]),

    io:format("Trimming clones started.~n",[]),
    ets:new(dupcode,[named_table,bag,public]),
    lists:map(
        fun(ICsE)->
            spawn_link(refusr_dupcode,trim_search_result_ri,[self(),ICsE,Tokens]),
            ?debug("spawned~n",[])
        end, ICs),
    ?debug("Loop finished!~n",[]),
    TICs = loop_dupcode(length(ICs),1,[]),
    ets:delete(dupcode),
    io:format("\rTrimming clones finished.~n",[]),

    io:format("Filter clones...",[]),
    FTICs = filter_clones(TICs,Minlen,Minnum),
    io:format("\rFilter clones finished.~n",[]),

    io:format("Calculating positions started.~nProcess: 0/~p",[length(FTICs)]),
    lists:map(
        fun(FTICsE)->
            spawn_link(refusr_dupcode, extend_result_ri, [self(),FTICsE])
        end, FTICs),
    EFTICs = loop_dupcode(length(FTICs),1,[]),
    io:format("\rCalculating positions finished.~n",[]),
    
    Name = proplists:get_value(name, Properties),
    save_result_dets(Name, Properties, EFTICs),
    
    {Name, EFTICs}.

loop_dupcode(Sum,Val,TICs) when Sum < Val->
    TICs;
loop_dupcode(Sum,Val,TICs)->
    receive
        {value, Ret}->
            io:format("\rProcess: ~p/~p",[Val,Sum]),
            loop_dupcode(Sum,Val+1,TICs++[Ret]);
        {'EXIT',_,normal} ->
            loop_dupcode(Sum,Val,TICs);
        Error -> 
            io:format("~nerror: ~p~n",[Error]),
            loop_dupcode(Sum,Val+1,TICs)
    end.

trim_search_result_ri(From, {RangeList,_,_}, Tokens)->
    ?debug("Process started: ~p~n",[From]),
    ICTokens =
        lists:map(
            fun({Start,End})->
                get_tokens_by_first_last_idx(Tokens,Start,End)
            end, RangeList),
    TICs = 
        lists:map(
            fun(TokenList)->
                trim_token_list_up_down(TokenList)
            end, ICTokens),
    From ! {value, TICs},
    ?debug("Process ended: ~p~n",[From]),
    TICs.

%trim_search_result(Tokens,{RangeList,_,_})->
%    ICTokens =
%        lists:map(
%            fun({Start,End})->
%                get_tokens_by_first_last_idx(Tokens,Start,End)
%            end, RangeList),
%    _TICs = 
%        lists:map(
%            fun(TokenList)->
%                trim_token_list_up_down(TokenList)
%            end, ICTokens).

%%---------------------------------------------------------
%% dets utilities
%%---------------------------------------------------------
open_dets()->
    case dets:open_file(?TABNAME, [{file, filename:join("data",atom_to_list(?TABNAME))}]) of
        {ok, ?TABNAME} -> ok;
        Error -> throw(Error)
    end.

insert_dets(Key, Data)->
    case dets:insert(?TABNAME, {Key, Data}) of
        ok -> ok;
        Error -> throw(Error)
    end.

close_dets()->
    case dets:close(?TABNAME) of
        ok -> ok;
        Error -> throw(Error)
    end.

save_result_dets(Name, Properties, Data)->
    open_dets(),
    NewProp = 
        [proplists:lookup(files, Properties),
         proplists:lookup(minlen, Properties),
         proplists:lookup(minnum, Properties),
         proplists:lookup(overlap, Properties)],
    insert_dets(Name, {NewProp, Data}),
    close_dets().

check_exist_search(Properties)->
    open_dets(),
    NewProp = 
        [proplists:lookup(files, Properties),
         proplists:lookup(minlen, Properties),
         proplists:lookup(minnum, Properties),
         proplists:lookup(overlap, Properties)],
    Match = dets:match(?TABNAME, {'$1',{NewProp, '$2'}}),
    close_dets(),
    Match.

get_result_dets(Name)->
    open_dets(),
    Result = dets:lookup(?TABNAME, Name),
    close_dets(),
    Result.

stored_results()->
    open_dets(),
    Res = dets:match(?TABNAME, {'$1',{'$2','_'}}),
    close_dets(),
    Res.

time_extend_right(Str)->
    string:right(Str,2,$0).
generate_name_by_time()->
    {{Y,M,D},{H,N,S}} = erlang:localtime(),
    NameString = "temp"
        ++integer_to_list(Y)
        ++time_extend_right(integer_to_list(M))
        ++time_extend_right(integer_to_list(D))
            ++time_extend_right(integer_to_list(H))
            ++time_extend_right(integer_to_list(N))
            ++time_extend_right(integer_to_list(S)),
    list_to_atom(NameString).

search_dupcode(_Files, _Minlen, _Minclones, _Overlap) ->
    "Not loaded!".
