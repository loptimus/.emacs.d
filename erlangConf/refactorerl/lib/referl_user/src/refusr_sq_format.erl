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

%%% @author Lilla Hajós <lya@elte.hu>

-module(refusr_sq_format).

-vsn("$Rev: 7353 $ ").

-export([result/2, nodes/2]).

-export([format_group/3]).

-export([positions_worker/2, file_and_tokens_worker/2]).

-include("user.hrl").
-include("sq_lib.hrl").

-define(Lib, refusr_sq_lib).

%%% ============================================================================
%%% Callbacks

%% @spec result(State::#state{}| {help, {EntityType::atom(),HelpType::atom()}},
%%              DisplayOpt::proplist()) -> term()
%% @doc Formats the results of a query.
%%      `DisplayOpt' contains the keys `positions' and `output'.
%%      The possible values for
%%          - `positions': none|scalar|linecol
%%          - `output': stdio|{iodev,io_device()}|msg|other|nodes
result({help, {EntityType, HelpType}}, DisplayOpt) ->
    {Title, Data} = ?Lib:help_data(EntityType, HelpType),
    case proplists:get_value(output, DisplayOpt, stdio) of
        stdio -> lists:flatten(help(Title, Data));
        msg -> [{nopos, lists:flatten(help(Title, Data))}];  
        _ -> {Title, Data}
    end;

result(State, DisplayOpt) ->
    PosOpt = proplists:get_value(positions, DisplayOpt, none),
    OutputOpt = proplists:get_value(output, DisplayOpt, stdio),
    {NodeType, NodesToPosition, Display} = state_to_disp(State),
    Dict = positions(NodeType, NodesToPosition, PosOpt),
    case OutputOpt of
        nodes ->
            Nodes = nodes_from_state(State),
            Disp  = display(Dict, Display, stdio, PosOpt),
            [{nodes, Nodes}, {text, Disp}];
        _ ->
            %todo: message handling in emacs
            display(Dict, Display, OutputOpt, PosOpt)
    end.

%%% ============================================================================
%%% Help

%% @spec help(Title::string(), [{Name::string(), Synonyms::[string()],
%%                               Description::string()}])-> string()
%% @doc Formats the help data.
help(Title, Data) ->
    {Names, Synonyms, Types} = 
        lists:foldl( fun({Name_, Synonyms_, Type_, _Desc}, {N, S, T}) ->
                             {[Name_|N], [Synonyms_|S], [Type_|T]}
                     end,
                     {[], [], []},
                     Data),

    MaxLengthOfSyns =
        lists:max([0| [length(lists:flatten(S))+length(S)|| S <- Synonyms]]),
    MaxLengthOfSyn =
        lists:max(lists:flatten([0|[[length(S)+1|| S <-Syns]||
				       Syns <- Synonyms]])),

    Col1Width = lists:max([6|[length(Name)+1|| Name <- Names]]),
    Col2Width = if
                    MaxLengthOfSyns < 10 -> 10;
                    MaxLengthOfSyns < 16 -> MaxLengthOfSyns;
                    MaxLengthOfSyn < 16 -> 15;
                    true -> MaxLengthOfSyn
                end,
    Col3Width = lists:max([6| [length(Type)+1|| Type <- Types]]),
    Col4Width = 73-Col1Width-Col2Width-Col3Width,

    Line1 = ["  ", string:chars($\_, 76, "\n")],
    TitleLine = [" | ", Title, string:chars($\ , 75-length(Title), "|\n |"),
                 string:chars($\_, 76, "|\n")],

    ColumnTitles = [" | Name", string:chars($\ , Col1Width-5, "| Synonyms"),
                    string:chars($\ , Col2Width-9, "| Type"),
                    string:chars($\ , Col3Width-5, "| Description"),
                    string:chars($\ , Col4Width-12, "|\n")],

    DataWithLinebreaks =
        lists:map(
          fun({Name, Syns, Type, Desc}) ->
                  SynWithLinebreak =
                      break(lists:flatten([[S, " "]|| S<-Syns]), Col2Width, []),
                  DescWithLinebreak = break(Desc, Col4Width, []),
                  LineNum = lists:max([length(SynWithLinebreak),
                                       length(DescWithLinebreak)]),
                  {LineNum, [Name], SynWithLinebreak, [Type], DescWithLinebreak}
          end,
          Data),

    Lines = [line_text(S, {Col1Width, Col2Width, Col3Width, Col4Width})||
                S <-  DataWithLinebreaks],

    BlankLine = line_text({1, [], [], [], []},
                          {Col1Width, Col2Width, Col3Width, Col4Width}),

    [Line1, TitleLine, ColumnTitles, BlankLine, Lines].

break(String, Length, Lines) ->
    StrLen = length(String),
    case StrLen > Length of
        true ->
            Idx = string:rchr(lists:sublist(String, Length), $\ ),
            {Line, Rest} = lists:split(Idx, String),
            break(Rest, Length, [Line|Lines]);
        false ->
            lists:reverse([String| Lines])
    end.

line_text({0, _, _, _, _}, _) ->
    [];
line_text({1, [], [], [], []}, {W1,W2,W3,W4}) ->
    [" |", string:chars($\_, W1, "|"), string:chars($\_, W2, "|"),
     string:chars($\_, W3, "|"), string:chars($\_, W4, "|\n")];
line_text({LineNum, Name, Syn, Type, Desc}, Widths = {W1,W2,W3,W4}) ->
    NewLineNum = if
                     LineNum == 1 andalso Name /= [] -> 1;
                     true -> LineNum-1
                 end,
    Ch = case NewLineNum of 0 -> $\_; _ -> $\  end,
    {Col1, NextName} =
        case Name of
            [] -> {string:chars(Ch, W1, "|"), []};
            [LN] -> {[LN, string:chars(Ch, W1-length(LN), "|")], []}
        end,
    {Col2, NextSyn} =
        case Syn of
            [] -> {string:chars(Ch, W2, "|"), []};
            [LS|LSs] -> {[LS, string:chars(Ch, W2-length(LS), "|")], LSs}
        end,
    {Col3, NextType} =
        case Type of
               [] -> {string:chars(Ch, W3, "|"), []};
               [LT] -> {[LT, string:chars(Ch, W3-length(LT),"|")], []}
        end,
    {Col4, NextDesc} =
        case Desc of
            [] -> {string:chars(Ch, W4, "|\n"), []};
            [LD|LDs] -> {[LD, string:chars(Ch, W4-length(LD), "|\n")], LDs}
        end,
    [[" |", Col1, Col2, Col3, Col4],
     line_text({NewLineNum, NextName, NextSyn, NextType, NextDesc}, Widths)].

%%% ============================================================================
%%% State

%% @private
%% @spec state_to_disp(#state{}) ->
%%          {EntityType::atom(), EntitiesToPosition::[entity()], Display}
%%       Display = {eq, PropName::atom(), PropValue} |
%%                 {list, EntityType::atom(), [entity()]} |
%%                 {chain, EntityType::atom(), [entity()], PostWS} |
%%                 {group_by, EntityType::atom(), entity()}
%% @doc This gives the list of the entities where a position is needed, and
%%      converts a `state' to an intermediate format used for displaying.
state_to_disp(#state{action=selection, prev_res=[], type=Type, res=Res}) ->
    {Type, Res, [{list, Type, Res}]};

state_to_disp(#state{action=selection, type=Type, res=Res,
		     prev_type=PrevType, prev_res=PrevRes}) ->
    ZippedRes = lists:filter(fun({_, Ns}) -> Ns /= [] end,
			     lists:zip(PrevRes, Res)),
    Disp = [{group_by, PrevType, Entity, list, Type, List}||
	       {Entity, List} <- ZippedRes],
    {Type, ?MISC:flatsort(Res), Disp};

state_to_disp(#state{action=property_query, type=Type, res=Res,
		     prev_type=PrevType, prev_res=PrevRes}) ->
    Disp = [{group_by, PrevType, Entity, eq, Type, PropVal}||
	       {Entity, PropVal} <- lists:zip(PrevRes, Res)],
    {PrevType, ?MISC:flatsort(PrevRes), Disp};

state_to_disp(#state{action=statistics, type=Type, res=Res}) ->
    {none, [], [{eq, Type, Res}]};

state_to_disp(#state{action=iteration, type=Type, res=Chains}) ->
    Incomplete = Chains#chains.incomplete,
    Disp = [{chain, Type, lists:reverse(Chain), "\n"}|| Chain <- Incomplete],
    {Type, ?MISC:flatsort(Incomplete), Disp};

state_to_disp(#state{action=closure, type=Type, res = Chains}) ->
    Complete = Chains#chains.complete,
    Incomplete = Chains#chains.incomplete,
    Recursive = Chains#chains.recursive,
    NodesToPosition = ?MISC:flatsort(Complete ++ Incomplete ++ Recursive),
    Disp = 
	[{chain, Type, lists:reverse(Chain), "...\n"}|| Chain <- Incomplete] ++
	[{chain, Type, lists:reverse(Chain), "\n"}|| Chain <- Complete] ++
	[{chain, Type, lists:reverse(Chain), "*\n"}|| Chain <- Recursive],
    {Type, NodesToPosition, Disp}.

%prop query + stat: error handling!!
nodes_from_state(#state{action=selection, prev_res=[], res=Res}) ->
    Res;
nodes_from_state(#state{action=selection, res=Res}) ->
    ?MISC:flatsort(Res);
nodes_from_state(#state{action=iteration, res = Chains}) ->
    ?MISC:flatsort([lists:last(Chain)|| Chain <- Chains#chains.incomplete]);
nodes_from_state(#state{action=closure, res = Chains}) ->
    ?MISC:flatsort(Chains#chains.complete ++
             Chains#chains.incomplete ++
             Chains#chains.recursive);
nodes_from_state(#state{action=statistics, res=Values}) ->
    Values;
nodes_from_state(#state{action=property_query, res=Props}) ->
    Props.%% ;
%% nodes_from_state(_) -> [].

positions(_Type, _Nodes, none) ->
    dict:new();
% todo: linecol? 1,1 -> {1,1},{1,1}?
positions(file, Nodes, _PositionOpt) ->
    dict:from_list(
      [ begin
            Pos = case ?Graph:class(File) of
                      file ->
                          {?File:path(File), 1, 1};
                      module ->
                          case ?Query:exec(File, ?Mod:file()) of
                              [] -> nopos;
                              [Node] -> {?File:path(Node), 1, 1}
                          end
                  end,
            {File, Pos}
        end || File <- Nodes ]);

positions(Type, Nodes, PositionOpt) ->
    TKeys = 
        [rpc:async_call(node(), ?MODULE, file_and_tokens_worker,
                        [Type, NodeGroup])
         || NodeGroup <- partition_list_according_cpus(Nodes)],
    NodesWithTokens = 
        lists:zip(Nodes, lists:append([rpc:yield(K) || K <- TKeys])),
    CollTokens =
        dict:to_list(
          lists:foldl(
            fun({_Node, []}, Dict) ->
                    Dict;
               %todo: how can the list be longer than 3?
               ({_Node, [File, First, Last|_]}, Dict) ->
                    D1 = case dict:is_key(File, Dict) of
                             false -> dict:store(File, [First], Dict);
                             true -> dict:append(File, First, Dict)
                         end,
                    dict:append(File, Last, D1)
            end,
            dict:new(),
            lists:keysort(2, NodesWithTokens))),
    CKeys = 
        [rpc:async_call(node(), ?MODULE, positions_worker, [List, PositionOpt])
         || List <- CollTokens],
    TokenPos = lists:append([rpc:yield(K) || K <- CKeys]),

    lists:foldl(
      fun({Node, []}, Dict) ->
              dict:store(Node, nopos, Dict);
         %todo: how can the list be longer than 3?
         ({Node, [File, First, Last|_]}, Dict) ->
              {First, {Pos1, _}} = lists:keyfind(First, 1, TokenPos),
              {Last, {_, Pos2}} = lists:keyfind(Last, 1, TokenPos),
              dict:store(Node, {?File:path(File), Pos1, Pos2}, Dict)
      end,
      dict:new(),
      NodesWithTokens).

%% @private
file_and_tokens_worker(Type, Nodes) ->
    [file_and_tokens(Type, Node) || Node <- Nodes].

%% @private
positions_worker({File, Tokens}, PositionOpt) ->
    lists:flatten(?Token:map_pos(File, Tokens, PositionOpt)).

partition_list_according_cpus(List) ->
    Length = length(List),
    GroupNum = 
        case erlang:system_info(logical_processors_available) of
            unknown ->
                (Length div 10) + 1;
            N ->
                (Length div (N*10)) + 1
        end,
    ?MISC:slice_list(GroupNum, List).


display_helper(Dict, Display, OutputOpt) ->
    DispLists = partition_list_according_cpus(Display),
    Keys = 
        [rpc:async_call(node(), ?MODULE, format_group,
                        [Dict, DispList, OutputOpt])
         || DispList <- DispLists],
    lists:flatten([rpc:yield(K) || K <- Keys]).

display(Dict, Display, stdio, _) ->
    TextPos = display_helper(Dict, Display, stdio),
    DispText = [[Text, pos_text(Pos)] || {Pos, Text} <- TextPos],
    lists:flatten(DispText);

display(Dict, Display, msg, scalar) ->
    display_helper(Dict, Display, stdio);

display(Dict, Display, OutOpt, _) when OutOpt == msg orelse OutOpt == other ->
    display_helper(Dict, Display, msg).

fetch(Entity, Dict) ->
    case dict:find(Entity, Dict) of
        error -> nopos;
        {ok, Value} -> Value
    end.

%% @private
format_group(Dict, DispLst, OutputOpt) ->
    [format(Dict,Disp, OutputOpt) || Disp <- DispLst].            

%todo: links to prev results?
format(Dict, {group_by, Type, Entity, list, LType, LEntities}, stdio) ->
    [{fetch(Entity, Dict), text(Type, Entity)}, {nopos, "\n"},
     [ [{nopos, "    "}, {fetch(LEntity, Dict), text(LType, LEntity)},
	{nopos, "\n"}]|| LEntity <- LEntities ]];
format(Dict, {group_by, Type, Entity, eq, PropName, PropVal}, stdio) ->
    [{fetch(Entity, Dict), text(Type, Entity)}, {nopos, "\n    "},
     {nopos, lists:flatten(io_lib:format("~p = ~p\n", [PropName, PropVal]))}];
format(Dict, {list, Type, Entities}, stdio) ->
    [ [{nopos, "    "},
       {fetch(Entity, Dict), text(Type, Entity)},
       {nopos, "\n"}]|| Entity <- Entities ];
format(Dict, {chain, Type, Chain, PostWS}, stdio) ->
    [[[{fetch(Entity, Dict), text(Type, Entity)}, {nopos, " "}]||
         Entity <- Chain],
     {nopos, PostWS}];
format(_Dict, {eq, PropName, PropVal}, stdio) ->
    [{nopos,
      lists:flatten(io_lib:format("    ~p = ~p\n", [PropName, PropVal]))}];

format(Dict, {group_by, Type, Entity, list, LType, LEntities}, msg) ->
    {group_by, {fetch(Entity, Dict), text(Type, Entity)},
     list, [{fetch(LEntity, Dict), text(LType, LEntity)}||
	       LEntity <- LEntities]};
format(Dict, {group_by, Type, Entity, eq, PropName, PropValue}, msg) ->
    {group_by, {fetch(Entity, Dict), text(Type, Entity)},
     eq, PropName, PropValue};
format(Dict, {list, Type, Entities}, msg) ->
    {list, [{fetch(Entity, Dict), text(Type, Entity)}|| Entity <- Entities]};
format(Dict, {chain, Type, Chain, PostWS}, msg) ->
    {chain,
     [{fetch(Entity, Dict), text(Type, Entity)}|| Entity <- Chain], PostWS};
format(_Dict, {eq, _PropName, _PropValue} = Data, msg) ->
    Data.

file_and_tokens(function, Node) ->
   ?Query:exec(Node, ?Query:seq(?Fun:definition(),
                                ?Query:all([ ?Form:file(),
                                             ?Syn:first_leaf(),
                                             ?Syn:last_leaf() ])));

file_and_tokens(record, Node) ->
    ?Query:exec(Node, ?Query:seq(?Rec:form(),
                                 ?Query:all([ ?Form:file(),
                                              ?Syn:first_leaf(),
                                              ?Syn:last_leaf() ])));

file_and_tokens(field, Node) ->
    Rec = ?Query:exec(Node, ?RecField:recorddef()),
    file_and_tokens(record, Rec);

file_and_tokens(macro, Node) ->
   ?Query:exec(Node,
               ?Query:all([?Form:file(), ?Syn:first_leaf(), ?Syn:last_leaf()]));

file_and_tokens(expression, Node) ->
    [File] = ?Syn:get_file(Node),
    [First] = ?Query:exec(Node, ?Syn:first_leaf()),
    [Last] = ?Query:exec(Node, ?Syn:last_leaf()),
    [File, First, Last];

file_and_tokens(variable, Node) ->
    case ?Query:exec(Node, ?Var:bindings()) of
        [Bind | _] -> file_and_tokens(expression, Bind);
        [] -> []
    end.


text(Type, Entity) ->
    lists:flatten(text0(Type, Entity)).

text0(file, File) ->
    case ?Graph:class(File) of
        file ->
            Path = ?File:path(File),
            string:substr(Path, string:rstr(Path, "/")+1);
        module ->
            case ?Query:exec(File, ?Mod:file()) of
                [] -> io_lib:write_atom(?Mod:name(File));
                [Node] ->
                    Path = ?File:path(Node),
                    string:substr(Path, string:rstr(Path, "/")+1)
            end
    end;
text0(function, Fun) ->
    [Mod] = ?Query:exec(Fun, ?Fun:module()),
    ?MISC:fun_text([?Mod:name(Mod), ?Fun:name(Fun), ?Fun:arity(Fun)]);
text0(record, Rec) ->
    io_lib:write_atom(?Rec:name(Rec));
text0(macro, Mac) ->
    ?Macro:name(Mac);
text0(variable, Var) ->
    ?Var:name(Var);
text0(field, Field) ->
    io_lib:write_atom(?RecField:name(Field));
text0(expression, Expr) ->
    Text = string:strip(lists:flatten(?Syn:tree_text(Expr))),
    string:strip(Text, both, $\n).

pos_text(nopos) ->
    [];
pos_text({File, {Pos11, Pos12}, {Pos21, Pos22}}) ->
    ["   ", File, io_lib:format(": ~p,~p-~p,~p", [Pos11, Pos12, Pos21, Pos22])];
pos_text({File, Pos1, Pos2}) ->
    ["   ", File, io_lib:format(": ~p-~p", [Pos1, Pos2])].

%%% ============================================================================
%%% Nodes

%% @spec nodes(Nodes::[entity()], Positions::atom()) -> string()
%%       Positions = none|scalar|linecol
%% @doc Returns a textual representation for a list of nodes.
nodes([], _) -> [];
nodes(Nodes, Positions) ->
    Type = ?Lib:node_type(hd(Nodes)),

    NT = [{Node, ?Lib:node_type(Node)}|| Node <- tl(Nodes)],
    Diffs = lists:filter(fun({_N, NType}) -> NType/=Type end, NT),
    case Diffs of
        [] ->
            Dict = positions(Type, Nodes, Positions),
            lists:flatten(
              [[text(Type, Node), pos_text(fetch(Node, Dict)), "\n"]||
                  Node <- Nodes]);
        _ ->
            throw(?LocalError(type_mismatch, ["nodes", hd(Diffs), Type]))
    end.
