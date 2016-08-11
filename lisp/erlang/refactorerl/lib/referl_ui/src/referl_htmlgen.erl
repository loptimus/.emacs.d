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

%%% @author Tamas Hoffman <hoffmantamas@caesar.elte.hu>

%TODO: macros in funs in macros, open trees at filefilter
%regexp dupcodes?, form errors?

-module(referl_htmlgen).
-vsn("$Rev: 8382 $").

-export([generate/1,getFile/1,getType/1,getProperNode/1,
		 filterNodes/1,findTokens/1,checkMacro/1, onlyText/1]).

-include_lib("referl_user/src/user.hrl").

%% @spec generate(filenode()) -> string()
%% @doc Returns a html text with links on specific nodes.
generate(Filename) ->
	Node=?Query:exec(?File:find(Filename)),
	if 
		length(Node)/=1 -> 
			error;
		true ->	
			lists:flatten(tree_text(hd(Node)))
	end.

%% @spec tree_text(node()) -> Chars
%%       Chars = [char() | Chars]
%% @doc Returns the textual representation of the syntactical subtree that
%% starts at `Top' as a deep list.
tree_text(Top) ->
    [lists:flatten(htmlConverts(T#token.prews)) ++ addLinks(LexNode,T,Data) 
		++ lists:flatten(htmlConverts(T#token.postws))
	        || {LexNode,Data} <- leaves(Top),
	           #lex{data=T=#token{}} <- [?ESG:data(LexNode)]].

addLinks(LexNode={_,_,Id},T,Data) ->
	addLinkStart(LexNode,Data,true) 
	++ highLight(T#token.type,T#token.text) 
	++ addLinkEnd(Id,lists:reverse(Data)).

addLinkStart(_,[],_) -> [];
addLinkStart(LexNode={_,_,Id},[{From,_,Type,{_,PType,PId}}|Datas],First) when (Id==From) and (First) -> 
	"<a" ++ highLight(Type) ++ " id=jump_"++atom_to_list(PType)++integer_to_list(PId)
	++" class=mylink href=\"javascript:void(0)\" onclick=javascript:jt.value=\""
	++atom_to_list(PType)++"|"++integer_to_list(PId)++"\";$(jb).click();>" 
		++ addLinkStart(LexNode,Datas,false);
addLinkStart(LexNode={_,_,Id},[{From,_,Type,{_,PType,PId}}|Datas],_) when (Id==From) -> 
	"<span" ++ highLight(Type) ++ " id=jump_"++atom_to_list(PType)++integer_to_list(PId)++">" 
		++ addLinkStart(LexNode,Datas,false);
addLinkStart(LexNode,[_|Datas],_) -> addLinkStart(LexNode,Datas,false).

addLinkEnd(_,[]) -> [];
addLinkEnd(Id,[{_,To,_,_}|[]]) when (Id==To) -> 
	"</a>" ++ addLinkEnd(Id,[]);
addLinkEnd(Id,[{_,To,_,_}|Datas]) when (Id==To) -> 
	"</span>" ++ addLinkEnd(Id,Datas);
addLinkEnd(Id,[_|Datas]) -> 
	addLinkEnd(Id,Datas).

%% @spec leaves(node()) -> [node()]
%% @doc Returns the leaves of the syntactical subtree that starts at `Top',
%% in the correct syntactical order. This yields the original token nodes of
%% the subtree.
leaves(Top) ->
    Nodes = ets:new(visited_nodes, [set]),
    try
        leaves(Top, Nodes, [{0,0,0,0}], false)
    after
        ets:delete(Nodes)
    end.

leaves(Top={_,_,Id}, Nodes, SearchIntervals=[{_,Max,_,_}|_],InMacro) ->
    case ets:lookup(Nodes, Top) of
        [] ->
            ets:insert(Nodes, {Top}),
            case refcore_syntax:children(Top) of
                [] ->	
					if
						InMacro==true ->
							Node=findOrigNode(Top),
							if
								Node==[] -> Data=SearchIntervals;
								true ->
									{_,_,Type}=intervalHelper(findTokens(hd(Node))),
									if
										Type==none -> Data=[];
										true -> 
											Data=try 
												checkMacroForFun(hd(Node),id(Top),Type)++[{id(Top),id(Top),Type,hd(Node)}] 
											catch _ -> [] end
									end
							end;
						true -> Data=SearchIntervals
					end,
					if
						(Id>Max) and (not InMacro) -> {Top,[]};
						true -> {Top,Data}
					end;
                Children ->
					NodeData=?ESG:data(Top),
					case NodeData of
						#lex{type=subst} -> IM=true;
						_ -> IM=InMacro
					end,
					lists:flatten([checkChildren(C,Nodes,SearchIntervals,IM) 
						|| {_,C} <- Children])
            end;
        _  ->
            []
    end.

checkMacroForFun(N,Id,T) when T==atom ->
	Node=?Query:exec1(N,[{esub,back}],error),
	NodeData=?ESG:data(Node),
	case NodeData of
		#expr{type=infix_expr} ->
			ApplicationNode=?Query:exec1(Node,[{esub,back}],error);
		_ ->
			ApplicationNode=Node
	end,
	Nodes=getApplicationNodes(ApplicationNode),
	if
		(Node==N) -> [];
		true ->
			%Tokens=lists:usort(lists:flatten([checkMacro(Nd) || Nd<-Nodes])),
			LexTokens=lists:map(fun(X) -> deepOrigSearch(X) end, Nodes),
			{From,To,Type}=intervalHelper({LexTokens,funappl}),
			if
				(From=<Id) and (Id=<To) -> [{From,To,Type,ApplicationNode}];
				true -> []
			end
	end;
checkMacroForFun(_,_,_) -> [].

checkChildren(Node,Nodes,SearchIntervals=[{Min,Max,_,_}|_],IM) ->
	{From,To,Type}=intervalHelper(findTokens(Node)),
	if
		From==0 -> NextSearchNodes=SearchIntervals;
		(From>Max) or (To<Min) -> NextSearchNodes=[{From,To,Type,Node}];
		true -> NextSearchNodes=SearchIntervals++[{From,To,Type,Node}]
	end,
	leaves(Node, Nodes, NextSearchNodes,IM).

intervalHelper({[],Type}) -> {0,0,Type};
intervalHelper({Nodes,Type}) ->
	LexTokens=lists:usort(lists:flatten([checkMacro(N) || N<-Nodes])),
	if
		LexTokens==[] -> {0,0,Type};
		true -> {id(hd(LexTokens)),id(lists:last(LexTokens)),Type}
	end.

findTokens(Node) ->
	NodeData=?ESG:data(Node),
	case NodeData of
		#form{type=module} ->
			{?Query:exec(Node,[{flex,2}]),
			mod};
		#form{type=record} ->
			{?Query:exec(Node,[{flex,2}]),
			recdef};
		#form{type=macro} ->
			{?Query:exec(Node,[{flex,2}]),
			macrodef};
		#expr{type=application} ->
			{getApplicationNodes(Node),funappl};
		#expr{type=implicit_fun} ->
			{getApplicationNodes(Node),funappl};
		#clause{type=fundef} ->
			{?Query:exec(Node,[{name,1},{elex,1}]),
			fundef};	
		#expr{type=variable} ->
			{?Query:exec(Node,[{elex,1}]),
			var};
		#expr{type=record_expr} ->
			{?Query:exec(Node,[{elex,2}]),
			recexpr};
		#expr{type=record_field} ->
			{?Query:exec(Node,[{elex,1}]),
			recfield};
		#expr{type=record_access} ->
			{?Query:exec(Node,[{elex,2}]),
			recexpr};
		#expr{type=record_update} ->
			{?Query:exec(Node,[{elex,2}]),
			recexpr};
		#expr{type=atom} ->
			Path=?Query:exec(Node,?Expr:fields()),
			if
				Path/=[] -> {?Query:exec(Node,[{elex,1}]),recfield};
				true -> {?Query:exec(Node,[{elex,1}]),atom}
			end;
		_ -> 
			{[],none}
	end.
	
getApplicationNodes(Node) ->
	N=?Query:exec1(Node,[{esub,1}],error),
	NodeData=?ESG:data(N),
	case NodeData of
		#expr{type=infix_expr} ->
				[?Query:exec1(N,[{esub,1},{elex,1}],error),
				?Query:exec1(N,[{elex,1}],error),
				?Query:exec1(N,[{esub,2},{elex,1}],error)];
		#expr{} -> 
			?Query:exec(N,[{elex,1}])
	end.

checkMacro([Node]) -> checkMacro(Node);
checkMacro(Node) ->
	NodeData=?ESG:data(Node),
	case NodeData of
		#lex{type=token,data=virtual} ->
			HasParam=?Query:exec(Node,[{llex,1},{llex,4}]),
			if
				HasParam/=[] -> [];
				true ->
					Nodes=[?Query:exec1(Node,[{llex,1},{llex,1}],error),
					?Query:exec1(Node,[{llex,1},{llex,2}],error)],
					[checkMacro(N) || N<-Nodes]
			end;
		_ -> 
			Node
	end.

%%% ============================================================================
%%% Html specific functions

highLight(comment,Text) -> "<span style='color: #990000'>"++Text++"</span>";
highLight(variable,Text) -> "<span style='color: #996600'>"++Text++"</span>";
highLight(string,Text) -> "<span style='color: #FF00DD'>"++Text++"</span>";
highLight('case',Text) -> "<span style='color: #9933CC'>"++Text++"</span>";
highLight('of',Text) -> "<span style='color: #9933CC'>"++Text++"</span>";
highLight('end',Text) -> "<span style='color: #9933CC'>"++Text++"</span>";
highLight('if',Text) -> "<span style='color: #9933CC'>"++Text++"</span>";
highLight('when',Text) -> "<span style='color: #9933CC'>"++Text++"</span>";
highLight('fun',Text) -> "<span style='color: #9933CC'>"++Text++"</span>";
highLight('try',Text) -> "<span style='color: #9933CC'>"++Text++"</span>";
highLight('catch',Text) -> "<span style='color: #9933CC'>"++Text++"</span>";
highLight('module',Text) -> "<span style='color: #9933CC'>"++Text++"</span>";
highLight('export',Text) -> "<span style='color: #9933CC'>"++Text++"</span>";
highLight(_,Text) -> Text.

highLight(recdef) -> " style='color: #9933CC'";
highLight(macrodef) -> " style='color: #9933CC'";
highLight(fundef) -> " style='color: #0000FF'";
highLight(funappl) -> " style='color: #006600'";
highLight(recexpr) -> " style='color: #006600'";
highLight(recfield) -> " style='color: #66CCFF'";
highLight(recaccess) -> " style='color: #66CCFF'";
highLight(_) -> [].

htmlConverts([]) -> [];
htmlConverts(S) -> 
	IsComment=lists:any(fun(X)-> X==true end,[isCommentChar(C) || C <- S]),
	Text=lists:flatten([replaceChars(C) || C <- S]),
	if
		IsComment == true -> highLight(comment,Text);
		true -> Text
	end.

isCommentChar(C) when ((C/=32) and (C/=10) and (C/=9)) -> true;
isCommentChar(_) -> false.

replaceChars(C) ->
	case C of
		32 -> "&nbsp;";                   % ' '
		10 -> "<br />";                   % '\n'
		9  -> "<b>&nbsp;&nbsp;&nbsp;&nbsp;</b>"; % '\t'
		$< -> "&lt;";					  % '<' 
		$> -> "&gt;";					  % '>'
		$& -> "&amp;";					  % '&'
		34 -> "&quot;";					  % '"'
		_  -> C
	end.

%%% ============================================================================
%%% Helper functions

getFile(Node) ->
	FileNode=getFile_(Node),
	if 
		FileNode/=[] -> 
			FilePath=?File:path(hd(FileNode));
		true -> FilePath=""
	end,
	
	FilePath.
getFile_({_,func,_}=Node) ->
	?Query:exec(Node,
				?Query:seq([?Fun:module(),?Mod:file()]));
getFile_({_,clause,_}=Node) ->
	?Query:exec(Node,
				?Query:seq([?Clause:form(),?Form:module(),?Mod:file()]));
getFile_({_,form,_}=Node) ->
	?Query:exec(Node,
				?Query:seq([?Form:module(),?Mod:file()]));
getFile_({_,_,_}=Node) ->
	PE=?Query:exec(Node,
				?Query:seq([?Expr:clause(),?Clause:form(),?Form:file()])),
	if
		PE==[] -> ?Query:exec(Node,
				?Query:seq([?Expr:nameof(),?Clause:form(),?Form:file()]));
		true -> PE
	end;
getFile_(Node) ->
	?Query:exec(?Query:exec(?Mod:find(Node)),?Mod:file()).

getType(Node) ->
	NodeData=?ESG:data(Node),
	case NodeData of
		#form{type=module} -> mod;
		#form{type=record} -> recdef;
		#form{type=macro} -> macrodef;
		#expr{type=application} -> funappl;
		#expr{type=implicit_fun} -> funappl;
		#clause{type=fundef} -> fundef;
		#expr{type=variable} -> var;
		#expr{type=record_expr} -> recexpr;
		#expr{type=record_field} -> recfield;
		#expr{type=record_access} -> recexpr;
		#expr{type=record_update} -> recexpr;
		#expr{type=atom} ->
			Path=?Query:exec(Node,?Expr:fields()),
			if
				Path/=[] -> recfield;
				true -> atom
			end;
		_ -> 
			none
	end.

getProperNode(Node) ->
	Type=getType(Node),
	case Type of
		macrodef -> N=[Node];
		mod -> N=?Query:exec(Node,?Form:module());
		recdef -> N=?Query:exec(Node,?Form:record());
		var -> N=?Query:exec(Node,?Expr:variables());
		funappl -> N=?Query:exec(Node,?Query:any(?Expr:function(),?Expr:dynfunction()));
		fundef -> N=?Query:exec(Node,?Query:seq([?Clause:form(),?Form:func()]));
		recexpr -> N=?Query:exec(Node,?Expr:record());
		recfield -> N=?Query:exec(Node,?Expr:field());
		atom -> N=[Node];
		_ -> N=[]
	end,
	if
		N==[] -> error;
		true -> hd(N)
	end.

filterNodes([Node|Nodes]) -> filterNodes_([Node|Nodes],true);
filterNodes([]) -> [].
filterNodes_([Node|Nodes],First) ->
	case Node of
		{_,func,_} ->
			N=?Query:exec(Node,
				?Query:seq([?Fun:definition(),
							?Form:clauses()]));
		{_,record,_} ->
			N=?Query:exec(Node,?Rec:form());
		{_,module,_} ->
			N=?Query:exec(Node,?Query:seq([?Mod:file(),?File:form(1)]));
		{_,_,_} -> 
			N=[Node];
		_ -> N=?Query:exec(?Query:exec(?Mod:find(Node)),?Query:seq([?Mod:file(),?File:form(1)]))
	end,
	if
		N==[] -> filterNodes_(Nodes,true);
		true ->
			File=getFile(hd(N)),
			Type=getType(hd(N)),
				if
					(File/="") and (Type/=none) ->
						if
							First==true ->
								referl_htmlserver:generate(File);
							true -> ok
						end,	
						N++filterNodes_(Nodes,false);
					(File/="") -> N++filterNodes_(Nodes,true);
					true -> filterNodes_(Nodes,true)
				end
	end;
filterNodes_([],_) -> [].

findOrigNode(Node) ->
	PrevNode=?Query:exec(Node,[{orig,back}]),
	if
		PrevNode==[] ->
			RealPrevNode=?Query:exec(Node,[{elex,back}]),
			if
				RealPrevNode/=[] -> RealPrevNode;
				true -> []
			end;
		true -> findOrigNode(hd(PrevNode))
	end.

deepOrigSearch(Node) ->
	Result=?Query:exec(Node,[{orig,1}]),
	if
		Result==[] -> Node;
		true -> deepOrigSearch(hd(Result))
	end.

onlyText(Node) ->
	[[T#token.text] || Token <- refcore_syntax:leaves(Node), #lex{data=T=#token{}} <- [?ESG:data(Token)]].

id({_,_,Id}) -> Id.

