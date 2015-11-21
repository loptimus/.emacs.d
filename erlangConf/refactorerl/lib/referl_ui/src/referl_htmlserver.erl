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

-module(referl_htmlserver).
-vsn("$Rev: 8337 $").

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3,
		 handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, dump/0, reset/0, drop/1, 
		 generate/1, generate_call/1, getdata/1,
		 addQuery/2, getQueries/1, delDirRecursive/2]).

-define(SERVER, htmlserver).

-record(fdata,  {filename		:: string(),
				 filehash		:: integer(),
                 htmldata		:: string()}).

-record(qdata,  {type			:: atom(),
                 queries		:: [string()]}).

-record(state,  {fdata			:: [#fdata{}],
				 qdata			:: [#qdata{}]}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 0, []).

%stop() -> 
%	gen_server:cast(?SERVER, stop).

dump() ->
    gen_server:call(?SERVER, dump, infinity).

reset() -> 
	gen_server:cast(?SERVER, reset).

drop(FileName) ->
    gen_server:cast(?SERVER, {drop,FileName}).

generate(FileName) ->
    gen_server:cast(?SERVER, {generate,FileName}).

generate_call(FileName) ->
    gen_server:call(?SERVER, {generate,FileName}, infinity).

getdata(FileName) ->
    gen_server:call(?SERVER, {getdata,FileName}, infinity).

addQuery(none, _) -> ok;
addQuery(Type, Query) ->
    gen_server:cast(?SERVER, {addquery,Type,Query}).

getQueries(Type) ->
    gen_server:call(?SERVER, {getqueries,Type}, infinity).

%%% ============================================================================
%%% Implementation functions

init(_) ->
	BaseDir=mnesia:system_info(directory)++"/htmlserver",
	StateFile=BaseDir++"/htmlserver_state.dat",
	FileExists=filelib:is_file(StateFile),
	if
		FileExists==true -> 
			{Result, State} = file:consult(StateFile),
			if
				(Result==ok) and (State/=[]) ->
					FileData=readSavedFiles((hd(State))#state.fdata),
					{ok, (hd(State))#state{fdata=FileData}};
				true ->
					{ok, #state{fdata=[],qdata=[]}}
			end;
		true ->
			{ok, #state{fdata=[],qdata=[]}}
	end.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(reset, #state{fdata=FData}) ->
	deleteFiles(FData),
	BaseDir=mnesia:system_info(directory)++"/htmlserver",
	file:delete(BaseDir++"/htmlserver_state.dat"),
    {noreply, #state{fdata=[],qdata=[]}};
handle_cast({drop,FileName}, State=#state{fdata=FData}) ->
	NewData=dropFile(FileName,FData),
	BaseDir=mnesia:system_info(directory)++"/htmlserver",
	file:delete(BaseDir++FileName),
	NewState=State#state{fdata=NewData},
	save(NewState),
    {noreply, NewState};
handle_cast({generate, FileName}, State=#state{fdata=FData}) ->	
	Data=findData(FileName,FData),
	FileNode=reflib_query:exec(reflib_file:find(FileName)),
	if
		FileNode==[] -> 
			self() ! {done,FileName},
			{noreply, State};
		true ->
			Hash=referl_misc:file_hash(hd(FileNode)),
			if	
				Data==[] ->
					HtmlData=generateData(FileName), 
					NewState=State#state{fdata=FData++[#fdata{filename=FileName,filehash=Hash,
							 			   htmldata=HtmlData}]},
					save(NewState,FileName,HtmlData);
				true -> 
					if
						Data#fdata.filehash/=Hash ->
							HtmlData=generateData(FileName),
							NewState=State#state{fdata=replaceData(FileName,FData,HtmlData,Hash)},
							save(NewState,FileName,HtmlData);
						true -> NewState=State
					end
			end,
			self() ! {done,FileName},
			{noreply, NewState}
	end;
handle_cast({addquery, Type, Query}, State=#state{qdata=QData}) ->
	QList=findQuery(Type,QData),
	if	
		QList==[] ->
			NewState=State#state{qdata=QData++[#qdata{type=Type, queries=[Query]}]};
		true -> 
			NewData=replaceQueries(Type,addString(Query,QList),QData),
			NewState=State#state{qdata=NewData}
	end,
	save(NewState),
    {noreply, NewState}.

handle_call({generate, FileName},_, State=#state{fdata=FData}) ->
	Node=reflib_query:exec(reflib_file:find(FileName)),
	if 
	length(Node)/=1 -> 
		 {reply, ok, State};
	true ->	
		Data=findData(FileName,FData),
		[FileNode]=reflib_query:exec(reflib_file:find(FileName)),
		Hash=referl_misc:file_hash(FileNode),
		if	
			Data==[] -> 
				HtmlData=generateData(FileName),
				NewState=State#state{
					fdata=FData++[#fdata{filename=FileName,filehash=Hash,
										 htmldata=HtmlData}]},
				io:format("~s generated.~n",[FileName]),
				save(NewState,FileName,HtmlData);
			true -> 
				if
					Data#fdata.filehash/=Hash ->
						HtmlData=generateData(FileName),
						NewState=State#state{
							fdata=replaceData(FileName,FData,HtmlData,Hash)},
						io:format("~s updated.~n",[FileName]),
						save(NewState,FileName,HtmlData);
					true -> NewState=State
				end
		end,
		{reply, ok, NewState}
	end;
handle_call({getdata,FileName}, {_, _}, State=#state{fdata=FData}) ->
	self()!{done,FileName},
	receive
        {done,File} when File==FileName ->
			Data=findData(FileName,FData),
			if
				Data==[] ->
					FoundData=findData2(FileName,FData);
				true -> 
					FoundData=Data
			end
	end,
	{reply, FoundData#fdata.htmldata, State};
handle_call({getqueries,Type}, {_, _}, State=#state{qdata=QData}) ->
	QList=lists:sort(findQuery(Type,QData)),
	{reply, QList, State};
handle_call(dump, _From, State) ->
    {reply, State, State}.

handle_info({'EXIT', _, _}, State) ->
    {noreply, State};
handle_info({done, _}, State) -> {noreply, State}.
	
terminate(_,_) -> ok.

code_change(_,_,_) -> ok.

%%% ============================================================================
%%% Helper functions

findData(FileName,[#fdata{filename=F}=D|_]) when F==FileName -> D;
findData(FileName,[_|DS]) -> findData(FileName,DS);
findData(_,[]) -> [].

findData2(FileName,[#fdata{filename=F}=D|_]) when F==FileName -> D;
findData2(FileName,[_|DS]) -> findData2(FileName,DS);
findData2(_,[]) -> #fdata{htmldata="File not found!"}.

dropFile(FileName,[#fdata{filename=F}|DS]) when F==FileName -> dropFile(FileName,DS);
dropFile(FileName,[D|DS]) -> [D|dropFile(FileName,DS)];
dropFile(_,[]) -> [].

replaceData(FileName,[#fdata{filename=F}=D|DS],NewData,NewHash) when F==FileName -> 
	[D#fdata{htmldata=NewData,filehash=NewHash}|DS];
replaceData(FileName,[D|DS],NewData,NewHash) -> 
	[D|replaceData(FileName,DS,NewData,NewHash)];
replaceData(_,[],_,_) -> [].

findQuery(Type,[#qdata{type=T}=Q|_]) when T==Type -> Q#qdata.queries;
findQuery(Type,[_|QS]) -> findQuery(Type,QS);
findQuery(_,[]) -> [].

addString(Query,[Q|QS]) when Q/=Query -> [Q|addString(Query,QS)];
addString(_,[_|_]=List) -> List;
addString(Query,[]) -> [Query].

replaceQueries(Type,NewList,[#qdata{type=T}=Q|QS]) when T==Type -> 
	[Q#qdata{queries=NewList}|QS];
replaceQueries(Type,NewList,[Q|QS]) -> 
	[Q|replaceQueries(Type,NewList,QS)];
replaceQueries(_,_,[]) -> [].

save(State=#state{fdata=FData}) ->
	BaseDir=mnesia:system_info(directory)++"/htmlserver",
	filelib:ensure_dir(BaseDir++"/htmlserver_state.dat"),
	{ok, File} = file:open(BaseDir++"/htmlserver_state.dat", [write]),
	io:format(File, "~p.", [State#state{fdata=noFileData(FData)}]),
	file:close(File).
save(State,FileName,HtmlData) ->
	save(State),
	BaseDir=mnesia:system_info(directory)++"/htmlserver",
	filelib:ensure_dir(BaseDir++FileName),	
	file:write_file(BaseDir++FileName, HtmlData,[write,raw]).

noFileData([FD|FDS]) ->
	[FD#fdata{htmldata=[]}|noFileData(FDS)];
noFileData([]) -> [].

readSavedFiles([F=#fdata{filename=FileName}|FS]) ->
	BaseDir=mnesia:system_info(directory)++"/htmlserver",
	{X, Binary} = file:read_file(BaseDir++FileName),
	if
		X==error -> readSavedFiles(FS);
		true -> [F#fdata{htmldata=binary_to_list(Binary)}|readSavedFiles(FS)]
	end;
readSavedFiles([]) -> [].

deleteFiles([#fdata{filename=FileName}|FS]) -> 
	BaseDir=mnesia:system_info(directory)++"/htmlserver",
	file:delete(BaseDir++FileName),
	delDirRecursive(BaseDir,filename:dirname(BaseDir++FileName)),
	deleteFiles(FS);
deleteFiles([]) -> ok.

delDirRecursive(BaseDir,Path) when BaseDir/=Path ->
	E=file:del_dir(Path),
	case E of 
		ok ->
			SplitList=lists:map(fun(X)-> "/"++X end, string:tokens(Path, "/")),
			delDirRecursive(BaseDir,lists:flatten(lists:reverse(lists:nthtail(1,lists:reverse(SplitList)))));
		_ -> ok
	end;
delDirRecursive(_,_) -> ok.

generateData(FileName) -> 
	try
		referl_htmlgen:generate(FileName)
	catch 
		E -> 
			io:format("Error during the generation of ~p: ~p~n",[FileName,E]),
			"Error during file generation!" 
	end.
