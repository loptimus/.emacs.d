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
%%% The Initial Developer of the  Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s  Lor�nd University are  Copyright 2008-2009,
%%% E�tv�s Lor�nd University. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc This refactoring renames record fields. After the
%%% transformation, the old name will be replaced by the new name in
%%% the record definition and in every reference to the given record
%%% field (e.g.\ record field access or field update expressions). The
%%% condition of the renaming is that there is no name conflict with
%%% another field in the record.
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li>The record field to be renamed
%%%       (see {@link reflib_args:record_field/1}).</li>
%%%   <li>The new name of the field
%%%       (see {@link reflib_args:name/1}).</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li>There must be no field with the new name in the record</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%   <li>The field name is changed to the new name in the definition
%%%   of the record and in every record expression that refers the
%%%   record field.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(reftr_rename_recfield).
-vsn("$Rev: 8204 $"). % for emacs"

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Errors

%% @private
error_text(name_collision, []) ->
    "Name collision with an existing field name";

error_text(new_fieldname_identical, [])->
    "New fieldname identical to the old one".


%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Field     = ?Args:record_field(Args),
    OldFieldName = ?RecField:name(Field),
    [Record]     = ?Query:exec(Field, ?RecField:recorddef()),
    [File]       = ?Query:exec(Record, ?Rec:file()),
    FilePath     = ?File:path(File),
    RecName      = ?Rec:name(Record),
    ArgsInfo     = add_info_text(Args, RecName, Field),
    Names        = recfield_names(Field, Record),
    % todo Add transformation info
    NewName      = ?Args:ask(ArgsInfo, name, fun cc_rfname/2, fun cc_error/3,
                             {RecName, OldFieldName, Names}),
    NameStr      = io_lib:write_atom(NewName),

    Refs         = ?Query:exec(Field, ?RecField:references()),
    ToFieldTypexp = [{fielddef, back}],
    [TypExp]     = ?Query:exec(Field, ToFieldTypexp),

    ?Macro:check_single_usage([{[TypExp], [{tlex, 1}]}]),

    [?Transform:touch(Node) || Node <- [TypExp | Refs]],
    [fun() ->
        ?Macro:inline_single_virtuals(Refs, elex),
        [?Macro:update_macro(Expr, {elex, 1}, NameStr) || Expr <- Refs],
        ?Macro:update_macro(TypExp, {tlex, 1}, NameStr)
    end,
    fun(_)->
            ?Query:exec(?Query:seq([?File:find(FilePath),
                                    ?Rec:find(RecName),
                                    ?Rec:field(NewName)]))
    end].

add_info_text(Args, RecName, Field) ->
    FieldName = ?RecField:name(Field),
    Info      = ?MISC:format("Renaming record field #~p.~p", [RecName, FieldName]),
    [{transformation_text, Info} | Args].

%%% ===========================================================================
%%% Implementation

%% Returns the record field names of `Record', except for `Field'.
recfield_names(Field, Record) ->
    [(?ESG:data(F))#field.name
        ||  F <- ?Query:exec(Record, ?Rec:fields()),
            F =/= Field].

%%% ----------------------------------------------------------------------------
%%% Checks

cc_rfname(NewRecFieldName, {_RecName, OldFieldName, Names}) ->
    ?Check(NewRecFieldName =/= OldFieldName, ?LocalErr0r(new_fieldname_identical)),
    ?Check(not lists:member(NewRecFieldName, Names), ?LocalErr0r(name_collision)),
    NewRecFieldName.

cc_error(?LocalErr0r(Type), NewRecFieldName, {RecName, _OldFieldName, _Names}) ->
    case Type of
        new_fieldname_identical -> ?MISC:format("The name of the record field is already #~p.~p.", 
                                               [RecName, NewRecFieldName]);
        name_collision -> ?MISC:format("The record field name #~p.~p is already used.",
                 [RecName, NewRecFieldName])
    end.
