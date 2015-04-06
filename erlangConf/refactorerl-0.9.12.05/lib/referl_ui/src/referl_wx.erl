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
%%% Portions created  by Eötvös  Loránd University are  Copyright 2010,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Erlang WX user interface management functions.
%%%
%%% @author Zsofia Arvay <zsofia.arvay@gmail.com>

-module(referl_wx).
-svn("$Rev: 8012 $ ").

-export([start/0, stop/0]).

-include("ui.hrl").
-include("gs.hrl").
-include_lib("wx/include/wx.hrl").

-define(Options, wx_options).
-define(Server, referl_wx_server).
-define(Watch, wx_ui_watcher).

%% @doc Starts the Erlang WX graphical refactoring browser user interface
%% and opens its main window.
start() ->
    case whereis(?Server) of
    undefined ->
        register(?Server, spawn(fun() -> start_spawn() end)),
        ok;
    _ ->
        already_started
    end.

%% @doc Shuts down the Erlang WX graphical interface and closes its windows.
stop() ->
    terminate().

start_spawn() ->
    Server = wx:new(),
    ?Config:start_link(),
    referl_ui_evsend:start(watch_start()),

    MB = wxMenuBar:new(),
        File = wxMenu:new(),
        Refac = wxMenu:new(),
    wxMenuBar:append(MB, File, "File"),
    wxMenuBar:append(MB, Refac, "Refactor"),
    
    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    Frame = wxFrame:new(Server, -1, "Proba", 
        [{pos, {30,30}}, {size, {800,600}}]),
        wxFrame:setName(Frame, "win"),
        wxFrame:setMinSize(Frame, {600, 300}),
        wxFrame:connect(Frame, close_window),
        wxFrame:setMenuBar(Frame, MB),
        wxFrame:connect(Frame, command_menu_selected, []),

    TB = wxFrame:createToolBar(Frame, []),

    TreeSizer = wxBoxSizer:new(?wxVERTICAL),
    TPanel = wxPanel:new(Frame, 
        [{style, ?wxVSCROLL bor ?wxHSCROLL},{size,{400,-1}}]),
        wxPanel:setName(TPanel, "ptree"),
        wxPanel:setBackgroundColour(TPanel, 
            ?Config:color(tree, background)),
    wxSizer:add(TreeSizer, TPanel, [{proportion, 1},{flag, ?wxEXPAND}]),

    wxPanel:connect(TPanel, scrollwin_pagedown),
    wxPanel:connect(TPanel, scrollwin_pageup),
    wxPanel:connect(TPanel, scrollwin_thumbtrack),
    
    TextSizer = wxBoxSizer:new(?wxVERTICAL),
    IStat = wxStaticText:new(Frame, -1, "Information:", []),
        wxStaticText:setName(IStat, "istat"),
        wxSizer:add(TextSizer, IStat, [{proportion,0},{flag,?wxEXPAND}]),
        wxSizer:add(TextSizer, 0, 10, [{proportion,0},{flag,?wxEXPAND}]),
    IText = wxTextCtrl:new(Frame, -1, [{style, 32},{size, {100,50}}]),
        wxTextCtrl:setName(IText, "info"),
        wxSizer:add(TextSizer, IText, [{proportion,1},{flag,?wxEXPAND}]),
    
    CStat = wxStaticText:new(Frame, -1, "Source code:", []),
        wxStaticText:setName(CStat, "cstat"),
        wxSizer:add(TextSizer, 0, 10, [{proportion,0},{flag,?wxEXPAND}]),
        wxSizer:add(TextSizer, CStat, [{proportion,0},{flag,?wxEXPAND}]),
        wxSizer:add(TextSizer, 0, 10, [{proportion,0},{flag,?wxEXPAND}]),
    CText = wxTextCtrl:new(Frame, -1, [{style, 32},{size, {100,50}}]),
        wxTextCtrl:setName(CText, "code"),
        wxSizer:add(TextSizer, CText, [{proportion,1},{flag,?wxEXPAND}]),
    
    
    wxSizer:add(MainSizer, TreeSizer, 
        [{proportion,0},{flag,?wxEXPAND bor ?wxALL},{border,10}]),
    wxSizer:add(MainSizer, TextSizer, 
        [{proportion,1},{flag,?wxEXPAND bor ?wxALL},{border,10}]),
    wxFrame:setSizer(Frame, MainSizer),

    wxFrame:setToolBar(Frame,TB),

    wxFrame:show(Frame),

    add_tool_menus(),

    window(?Sup:root(), []).

window(Root, History) ->
    Children = ?Sup:children(Root),

    Frame = wx:typeCast(wxWindow:findWindowByName("win"), wxFrame),
    wxFrame:setTitle(Frame, title_text(Root, History)),

    add_refac_menus(Root),
    add_tool_buttons(Root),

    draw_tree({Root, Children}),
    write_info(Root),
    write_text(Root),
    loop(Root, History).

-define(P(Number, Percent), trunc(Number * Percent)).

loop(Root, History) ->
    receive
    {wx, _, _, _, {wxClose, close_window}} ->
        stop(),
        stopped;
    {wx, _, _, Datas, {wxCommand, command_button_clicked, _,_,_}} ->
        case Datas of
            {back,_,_}->
                case History of
                    [] -> loop(Root, History);
                    [Parent|Tail] ->
                        destroy_children(),
                        window(Parent,Tail)
                end;
            {new,Tag,R}->
                destroy_children(),
                window(Tag,[R|History]);
            {cancel, TrFrame} ->
                wxWindow:destroy(TrFrame),
                Frame = wxWindow:findWindowByName("win"),
                wxFrame:enable(Frame),
                loop(Root, History);
            {ok, TrFrame, Tr}->
                Args = tr_ok(Tr) ++
                    case Tr#tr.label of
                        "Run query" -> [{root, Root}];
                        _ -> []
                    end,
                wxWindow:destroy(TrFrame),
                Frame = wxWindow:findWindowByName("win"),
                wxFrame:enable(Frame),
                do_transform(Tr#tr.kind, Tr#tr.func, Args),
                {New, NewHistory} = reload(Tr#tr.back, Root, History),
                window(New, NewHistory);
            {optionscancel, Pref} -> 
                wxWindow:destroy(Pref),
                Frame = wxWindow:findWindowByName("win"),
                wxWindow:enable(Frame, [{enable,true}]),
                loop(Root, History);
            {optionsok, Pref, OpList} -> 
                List = [{Id, lists:flatten(
                        [case wxCheckBox:isChecked(CheckItem) of 
                            true -> Index; 
                            false -> [] 
                            end 
                        || {CheckItem,Index}<-lists:zip(CheckList,
                            ?MISC:seq2(1,length(CheckList)))])} 
                    || {Id,CheckList}<-OpList],
                ?Config:set_options(List),
                wxWindow:destroy(Pref),
                Frame = wxWindow:findWindowByName("win"),
                wxWindow:enable(Frame, [{enable,true}]),
                add_tool_buttons(Root),
                loop(Root, History);
            {error, DErr}->
                wxDialog:destroy(DErr),
                loop(Root, History)
        end;
    {wx, MenuID, _, _, {wxCommand, command_menu_selected, _, _, _}} ->
        case MenuID of
            ?wxID_EXIT -> stop();
            ?wxID_PREFERENCES -> open_preferences(),
                loop(Root, History);
            _ -> Tr = 
                if
                    MenuID < 100 -> lists:nth(MenuID, ?Sup:tool_buttons());
                    true -> lists:nth(MenuID-100, ?Sup:tr_buttons(Root))
                end,
                if
                    Tr#tr.label /= "Refresh" -> tr_window(Tr);
                    true -> void
                end,
                case Tr#tr.map of
                    false ->
                        {New, NewHistory} = reload(Tr#tr.back, Root, History),
                        destroy_children(),
                        window(New, NewHistory);
                    true -> loop(Root, History)
                end
        end;
    {wx, _, _, _, {wxScrollWin, scrollwin_pageup}} ->
        TPanel = wx:typeCast(wxWindow:findWindowByName("ptree"), wxPanel),
        Pos = wxWindow:getScrollPos(TPanel, ?wxVERTICAL),
        scroll_tree({Root, ?Sup:children(Root)}, -1*Pos),
        loop(Root, History);
    {wx, _, _, _, {wxScrollWin, scrollwin_pagedown}} ->
        TPanel = wx:typeCast(wxWindow:findWindowByName("ptree"), wxPanel),
        Pos = wxWindow:getScrollPos(TPanel, ?wxVERTICAL),
        scroll_tree({Root, ?Sup:children(Root)}, -1*Pos),
        loop(Root, History);
    {wx, _, _, _, {wxScrollWin, scrollwin_thumbtrack}} ->
        TPanel = wx:typeCast(wxWindow:findWindowByName("ptree"), wxPanel),
        Pos = wxWindow:getScrollPos(TPanel, ?wxVERTICAL),
        scroll_tree({Root, ?Sup:children(Root)}, -1*Pos),
        loop(Root, History);
    {wx, _, _, P, {wxSize, size, {_, H}, _}} ->
        TPanel = wxWindow:findWindowByName("ptree"),
        if
            P*60+80 > H ->
                wxWindow:setScrollbar(TPanel, ?wxVERTICAL, 0, H, (P*60+80));
            true -> 
                wxWindow:refresh(TPanel)
        end,
        loop(Root, History);
    reload ->
        {New, NewHistory} = reload(false, Root, History),
        window(New, NewHistory);
    Msg ->
        ?DebugThis(Msg),
        loop(Root, History)
    end.

terminate() ->
    wx:destroy(),
    watch_stop(),
    ?Config:stop(),
    unregister(?Server).

reload(undo, _, _) ->
    {?Sup:root(), []};
reload(Back, Root, History) ->
    case Back of
        true -> Now = [];
        false -> Now = [Root]
    end,
    destroy_children(),
    ?Sup:find_node(Now ++ History).

destroy_children() ->
    IText = wx:typeCast(wxWindow:findWindowByName("info"), wxTextCtrl),
    wxTextCtrl:clear(IText),
    CText = wx:typeCast(wxWindow:findWindowByName("code"), wxTextCtrl),
    wxTextCtrl:clear(CText),

    TPanel = wxWindow:findWindowByName("ptree"),
    wxWindow:destroyChildren(TPanel).

-define(u(UD), case UD of undefined -> ""; UDElse -> ?MISC:to_list(UDElse) end).

title_text(Tag, History) ->
    lists:flatten([ ?MISC:to_list(X#tag.class) ++ "(" ++
        ?u(X#tag.name) ++ ") / " ||
            X <- lists:reverse(History) ]) ++
    ?MISC:to_list(Tag#tag.class) ++ "(" ++
    ?u(Tag#tag.name) ++ ")".

tr_window(Tr) ->
    case Tr#tr.map of
    true ->
        Height = 40 + length(Tr#tr.prop) * 40 + 40,
        Frame = wxWindow:findWindowByName("win"),
        TrFrame = wxFrame:new(Frame, -1, Tr#tr.label, 
            [{pos,{200,300}},
             {size,{400,Height}},
             {style,?wxFRAME_FLOAT_ON_PARENT}]),
        wxStaticText:new(TrFrame, -1, Tr#tr.desc, 
            [{pos,{10,10}},
             {size,{300,30}}]),
        lists:foreach(
            fun({{entry, Name, Desc}, X}) ->
                L = length(Desc)*7,
                wxStaticText:new(TrFrame, -1, Desc, 
                  [{pos,{10,40+(X-1)*40}},{size,{L,20}}]),
                TC = wxTextCtrl:new(TrFrame, -1, 
                    [{pos,{L+20,40+(X-1)*40}},{size,{200,-1}}]),
                wxTextCtrl:setName(TC, 
                    if is_atom(Name)->atom_to_list(Name); true->Name end);
            (_) -> void
            end, lists:zip(Tr#tr.prop, ?MISC:seq2(1, length(Tr#tr.prop)))),
        Y = 40 + length(Tr#tr.prop) * 40,
        OButt = wxButton:new(TrFrame, -1, [{label,"OK"},{pos,{10,Y}}]),
        wxButton:connect(OButt, command_button_clicked, 
            [{userData, {ok, TrFrame, Tr}}]),
        CButt = wxButton:new(TrFrame, -1, [{label,"Cancel"},{pos,{130,Y}}]),
        wxButton:connect(CButt, command_button_clicked, 
            [{userData, {cancel,TrFrame}}]),
        wxFrame:show(TrFrame),
        wxFrame:makeModal(TrFrame);
    false ->
        do_transform(Tr#tr.kind, Tr#tr.func, [])
    end.

tr_ok(Tr) ->
    _Args = lists:map(
        fun({entry, Name, _}) ->
            {Name, wxTextCtrl:getValue(
                       wx:typeCast(
                           wxWindow:findWindowByName(
                               if 
                                   is_atom(Name)->atom_to_list(Name); 
                                   true->Name 
                               end), 
                           wxTextCtrl))}
        end, Tr#tr.prop).

do_transform(undo, Fun, Args) ->
    ReqID = Fun(Args),
    receive
        {ReqID, reply, R} -> R
    end;
do_transform(transform, Fun, Args) ->
    ReqID = Fun(Args),
    receive
        {ReqID, reply, R} ->
            case R of
                {error, {_Code, Msg}} ->
                    infowindow(Msg, "ERROR"),
                    error;
                {ok, {abort,{_Code, Msg}}} ->
                    infowindow(Msg, "ABORT");
                {ok, {result, [{result, Result}]}} ->
                    case lists:keysearch(querystr, 1, Args) of
                        false -> Result;
                        {value,{querystr, QueryStr}} -> 
                            open_miniframe(Result, QueryStr)
                    end;
                _ ->
                    R
            end
    end.

open_preferences() ->
    Frame = wxWindow:findWindowByName("win"),
    wxWindow:enable(Frame, [{enable, false}]),
    Pref = wxFrame:new(Frame, ?wxID_ANY, "Preferences", [{size, {250,350}}]),

    Sizer = wxFlexGridSizer:new(2, []),

    ToolBox = add_option_group(Sizer, Pref, tool, "Tool"),
    FunBox = add_option_group(Sizer, Pref, func, "Function"),
    ModBox = add_option_group(Sizer, Pref, module, "Module"),
    VarBox = add_option_group(Sizer, Pref, variable, "Variable"),
    MacBox = add_option_group(Sizer, Pref, macro, "Macro"),
    ImpBox = add_option_group(Sizer, Pref, implicit_fun, "Implicit function"),
    AppBox = add_option_group(Sizer, Pref, application, "Application"),

    BOk = wxButton:new(Pref, ?wxID_ANY, [{label,"OK"}]),
    wxSizer:add(Sizer, BOk),
    wxButton:connect(BOk, command_button_clicked, [{userData, {optionsok, Pref, 
        [{tool,ToolBox},{func,FunBox},{module,ModBox},{variable,VarBox},
        {macro,MacBox},{implicit_fun,ImpBox},{application,AppBox}]}}]),
    BCancel = wxButton:new(Pref, ?wxID_ANY, [{label,"Cancel"}]),
    wxSizer:add(Sizer, BCancel),
    wxButton:connect(BCancel, command_button_clicked, 
        [{userData, {optionscancel,Pref}}]),

    wxFrame:setSizer(Pref, Sizer),
    wxFrame:show(Pref).

add_option_group(Sizer, Panel, Option, Label) ->
    OptionSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, Label}]),
    OptionBox = 
        [wxCheckBox:new(Panel, ?wxID_ANY, Item, []) 
        || Item<-?Config:options(Option)],
    lists:foreach(
        fun(Elem) -> 
            wxSizer:add(OptionSizer, Elem, 
                [{proportion, 1},{flag, ?wxEXPAND}]) 
        end, OptionBox),
    wxSizer:add(Sizer, OptionSizer, [{proportion, 1},{flag, ?wxEXPAND}]),
    lists:foreach(
        fun(Item) -> 
            wxCheckBox:setValue(lists:nth(Item, OptionBox), true) 
        end, ?Config:toolbar(Option)),
    OptionBox.

open_miniframe(Result, QueryStr) ->
    Frame = wxWindow:findWindowByName("win"),
    MiniSizer = wxBoxSizer:new(?wxVERTICAL),
    MiniFrame = wxMiniFrame:new(Frame, ?wxID_ANY, 
        "Query result: "++QueryStr, 
        [{style,?wxDEFAULT_FRAME_STYLE}]),
    MiniPanel = wxPanel:new(MiniFrame, []),
    TC = wxTextCtrl:new(MiniPanel, ?wxID_ANY, 
        [{size,{100, 150}},
         {style,?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxHSCROLL}]),
    wxSizer:add(MiniSizer, TC, 
        [{proportion,1},{flag,?wxEXPAND}]),
    wxPanel:setSizer(MiniPanel, MiniSizer),
    wxTextCtrl:setValue(TC, Result),
    wxMiniFrame:show(MiniFrame).

infowindow(ERROR, Mode) ->
    DErr = wxMessageDialog:new(
              wxWindow:findWindowByName("win"), 
              ERROR, 
              [{caption, Mode},{pos, {-1,-1}},{style, ?wxOK}]),
    wxDialog:showModal(DErr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scroll_tree(Tree, Diff) ->
    {Root, Children} = Tree,

    TPanel = wx:typeCast(wxWindow:findWindowByName("ptree"), wxPanel),

    wxWindow:destroyChildren(TPanel),

    OnPaint = fun(_Evt, _Obj) ->
        Paint = wxPaintDC:new(TPanel),
        Pen = wxPen:new({0,0,0}),
        wxPen:setWidth(Pen, 2),
        wxDC:setPen(Paint, Pen),
        lists:foreach(
            fun(Y) ->
                P = Y * 60,
                wxDC:drawLine(Paint, {30, -30+P+Diff}, {30, 35+P+Diff}),
                wxDC:drawLine(Paint, {30, 35+P+Diff}, {85, 35+P+Diff})
            end,
            ?MISC:seq2(1, length(Children))),
        wxPaintDC:destroy(Paint)
    end,
    wxPanel:connect(TPanel, paint, [{callback,OnPaint}]),
    draw_node(Root, Root, 20, 20+Diff, back),
    lists:foreach(
        fun({Name, Y}) ->
            draw_node(Root, Name, 80, Y + 20+Diff, new)
        end,
        lists:zip(Children, [ Z * 60 || Z <- ?MISC:seq2(1, length(Children))])),
    
    wxPanel:refresh(TPanel).

draw_tree(Tree) ->
    {Root, Children} = Tree,
    ChildNum = length(Children),

    TPanel = wx:typeCast(wxWindow:findWindowByName("ptree"), wxPanel),
    {_,H} = wxPanel:getSize(TPanel),

    wxPanel:connect(TPanel, size, [{userData, ChildNum}]),

    wxWindow:setScrollbar(TPanel, ?wxVERTICAL, 0, H, (ChildNum*60+80)),

    OnPaint = fun(_Evt, _Obj) ->
        Paint = wxPaintDC:new(TPanel),
        Pen = wxPen:new({0,0,0}),
        wxPen:setWidth(Pen, 2),
        wxDC:setPen(Paint, Pen),
        lists:foreach(
            fun(Y) ->
                P = Y * 60,
                wxDC:drawLine(Paint, {30, -30+P}, {30, 35+P}),
                wxDC:drawLine(Paint, {30, 35+P}, {85, 35+P})
            end,
            ?MISC:seq2(1, length(Children))),
        wxPaintDC:destroy(Paint)
    end,
    wxPanel:connect(TPanel, paint, [{callback,OnPaint}]),
    draw_node(Root, Root, 20, 20, back),
    lists:foreach(
        fun({Name, Y}) ->
            draw_node(Root, Name, 80, Y + 20, new)
        end,
        lists:zip(Children, [ Z * 60 || Z <- ?MISC:seq2(1, length(Children))])),
    
    wxPanel:refresh(TPanel).

-define(text_length(String), length(String) * 8 + 14).

draw_node(Root, Tag, X, Y, To) ->
    Order = ?Config:order(Tag#tag.class),
    TagList = ?MISC:record_to_proplist(Tag, record_info(fields, tag)),
    OList = [ proplists:lookup(T, TagList) || T <- Order ],
    
    TPanel = wx:typeCast(wxWindow:findWindowByName("ptree"), wxPanel),
    
    Pic = wxBitmap:new(getButtWidth(OList),39),
    DC = wxMemoryDC:new(),
    wxMemoryDC:selectObject(DC, Pic),
    draw_recs(7, 0, To, OList, Tag, Pic, DC),
    wxMemoryDC:destroy(DC),

    BButt = wxBitmapButton:new(TPanel, -1, Pic, 
                [{pos,{X,Y}},{style,2097152},{size,{getButtWidth(OList),39}}]),
    wxEvtHandler:connect(BButt, command_button_clicked, 
        [{userData, {To,Tag, Root}}]).

getButtWidth([])->
    13;
getButtWidth([{_,undefined}|OList])->
    getButtWidth(OList);
getButtWidth([{Attr,Text}|OList])->
    RecWidth = case Attr of
        name -> ?text_length(Text);
        _ -> ?Config:size(Attr)
    end,
    RecWidth + getButtWidth(OList).

draw_recs(_, _, _, [], _, _, _) -> done;
draw_recs(X, Y, To, [{_, undefined} | OList], Tag, Pic, DC) ->
    draw_recs(X, Y, To, OList, Tag, Pic, DC);
draw_recs(X, Y, To, [{Attr, Text} | OList], Tag, Pic, DC) ->
    {BG, FG} = ?Config:color(Attr, Text),
    RecWidth = case Attr of
                name -> ?text_length(Text);
                _ -> ?Config:size(Attr)
               end,
    wxDC:setBackground(DC, wxBrush:new(BG)),
    wxDC:setBrush(DC, wxBrush:new(BG)),
    Pen = wxPen:new({0,0,0}),
    wxPen:setWidth(Pen, 2),
    wxDC:setPen(DC, Pen),

    wxDC:drawRectangle(DC, {X,Y+7,RecWidth,26}),
        
    wxDC:setTextForeground(DC, FG),
    wxDC:setPen(DC, wxPen:new(FG)),
    wxDC:setFont(DC, wxFont:new(8, 70, 90, 90)),

    wxDC:drawText(DC, 
                  if 
                    is_atom(Text) -> atom_to_list(Text); 
                    is_integer(Text) -> integer_to_list(Text); 
                    true -> Text 
                  end, 
                  {X+5, Y+14}),

    draw_recs(X + RecWidth, Y, To, OList, Tag, Pic, DC).

write_info(Tag) ->
    TAttrType = wxTextAttr:new({255,0,0}),
    TAttrText = wxTextAttr:new({0,0,0}),
    IText = wx:typeCast(wxWindow:findWindowByName("info"), wxTextCtrl),

    lists:foreach(
        fun({X,Y})->
            wxTextCtrl:setDefaultStyle(IText,TAttrType),
            wxTextCtrl:appendText(IText, info_type(X)++": "),
            wxTextCtrl:setDefaultStyle(IText, TAttrText),
            wxTextCtrl:appendText(IText, info_text(Y)++"\n")
        end,
        ?Sup:info(Tag)
    ),

    wxTextCtrl:setEditable(IText, false).

info_type(name) -> "Name";
info_type(arity) -> "Arity";
info_type(exported) -> "Exported";
info_type(clausenum) -> "Number of Clauses";
info_type(file) -> "Location";
info_type(functions) -> "Functions";
info_type(files) -> "Number of files";
info_type(macros) -> "Macros";
info_type(records) -> "Records";
info_type(fieldnum) -> "Number of fields";
info_type(fieldnames) -> "Fields";
info_type(hasguard) -> "Has guard";
info_type(linenum) -> "Number of lines";
info_type(varbindingnum) -> "Number of bindings";
info_type(varrefnum) -> "Number of references";
info_type(_) -> "Unknown".

info_text({string, String}) -> String;
info_text({list, List}) -> string:join(List, ", ").

write_text(Tag) ->
    Text = lists:flatten(?Sup:text(Tag)),
    CText = wx:typeCast(wxWindow:findWindowByName("code"), wxTextCtrl),
    wxTextCtrl:setValue(CText, Text),
    wxTextCtrl:setEditable(CText, false).

watch_start() ->
    case whereis(?Watch) of
        undefined ->
            Pid = spawn(fun() -> watch_loop() end),
            register(?Watch, Pid),
            Pid;
        Pid -> Pid
    end.

watch_stop() ->
    ?Watch ! stop,
    unregister(?Watch).

watch_loop() ->
    receive
    stop ->
        stopped;
    {Type, _} when Type == add orelse Type == reload ->
        ?Server ! reload,
        watch_loop();
    {status, Status} ->
        ?Server ! {status, Status},
        watch_loop();
    {trfinished, _} ->
        ?Server ! trfinished,
        watch_loop();
    {uifinished, undo} ->
        ?Server ! {uifinished, undo},
        watch_loop();
    {uifinished, reset} ->
        ?Server ! {uifinished, reset},
        watch_loop();
    _ ->
        watch_loop()
    end.

add_refac_menus(Tag) ->
    TrButtons = ?Sup:tr_buttons(Tag),

    Frame = wx:typeCast(wxWindow:findWindowByName("win"), wxFrame),
    MB = wxFrame:getMenuBar(Frame),
    Refac = wxMenuBar:getMenu(MB, 1),

    [ wxMenu:delete(Refac, MI) || MI<-wxMenu:getMenuItems(Refac) ],

    case TrButtons of
        [] -> wxMenuBar:enableTop(MB, 1, false);
        _ -> 
            add_menu_items(TrButtons, Refac, 100),
            wxMenuBar:enableTop(MB, 1, true)
    end.

add_tool_menus() ->
    ToolButtons = ?Sup:tool_buttons(),

    Frame = wx:typeCast(wxWindow:findWindowByName("win"), wxFrame),
    MB = wxFrame:getMenuBar(Frame),
    File = wxMenuBar:getMenu(MB, 0),

    add_menu_items(ToolButtons, File, 0),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?wxID_PREFERENCES, "Options"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?wxID_EXIT, "Exit").

add_menu_items(Items, Menu, Index) ->
    lists:foreach(
        fun({B, X}) ->
            wxMenu:append(Menu, Index+X, B#tr.label)
        end, lists:zip(Items, ?MISC:seq2(1, length(Items)))
    ).

add_tool_buttons(Tag) ->
    Frame = wx:typeCast(wxWindow:findWindowByName("win"), wxFrame),
    wxWindow:destroy(wxFrame:getToolBar(Frame)),
    TB = wxFrame:createToolBar(Frame,[]),

    VisibleTools = ?Config:toolbar(tool),
    Tools = ?Sup:tool_buttons(),

    lists:foreach(
        fun(Item) -> 
            B = lists:nth(Item, Tools),
            if
                B#tr.image /= "" ->
                    wxToolBar:addTool(TB, Item, B#tr.label,
                        wxBitmap:new(
                            string:concat("./lib/referl_ui/wx/",
                                B#tr.image),
                            [{type,?wxBITMAP_TYPE_JPEG}]),
                        [{shortHelp,B#tr.label}]);
                true -> void
            end
        end, 
        VisibleTools),

    VisibleRefacs = 
        case Tag#tag.class of
            expr -> ?Config:toolbar(Tag#tag.type);
            _ -> ?Config:toolbar(Tag#tag.class)
        end,
    Refacs = ?Sup:tr_buttons(Tag),
    
    case VisibleRefacs of
        [] -> void;
        _ -> wxToolBar:addSeparator(TB)
    end,

    lists:foreach(
        fun(Item) -> 
            B = lists:nth(Item, Refacs),
            if
                B#tr.image /= "" ->
                    wxToolBar:addTool(TB, Item+100, B#tr.label,
                        wxBitmap:new(
                            string:concat("./lib/referl_ui/wx/",
                                B#tr.image),
                            [{type,?wxBITMAP_TYPE_JPEG}]),
                        [{shortHelp,B#tr.label}]);
                true -> void
            end
        end, 
        VisibleRefacs),

    wxToolBar:realize(TB),
    wxFrame:refresh(Frame).
