-module(refusr_sq_lib_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("referl_gen/include/gen_export.hrl").
-include_lib("referl_core/include/core_global.hrl").
-include_lib("referl_lib/include/lib_export.hrl").


-include_lib("referl_core/include/core_export.hrl").


-define(Lib, refusr_sq_lib).

add_file({Type, Name, Text}) ->
    File = ?ESG:create(#file{path=Name, type=Type, eol={lf,eol},lastmod=now()}),
    ?ESG:insert(?ESG:root(), file, File),
    ?Graph:mklink(File, incl, File),
    ?FileMan:add_text(File, last, Text),
    ?ESG:finalize(),
    File.

%% Initial selectors
init_sel_testfile() ->
    {module, ?MISC:canonical_filename("init_sel_test.erl"),
     "-module(init_sel_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

init_sel_test() ->
    Filename = ?MISC:canonical_filename("init_sel_test.erl"),
    {setup,
     add_file(init_sel_testfile()),
     fun ?FileMan:drop_file/1 ,
     [?assertEqual(refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 34}], '@def'),
                   refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 34},
                      {ask_missing, false}], '@rec')),
      ?assertEqual(refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 161}], '@def'),
                   refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 161},
                      {ask_missing, false}], '@rec')),
      ?assertEqual(refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 40}], '@def'),
                   refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 40},
                      {ask_missing, false}], '@field')),
      ?assertEqual(refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 144}], '@def'),
                   refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 144},
                      {ask_missing, false}], '@field')),
      ?assertEqual(refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 59}], '@def'),
                   refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 59},
                      {ask_missing, false}], '@macro')),
       ?assertEqual(refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 114}], '@def'),
                   refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 114},
                      {ask_missing, false}], '@macro')),
      ?assertEqual(refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 139}], '@def'),
                   refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 139},
                      {ask_missing, false}], '@var')),
      ?assertEqual(refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 74}], '@def'),
                   refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 74},
                      {ask_missing, false}], '@mod')),
      ?assertEqual(refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 79}], '@def'),
                   refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 79},
                      {ask_missing, false}], '@fun')),
      ?assertEqual(refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 65}], '@def'),
                   refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 65},
                      {ask_missing, false}], '@fun')),
      ?assertEqual(refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 116}], '@def'),
                   refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 116},
                      {ask_missing, false}], '@fun')),
      ?assertEqual(refusr_sq_lib:init_sel(
                     [{file, Filename}, {position, 24}], '@def'),
                   {none, []})]}.


file_testfile() ->
    {module, ?MISC:canonical_filename("file_test.erl"),
     "-module(file_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

file_test() ->
    Filename = ?MISC:canonical_filename("file_test.erl"),
    {setup,
     add_file(file_testfile()),
     fun(File) -> ?FileMan:drop_file(File) end,
     [?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file"),
                   [{list,
                     [{{Filename,1,1},"file_test.erl"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs"),
                   [{group_by,{nopos,"file_test.erl"},
                    list,
                     [{{Filename,61,97},"file_test:f/1"},
                      {{Filename,99,119},"file_test:g/1"},
                      {{Filename,121,160},"file_test:h/1"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.recs"),
                   [{group_by,{nopos,"file_test.erl"},
                    list,[{{Filename,21,45},"rec"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.macros"),
                   [{group_by,{nopos,"file_test.erl"},
                     list,[{{Filename,47,59},"F"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.includes"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     list,
                     [{{Filename,1,1},"file_test.erl"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.included_by"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     list,
                     [{{Filename,1,1},"file_test.erl"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.imports"),
                   []),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.exports"),
                   []),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.module"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,module,true}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.header"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                    eq,header,false}]),
%atom vs string???
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.name"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,name,file_test}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.dir"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,dir,filename:dirname(Filename)}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.path"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,path,Filename}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.mod_sum"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,mod_sum,239}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.loc"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,loc,8}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.choc"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,choc,161}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.num_of_fun"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,num_of_fun,3}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.num_of_macros"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,num_of_macros,1}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.num_of_records"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,num_of_records,1}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.included_files"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,included_files,0}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.imported_modules"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,imported_modules,0}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.number_of_funpath"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,number_of_funpath,3}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.fun_calls_in"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,fun_calls_in,0}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.fun_calls_out"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,fun_calls_out,1}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.cohesion"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,cohesion,2}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.otp"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,otp,0}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.max_app_depth"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,max_app_depth,2}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.max_depth_of_calling"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,max_depth_of_calling,2}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.min_depth_of_calling"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,min_depth_of_calling,1}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.max_depth_of_cases"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,max_depth_of_cases,0}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.num_of_funclauses"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,num_of_funclauses,3}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.branches_of_recursion"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,branches_of_recursion,0}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.mccabe"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,mccabe,3}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.num_of_funexpr"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,num_of_funexpr,0}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.number_of_messpass"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,number_of_messpass,0}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.fun_return_points"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,fun_return_points,3}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.max_length_of_line"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,max_length_of_line,26}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.avg_length_of_line"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,avg_length_of_line,19}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.no_space_after_comma"),
                   [{group_by,
                     {{Filename,1,1},"file_test.erl"},
                     eq,no_space_after_comma,3}])
     ]}.


fun_testfile() ->
    {module, ?MISC:canonical_filename("fun_test.erl"),
     "-module(fun_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

fun_test() ->
    Filename = ?MISC:canonical_filename("fun_test.erl"),
    {setup,
     add_file(fun_testfile()),
     fun(File) -> ?FileMan:drop_file(File) end,
     [?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs.refs"),
                   [{group_by,{nopos,"fun_test:f/1"},
                     list,[{{Filename,108,117},"?F(h(Rec))"}]},
                    {group_by,{nopos,"fun_test:h/1"},
                     list,[{{Filename,111,116},"h(Rec)"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs.calls"),
                   [{group_by,{{Filename,60,96},"fun_test:f/1"},
                     list,[{nopos,"io:format/1"}]},
                    {group_by,{nopos,"fun_test:g/1"},
                     list,
                     [{{Filename,60,96},"fun_test:f/1"},
                      {{Filename,120,159},"fun_test:h/1"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs.called_by"),
                   [{group_by,{nopos,"fun_test:f/1"},
                     list,[{{Filename,98,118},"fun_test:g/1"}]},
                    {group_by,{nopos,"fun_test:h/1"},
                     list,[{{Filename,98,118},"fun_test:g/1"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs.args"),
                   [{group_by,{nopos,"fun_test:f/1"},
                     list,[{{Filename,62,62},"X"}]},
                    {group_by,{nopos,"fun_test:g/1"},
                     list,[{{Filename,100,102},"Rec"}]},
                    {group_by,{nopos,"fun_test:h/1"},
                     list,[{{Filename,122,124},"Rec"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==f].body"),
                   [{group_by,{nopos,"fun_test:f/1"},
                     list,
                     [{{Filename,68,84},"io:format(\"alma\")"},
                      {{Filename,95,95},"X"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==f].exprs"),
                   [{group_by,{nopos,"fun_test:f/1"},
                     list,
                     [{{Filename,62,62},"X"},
                      {{Filename,68,84},"io:format(\"alma\")"},
                      {{Filename,95,95},"X"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==f].vars"),
                   [{group_by,{nopos,"fun_test:f/1"},
                     list,
                     [{{Filename,62,62},"X"}]}]),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.exported")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,exported,true},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,exported,false},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,exported,false}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.name")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,name,format},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,name,f},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,name,h}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.arity")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,arity,1},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,arity,1},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,arity,1}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.bif")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,bif,false},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,bif,false},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,bif,false}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.pure")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,pure,false},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,pure,false},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,pure,true}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.defined")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,defined,false},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,defined,true},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,defined,true}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.dirty")),
                   lists:usort(
                     [{group_by,{nopos,"io:format/1"},
                       eq,dirty,true},
                      {group_by,{{Filename,60,96},"fun_test:f/1"},
                       eq,dirty,true},
                      {group_by,{{Filename,120,159},"fun_test:h/1"},
                       eq,dirty,false}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.mod")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,mod,io},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,mod,fun_test},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,mod,fun_test}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.exported")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,exported,true},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,exported,false},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,exported,false}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.loc")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,loc,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,loc,2},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,loc,2}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.choc")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,choc,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,choc,36},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,choc,39}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.fun_sum")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,fun_sum,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,fun_sum,9},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,fun_sum,9}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.max_application_depth")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,max_application_depth,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,max_application_depth,1},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,max_application_depth,0}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.max_depth_of_calling")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,max_depth_of_calling,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,max_depth_of_calling,1},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,max_depth_of_calling,0}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.max_depth_of_cases")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,max_depth_of_cases,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,max_depth_of_cases,0},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,max_depth_of_cases,0}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.number_of_funclauses")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,number_of_funclauses,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,number_of_funclauses,1},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,number_of_funclauses,1}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.branches_of_recursion")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                     eq,branches_of_recursion,0},
                    {group_by,{{Filename,60,96},"fun_test:f/1"},
                     eq,branches_of_recursion,0},
                    {group_by,{{Filename,120,159},"fun_test:h/1"},
                     eq,branches_of_recursion,0}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.mccabe")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,mccabe,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,mccabe,1},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,mccabe,1}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.calls_for_function")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,calls_for_function,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,calls_for_function,1},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,calls_for_function,1}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.calls_from_function")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,calls_from_function,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,calls_from_function,1},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,calls_from_function,0}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.number_of_funexpr")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,number_of_funexpr,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,number_of_funexpr,0},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,number_of_funexpr,0}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.number_of_messpass")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,number_of_messpass,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,number_of_messpass,0},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,number_of_messpass,0}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.fun_return_points")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,fun_return_points,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,fun_return_points,1},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,fun_return_points,1}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.max_length_of_line")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,max_length_of_line,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,max_length_of_line,26},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,max_length_of_line,21}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.average_length_of_line")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,average_length_of_line,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,average_length_of_line,18},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,average_length_of_line,19}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.no_space_after_comma")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,no_space_after_comma,0},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,no_space_after_comma,0},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,no_space_after_comma,0}])),
      ?assertEqual(lists:usort(
                     refusr_sq:run(
                       [{positions, scalar},{output, other}],
                       [{file, Filename}, {position, 34}],
                       "@file.funs.calls.is_tail_recursive")),
                   lists:usort([{group_by,{nopos,"io:format/1"},
                                 eq,is_tail_recursive,unknown},
                                {group_by,{{Filename,60,96},"fun_test:f/1"},
                                 eq,is_tail_recursive,non_rec},
                                {group_by,{{Filename,120,159},"fun_test:h/1"},
                                 eq,is_tail_recursive,non_rec}]))
     ]}.

var_testfile() ->
    {module, ?MISC:canonical_filename("var_test.erl"),
     "-module(var_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

var_test() ->
    Filename = ?MISC:canonical_filename("var_test.erl"),
    {setup,
     add_file(var_testfile()),
     fun(File) -> ?FileMan:drop_file(File) end,
     [?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==h].vars"),
                   [{group_by,{nopos,"var_test:h/1"},
                     list,[{{Filename,122,124},"Rec"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==h].vars.refs"),
                   [{group_by,{nopos,"Rec"},
                     list,[{{Filename,130,132},"Rec"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==h].vars.bindings"),
                   [{group_by,{nopos,"Rec"},
                     list,[{{Filename,122,124},"Rec"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==h].vars.fundef"),
                   [{group_by,{nopos,"Rec"},
                     list,[{{Filename,120,159},"var_test:h/1"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==h].vars.name"),
                   [{group_by,{{Filename,122,124},"Rec"},
                     eq,name,"Rec"}])
     ]}.

rec_testfile() ->
    {module, ?MISC:canonical_filename("rec_test.erl"),
     "-module(rec_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

rec_test() ->
    Filename = ?MISC:canonical_filename("rec_test.erl"),
    {setup,
     add_file(rec_testfile()),
     fun(File) -> ?FileMan:drop_file(File) end,
     [?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.recs"),
                   [{group_by,{nopos,"rec_test.erl"},
                     list,[{{Filename,20,44},"rec"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.recs.refs"),
                   [{group_by,{nopos,"rec"},
                     list,
                     [{{Filename,130,139},"Rec#rec.f2"},
                      {{Filename,152,158},"#rec.f1"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.recs.fields"),
                   [{group_by,{nopos,"rec"},
                     list,
                     [{{Filename,20,44},"f1"},
                      {{Filename,20,44},"f2"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.recs.file"),
                   [{group_by,{nopos,"rec"},
                     list,[{{Filename,1,1},"rec_test.erl"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.recs.name"),
                   [{group_by,{{Filename,20,44},"rec"},
                     eq,name,rec}])
     ]}.

recfield_testfile() ->
    {module, ?MISC:canonical_filename("recfield_test.erl"),
     "-module(recfield_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

recfield_test() ->
    Filename = ?MISC:canonical_filename("recfield_test.erl"),
    {setup,
     add_file(recfield_testfile()),
     fun(File) -> ?FileMan:drop_file(File) end,
     [?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.recs.fields.refs"),
                   [{group_by,{nopos,"f1"},
                     list,[{{Filename,162,163},"f1"}]},
                    {group_by,{nopos,"f2"},
                     list,[{{Filename,143,144},"f2"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.recs.fields.rec"),
                   [{group_by,{nopos,"f1"},
                     list,[{{Filename,25,49},"rec"}]},
                    {group_by,{nopos,"f2"},
                     list,[{{Filename,25,49},"rec"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.recs.fields.file"),
                   [{group_by,{nopos,"f1"},
                     list,[{{Filename,1,1},"recfield_test.erl"}]},
                    {group_by,{nopos,"f2"},
                     list,[{{Filename,1,1},"recfield_test.erl"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.recs.fields.name"),
                   [{group_by,{{Filename,25,49},"f1"},
                     eq,name,f1},
                    {group_by,{{Filename,25,49},"f2"},
                     eq,name,f2}])
     ]}.

macro_testfile() ->
    {module, ?MISC:canonical_filename("macro_test.erl"),
     "-module(macro_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

macro_test() ->
    Filename = ?MISC:canonical_filename("macro_test.erl"),
    {setup,
     add_file(macro_testfile()),
     fun(File) -> ?FileMan:drop_file(File) end,
     [?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.macros"),
                   [{group_by,{nopos,"macro_test.erl"},
                     list,[{{Filename,48,60},"F"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.macros.refs"),
                   [{group_by,{nopos,"F"},
                     list,[{{Filename,110,111},"?F"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.macros.file"),
                   [{group_by,{nopos,"F"},
                     list,[{{Filename,1,1},"macro_test.erl"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.macros.name"),
                   [{group_by,{{Filename,48,60},"F"},
                     eq,name,"F"}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.macros.arity"),
                   [{group_by,{{Filename,48,60},"F"},
                     eq,arity,0}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.macros.const"),
                   [{group_by,{{Filename,48,60},"F"},
                     eq,const,true}])
     ]}.

expr_testfile() ->
    {module, ?MISC:canonical_filename("expr_test.erl"),
     "-module(expr_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

expr_test() ->
    Filename = ?MISC:canonical_filename("expr_test.erl"),
    {setup,
     add_file(expr_testfile()),
     fun(File) -> ?FileMan:drop_file(File) end,
     [?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==h].exprs"),
                   [{group_by,{nopos,"expr_test:h/1"},
                     list,
                     [{{Filename,123,125},"Rec"},
                      {{Filename,131,140},"Rec#rec.f2"},
                      {{Filename,153,159},"#rec.f1"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==h].exprs.fundef"),
                   [{group_by,{nopos,"Rec"},
                     list,[{{Filename,121,160},"expr_test:h/1"}]},
                    {group_by,{nopos,"Rec#rec.f2"},
                     list,[{{Filename,121,160},"expr_test:h/1"}]},
                    {group_by,{nopos,"#rec.f1"},
                     list,[{{Filename,121,160},"expr_test:h/1"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs.exprs.funs"),
                   [{group_by,{nopos,"io:format(\"alma\")"},
                     list,[{nopos,"io:format/1"}]},
                    {group_by,{nopos,"?F(h(Rec))"},
                     list,
                     [{{Filename,61,97},"expr_test:f/1"},
                      {{Filename,121,160},"expr_test:h/1"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==h].exprs.vars"),
                   [{group_by,{nopos,"Rec"},
                     list,[{{Filename,123,125},"Rec"}]},
                    {group_by,{nopos,"Rec#rec.f2"},
                     list,[{{Filename,123,125},"Rec"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==h].exprs.records"),
                   [{group_by,{nopos,"Rec#rec.f2"},
                     list,[{{Filename,21,45},"rec"}]},
                    {group_by,{nopos,"#rec.f1"},
                     list,[{{Filename,21,45},"rec"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs.exprs.macros"),
                   [{group_by,{nopos,"?F(h(Rec))"},
                     list,[{{Filename,47,59},"F"}]}]),
       ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==g].exprs.sub"),
                   [{group_by,{{Filename,101,103},"Rec"},
                     list,[{{Filename,101,103},"Rec"}]},
                    {group_by,{{Filename,109,118},"?F(h(Rec))"},
                     list,
                     [{{Filename,109,118},"?F(h(Rec))"},
                      {{Filename,109,110},"?F"},
                      {{Filename,111,118},"(h(Rec))"},
                      {{Filename,112,117},"h(Rec)"},
                      {{Filename,112,112},"h"},
                      {{Filename,113,117},"(Rec)"},
                      {{Filename,114,116},"Rec"}]}]),
       ?assertEqual(refusr_sq:run(
                      [{positions, scalar},{output, other}],
                      [{file, Filename}, {position, 34}],
                      "@file.funs[name==g].exprs.param"),
                    [{group_by,{nopos,"?F(h(Rec))"},
                      list,[{{Filename,112,117},"h(Rec)"}]}]),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==g].exprs.top"),
                   [{group_by,{{Filename,101,103},"Rec"},
                     list,[{{Filename,101,103},"Rec"}]},
                    {group_by,{{Filename,109,118},"?F(h(Rec))"},
                     list,[{{Filename,109,118},"?F(h(Rec))"}]}]),
%%       ?assertEqual(refusr_sq:run(
%%                   [{positions, scalar},{output, other}],
%%                   [{file, Filename}, {position, 34}],
%%                   "@file.funs[name==g].exprs.reach"),
%%                 []),
%%       ?assertEqual(refusr_sq:run(
%%                   [{positions, scalar},{output, other}],
%%                   [{file, Filename}, {position, 34}],
%%                   "@file.funs[name==g].exprs.origin"),
%%                 []),
      ?assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 34}],
                     "@file.funs[name==g].exprs.file"),
                   [{group_by,{nopos,"Rec"},
                     list,[{{Filename,1,1},"expr_test.erl"}]},
                    {group_by,{nopos,"?F(h(Rec))"},
                     list,[{{Filename,1,1},"expr_test.erl"}]}])
     ]}.
