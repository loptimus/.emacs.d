" Vim global plugin for using RefactorErl
"
" TODO: File reloading after save (not after refactorings)
"       Handling RefactorErl error messages
"
" Maintainer: Roland Király <kiralyroland@inf.elte.hu>
"             Gabor Olah <olikas.g@gmail.com>
"
" License:
"
" The  contents of this  file are  subject to  the Erlang  Public License,
" Version  1.1, (the  "License");  you may  not  use this  file except  in
" compliance  with the License.  You should  have received  a copy  of the
" Erlang  Public License  along  with this  software.  If not,  it can  be
" retrieved at http://plc.inf.elte.hu/erlang/
"
" Software  distributed under  the License  is distributed  on an  "AS IS"
" basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
" the License  for the specific language governing  rights and limitations
" under the License.
"
" The Original Code is RefactorErl.
"
" The Initial Developer of the  Original Code is Eötvös Loránd University.
" Portions created  by Eötvös  Loránd University are  Copyright 2009,
" Eötvös Loránd University. All Rights Reserved.

" Set plugin:
"
" Copy this file into your vim plugin directory (linux:/usr/share/vim/vimcurrent/plugin/)
" Set the s:refpath variable
"
" Usage:
"
" VIM command
"
" RHelp     RefactorErl Help
" Refstart  Start RefactorErl
" Refstop   Stop RefactorErl
" Add       Add file
" Drop      Drop file
" Metric    Run metric query
" Sq        Run semantic query
" Renmod    Rename module
" Renheader Rename header file
" Renvar    Rename variable
" Renfun    Rename function
" Sqr       Run semantic query with file position
" Ls        Show files
"
" In GVim: use the RefactorErl menu
"
if exists("g:loaded_referl")
    finish
endif

let g:loaded_referl = 1
let g:ref_vis_start = -1
let g:ref_vis_stop = -1

let s:save_cpo = &cpo
set cpo&vim

if !hasmapto('<Plug>RefactorErl')
  map <unique> <Leader>a <Plug>RefactorErl
endif

" Modify this variable to your RefacorErl path
" Should be full path!
let s:refpath = '<your-referl-path>/bin/RefactorErl '

noremenu  <silent> &RefactorErl.&Server.St&art\ server :call <SID>refsystem('start')<CR>
noremenu  <silent> &RefactorErl.&Server.St&op\ server :call <SID>refsystem('stop')<CR>

noremenu  <silent> &RefactorErl.F&iles.&Add\ file :call <SID>addfile()<CR>
noremenu  <silent> &RefactorErl.F&iles.&Drop\ file :call <SID>dropfile()<CR>
noremenu  <silent> &RefactorErl.F&iles.&Show\ database :call <SID>show()<CR>
noremenu  <silent> &RefactorErl.F&iles.Draw\ &Graph :call <SID>drgph()<CR>

noremenu  <silent> &RefactorErl.&Undo\ (One\ step\ only) :call <SID>refundo()<CR>

noremenu  <silent> &RefactorErl.-Sep1- :

noremenu  <silent> &RefactorErl.&Semantic\ query :call <SID>paramsq()<CR>

noremenu  <silent> &RefactorErl.-Sep1- :

noremenu  <silent> &RefactorErl.&Function.Reorder\ function\ &parameter :call <SID>refact('reorder_funpar')<CR>
noremenu  <silent> &RefactorErl.&Function.Generate\ function\ &specification :call <SID>refact('genspec')<CR>

noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Eliminate\ function\ call :call <SID>refact('inline_fun')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Eliminate\ macro\ substitution :call <SID>refact('inline_mac')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Eliminate\ variable :call <SID>refact('elim_var')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Eliminate\ fun\ expression :call <SID>refactrange('expand_funexpr')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ function :call <SID>refactrange('extract_fun')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ import :call <SID>refactrange('introduce_import')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ record :call <SID>refactrange('introduce_rec')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ variable :call <SID>refactrange('merge')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ process :call <SID>refact('funapp_to_proc')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ function\ argument :call <SID>refactrange('gen')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Introduce\ tuple :call <SID>refactrange('tuple_funpar')<CR>
noremenu  <silent> &RefactorErl.&Introduce/Eliminate.Transform\ list\ comprehension :call <SID>refactrange('list_comp')<CR>

noremenu  <silent> &RefactorErl.&Move\ to\ another\ module.Move\ function :call <SID>refact('move_fun')<CR>
noremenu  <silent> &RefactorErl.&Move\ to\ another\ module.Move\ macro :call <SID>refact('move_mac')<CR>
noremenu  <silent> &RefactorErl.&Move\ to\ another\ module.Move\ record :call <SID>refact('move_rec')<CR>

noremenu  <silent> &RefactorErl.&Renameing.Universal\ renamer :call <SID>refact('rename')<CR>
noremenu  <silent> &RefactorErl.&Renameing.Rename\ function :call <SID>refact('rename_fun')<CR>
noremenu  <silent> &RefactorErl.&Renameing.Rename\ header :call <SID>refact('rename_header')<CR>
noremenu  <silent> &RefactorErl.&Renameing.Rename\ macro :call <SID>refact('rename_mac')<CR>
noremenu  <silent> &RefactorErl.&Renameing.Rename\ module :call <SID>refact('rename_mod')<CR>
noremenu  <silent> &RefactorErl.&Renameing.Rename\ record :call <SID>refact('rename_rec')<CR>
noremenu  <silent> &RefactorErl.&Renameing.Rename\ record\ field :call <SID>refact('rename_recfield')<CR>
noremenu  <silent> &RefactorErl.&Renameing.Rename\ variable :call <SID>refact('rename_var')<CR>

noremenu  <silent> &RefactorErl.&Upgrade.Upgrade\ regexp\ interface :call <SID>refact('upgrade_regexp')<CR>

noremenu  <silent> &RefactorErl.-Sep1- :

noremenu  <silent> &RefactorErl.Help :call <SID>help()<CR>

command Refh call s:help()
command RefSta call s:refsystem('start')
command RefSto call s:refsystem('stop')
command Refa call s:addfile()
command Refd call s:dropfile()
command Refls call s:show()
command Refgraph call s:drgph()
command Refundo call s:refundo()
command Refsq call s:paramsq()
command Refrf call s:refact('rename_fun')
command Refrh call s:refact('rename_header')
command Refrc call s:refact('rename_mac')
command Refrm call s:refact('rename_mod')
command Refrrf call s:refact('rename_recfield')
command Refrrd call s:refact('rename_rec')
command Refrv call s:refact('rename_var')
command Refmf call s:refact('move_fun')
command Refmm call s:refact('move_mac')
command Refmr call s:refact('move_rec')
command Refef call s:refact('inline_fun')
command Refof call s:refact('reorder_funpar')
command Refgs call s:refact('genspec')
command Refem call s:refact('inline_mac')
command Refev call s:refact('elim_var')
command Refeu call s:refactrange('expand_funexpr')
command Refif call s:refactrange('extract_fun')
command Refii call s:refactrange('introduce_import')
command Refir call s:refactrange('introduce_rec')
command Refiv call s:refactrange('merge')
command Reffp call s:refact('funapp_to_proc')
command Refia call s:refactrange('gen')
command Reftf call s:refactrange('tuple_funpar')
command Reftl call s:refactrange('list_comp')
command Refuir call s:refact('upgrade_regexp')


let s:winop = 0

function s:help()
    if s:winop == 1
        execute ":confirm close"
    endif
    let s:winop = 1
    botright new
    set buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
    call setline(2, 'Refstart  Start RefactorErl')
    call setline(3, 'Refstop   Stop RefactorErl')
    call setline(4, 'Add       Add file')
    call setline(5, 'Drop      Drop file')
    call setline(6, 'Metric    Run metric query')
    call setline(7, 'Sq        Run semantic query')
    call setline(8, 'Renmod    Rename module')
    call setline(9, 'Renheader Rename header file')
    call setline(10, 'Renvar    Rename variable')
    call setline(11, 'Renfun    Rename function')
    call setline(12, 'Sqr       Run semantic query with file position')
    call setline(13, 'Ls        Show files')
    1
endfunction

" Universal refactoring function calling ri:transform/2.
" Param: refactoring is the name of the refactoring accepted by ri:transform.
function s:refact(refactoring)
    let cbn = expand("%:p")
    let pos = line2byte( line( "." )) + col( "." ) - 1
    let stcmd = s:refpath." ri transform " . a:refactoring . " \"[{file, \\\"" . cbn . "\\\"}, {position, " . pos . "}]\""
    execute '!' . stcmd
    call s:refreshFileWindow( bufnr( "%" ), bufname( bufnr( "%" ) ) )
    1
endfunction

" Universal refactoring function based on range calling ri:transform/2.
" Param: refactoring is the name of the refactoring accepted by ri:transform.
function s:refactrange(refactoring)
    let cbn = expand("%:p")
    let posstr = "{posrange, {". g:ref_vis_start .",". g:ref_vis_stop ."}}"
    let stcmd = s:refpath." ri transform " . a:refactoring . " \"[{file, \\\"" . cbn . "\\\"}, ".posstr."]\""
    execute '!' . stcmd
    let g:ref_vis_start = -1
    let g:ref_vis_stop = -1
    call s:refreshFileWindow( bufnr( "%" ), bufname( bufnr( "%" ) ) )
    1
endfunction

" Semantic query handler
" TODO position fix
function s:paramsq()
    let cbf = expand("%:p")
    let querystr = input("Type the query: ")
    let pos = line2byte( line( "." )) + col( "." ) - 1

    let shellcmd = s:refpath . " ri q \\\"" . cbf . "\\\" \"{position, " . pos . "}\" \\\"" . querystr. "\\\" [linenum]"
    let bnum = bufnr('referl')
    if bufexists(bnum)
        execute ':buffer '.bnum
        execute ':bd'
    endif
    execute ':botright  new '.'referl'
    set buftype=nofile noswapfile nowrap
    call setline(1, 'Referl Command:  ' . querystr)
    call append(line('$'), substitute(getline(2), '.', '=', 'g'))
    silent execute '$read !'. shellcmd
    1
endfunction

function s:refundo()
    let stcmd = s:refpath." ri undo"
    execute '!'.stcmd
    1
endfunction

function SavePos()
    if mode()!~#"^[vV\<C-v>]"
        let g:ref_vis_start = line2byte( line( "'<" )) + col( "'<" ) - 1
        let g:ref_vis_stop = line2byte( line( "'>" )) + col( "'>" ) - 1
    endif
    1
endfunction

noremap <silent> <F2> :call SavePos()<CR>

function s:refsystem(fun)
    let stcmd = s:refpath.a:fun
    execute '!'.stcmd
    1
endfunction

function s:addfile()
    let stcmd = s:refpath." ri add \"".expand("%:p")."\""
    execute '!'.stcmd
    1
endfunction

function s:dropfile()
    let stcmd = s:refpath." ri drop \"".expand("%:p")."\""
    execute '!'.stcmd
    1
endfunction


function s:bfnpath(bfname)
    let lastSlash = strridx(a:bfname, '/')
    return strpart(a:bfname, 0, lastSlash+1)
endfunction

function s:shname(bfname)
    let lastSlash = strridx(a:bfname, '/')
    let shrtname = strpart(a:bfname, lastSlash+1, strlen(a:bfname))
    let dotp = strridx(shrtname, '.')
    return strpart(shrtname, 0, dotp)
endfunction

function s:refreshFileWindow(currentbuf, filename)
    exe 'bw '.a:currentbuf
    exe ':edit '.a:filename
endfunction

function s:show()
    let bnum = bufnr('referl')
    if bufexists(bnum)
        execute ':buffer '.bnum
        execute ':bd'
    endif
    execute ':botright  new '.'referl'
    set buftype=nofile noswapfile nowrap
    let shellcmd = s:refpath." ri ls "
    call setline(1, 'Referl Command:  ' ." Show database")
    call append(line('$'), substitute(getline(2), '.', '=', 'g'))
    silent execute '$read !'. shellcmd
    1
endfunction

function s:drgph()
    let file = input("Type the file name here: ")
    let stcmd = s:refpath." ri graph \"".file."\""
    execute '!'.stcmd
    echo stcmd
    1
endfunction

command! -complete=shellcmd -nargs=+ DoShellCmd call s:refAutoCommand(<q-args>)
function s:refAutoCommand(command)
    "let mainbuf = bufnr("$")
    let shellcmd = s:refpath.pr
    for word in split(shellcmd)
    if word[0] =~ '\v[%#<]'
        let word = expand(word)
    endif
    let word = shellescape(word, 1)
    call add(words, word)
    endfor
    let shellcmd = join(words)
    let bnum = bufnr('referl')
    if bufexists(bnum)
        execute ':buffer '.bnum
        execute ':bd'
    endif
    execute ':botright  new '.'referl'
    set buftype=nofile noswapfile nowap
    call setline(1, 'Referl Command: ' . pr)
    call append(line('$'), substitute(getline(2), '.', '=', 'g'))
    silent execute '$read !'. shellcmd
    1
endfunction

" Avoid installing twice or when in unsupported Vim version.
if exists('g:loaded_file_line') || (v:version < 700)
	finish
endif





let &cpo = s:save_cpo
