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

-module(refcore_erl_nodes).
-vsn("$Rev: 3877 $").

-export([structure/1, attribs/1, lexlink/1, parentlink/1]).

-include("core.hrl").

structure(#form{type = file, paren = default}) ->
    [{token, '-'}, {token, file}, {token, '('},
     {symbol, eattr}, {token, ','}, {symbol, eattr},
     {token, ')'}, {token, stop}];
structure(#typexp{type = arity_qualifier}) ->
    [{symbol, tsub}, {token, '/'}, {symbol, tsub}];
structure(#expr{type = infix_expr, value = '++'}) ->
    [{symbol, esub}, {token, '++'}, {symbol, esub}];
structure(#typexp{type = spec_union}) ->
    [{symbol, tsub}, {token, ';'}, {repeat, ';', tsub}];
structure(#expr{type = arglist}) ->
    [{token, '('}, {optional, [{repeat, ',', esub}]},
     {token, ')'}];
structure(#expr{type = filter}) -> [{symbol, exprcl}];
structure(#form{type = type, paren = default,
		tag = type}) ->
    [{token, '-'}, {token, type}, {symbol, tattr},
     {symbol, tattr}, {token, '::'}, {symbol, tattr},
     {token, stop}];
structure(#typexp{type = variable}) ->
    [{token, variable}];
structure(#typexp{type = spec_field}) ->
    [{token, atom},
     {optional, [{token, '='}, {symbol, texpr}]},
     {optional, [{token, '::'}, {symbol, tsub}]}];
structure(#expr{type = binary_gen}) ->
    [{symbol, exprcl}, {token, '<='}, {symbol, exprcl}];
structure(#clause{type = pattern}) ->
    [{symbol, pattern},
     {optional, [{token, 'when'}, {symbol, guard}]},
     {token, '->'}, {repeat, ',', body}];
structure(#expr{type = record_update}) ->
    [{symbol, esub}, {token, '#'}, {token, atom},
     {symbol, esub}];
structure(#form{type = spec, paren = default,
		tag = name}) ->
    [{token, '-'}, {token, spec}, {symbol, tattr},
     {symbol, tattr}, {token, stop}];
structure(#form{type = export, paren = no}) ->
    [{token, '-'}, {token, export}, {symbol, eattr},
     {token, stop}];
structure(#expr{type = catch_expr}) ->
    [{token, 'catch'}, {symbol, exprcl}];
structure(#expr{type = application}) ->
    [{symbol, esub}, {symbol, esub}];
structure(#expr{type = record_index}) ->
    [{token, '#'}, {token, atom}, {token, '.'},
     {symbol, esub}];
structure(#clause{type = funexpr}) ->
    [{token, '('}, {optional, [{repeat, ',', pattern}]},
     {token, ')'},
     {optional, [{token, 'when'}, {symbol, guard}]},
     {token, '->'}, {repeat, ',', body}];
structure(#expr{type = record_field}) ->
    [{token, atom},
     {optional, [{token, '='}, {symbol, esub}]}];
structure(#expr{type = prefix_bit_expr, value = '-'}) ->
    [{token, '-'}, {symbol, esub}];
structure(#expr{type = infix_expr, value = 'div'}) ->
    [{symbol, esub}, {token, 'div'}, {symbol, esub}];
structure(#typexp{type = func, tag = sig}) ->
    [{token, 'fun'}, {token, '('}, {symbol, tsub},
     {token, ')'}];
structure(#expr{type = variable}) ->
    [{token, variable}];
structure(#expr{type = prefix_bit_expr,
		value = 'bnot'}) ->
    [{token, 'bnot'}, {symbol, esub}];
structure(#expr{type = integer}) -> [{token, integer}];
structure(#expr{type = xatom}) ->
    [{token, '.'}, {symbol, esub},
     {optional, [{symbol, esub}]}];
structure(#form{type = spec, paren = yes, tag = ref}) ->
    [{token, '-'}, {token, spec}, {token, '('},
     {symbol, tattr}, {token, '::'}, {symbol, tattr},
     {token, ')'}, {token, stop}];
structure(#form{type = type, paren = default,
		tag = opaque}) ->
    [{token, '-'}, {token, opaque}, {symbol, tattr},
     {symbol, tattr}, {token, '::'}, {symbol, tattr},
     {token, stop}];
structure(#expr{type = prefix_bit_expr, value = '+'}) ->
    [{token, '+'}, {symbol, esub}];
structure(#expr{type = infix_expr, value = '/='}) ->
    [{symbol, esub}, {token, '/='}, {symbol, esub}];
structure(#typexp{type = list, tag = empty}) ->
    [{token, '['}, {token, ']'}];
structure(#clause{type = expr}) -> [{symbol, body}];
structure(#expr{type = funlist}) ->
    [{token, '['}, {optional, [{repeat, ',', esub}]},
     {token, ']'}];
structure(#clause{type = hexpr}) -> [{symbol, body}];
structure(#expr{type = infix_expr, value = 'rem'}) ->
    [{symbol, esub}, {token, 'rem'}, {symbol, esub}];
structure(#form{type = type, paren = yes,
		tag = opaque}) ->
    [{token, '-'}, {token, opaque}, {token, '('},
     {symbol, tattr}, {symbol, tattr}, {token, '::'},
     {symbol, tattr}, {token, ')'}, {token, stop}];
structure(#expr{type = match_expr}) ->
    [{symbol, esub}, {token, '='}, {symbol, esub}];
structure(#typexp{type = paren}) ->
    [{token, '('}, {symbol, tsub}, {token, ')'}];
structure(#expr{type = binary_field}) ->
    [{symbol, esub},
     {optional, [{token, '/'}, {repeat, '-', esub}]}];
structure(#typexp{type = field}) ->
    [{symbol, tsub}, {token, '::'}, {symbol, tsub}];
structure(#expr{type = block_expr}) ->
    [{token, 'begin'}, {symbol, exprcl}, {token, 'end'}];
structure(#expr{type = infix_expr, value = 'bor'}) ->
    [{symbol, esub}, {token, 'bor'}, {symbol, esub}];
structure(#typexp{type = integer}) ->
    [{token, integer}];
structure(#expr{type = infix_expr, value = 'orelse'}) ->
    [{symbol, exprcl}, {token, 'orelse'}, {symbol, exprcl}];
structure(#expr{type = infix_expr, value = '=:='}) ->
    [{symbol, esub}, {token, '=:='}, {symbol, esub}];
structure(#expr{type = infix_expr, value = '>'}) ->
    [{symbol, esub}, {token, '>'}, {symbol, esub}];
structure(#expr{type = infix_expr, value = '=='}) ->
    [{symbol, esub}, {token, '=='}, {symbol, esub}];
structure(#clause{type = compr}) ->
    [{repeat, ',', body}];
structure(#typexp{type = atom}) -> [{token, atom}];
structure(#expr{type = bit_size_expr}) ->
    [{symbol, esub}, {token, ':'}, {token, integer}];
structure(#expr{type = implicit_fun}) ->
    [{token, 'fun'}, {symbol, esub}, {token, '/'},
     {symbol, esub}];
structure(#typexp{type = arglist}) ->
    [{token, '('}, {optional, [{repeat, ',', tsub}]},
     {token, ')'}];
structure(#expr{type = record_access}) ->
    [{symbol, esub}, {token, '#'}, {token, atom},
     {token, '.'}, {symbol, esub}];
structure(#typexp{type = record}) ->
    [{token, '#'}, {token, atom}, {token, '{'},
     {optional, [{repeat, ',', tsub}]}, {token, '}'}];
structure(#expr{type = infix_expr,
		value = 'andalso'}) ->
    [{symbol, exprcl}, {token, 'andalso'},
     {symbol, exprcl}];
structure(#typexp{type = spec_guard}) ->
    [{symbol, tsub}, {token, 'when'}, {symbol, tsub}];
structure(#expr{type = prefix_bit_expr,
		value = 'not'}) ->
    [{token, 'not'}, {symbol, esub}];
structure(#typexp{type = negate}) ->
    [{token, '-'}, {symbol, tsub}];
structure(#clause{type = guard}) ->
    [{symbol, guard}, {token, '->'}, {repeat, ',', body}];
structure(#expr{type = float}) -> [{token, float}];
structure(#typexp{type = poly_sig}) ->
    [{token, '('}, {token, '.'}, {token, '.'}, {token, '.'},
     {token, ')'}, {token, '->'}, {symbol, tsub}];
structure(#clause{type = timeout}) ->
    [{symbol, tmout}, {token, '->'}, {repeat, ',', body}];
structure(#expr{type = infix_expr, value = '-'}) ->
    [{symbol, esub}, {token, '-'}, {symbol, esub}];
structure(#expr{type = parenthesis}) ->
    [{token, '('}, {symbol, esub}, {token, ')'}];
structure(#expr{type = infix_expr, value = '+'}) ->
    [{symbol, esub}, {token, '+'}, {symbol, esub}];
structure(#form{type = spec, paren = yes,
		tag = name}) ->
    [{token, '-'}, {token, spec}, {token, '('},
     {symbol, tattr}, {symbol, tattr}, {token, ')'},
     {token, stop}];
structure(#expr{type = funref}) ->
    [{symbol, esub}, {token, '/'}, {symbol, esub}];
structure(#typexp{type = varlist}) ->
    [{token, '('}, {optional, [{repeat, ',', tsub}]},
     {token, ')'}];
structure(#expr{type = char}) -> [{token, char}];
structure(#expr{type = infix_expr, value = 'bxor'}) ->
    [{symbol, esub}, {token, 'bxor'}, {symbol, esub}];
structure(#typexp{type = guardlist}) ->
    [{symbol, tsub}, {token, ','}, {repeat, ',', tsub}];
structure(#typexp{type = fun_sig}) ->
    [{symbol, tsub}, {token, '->'}, {symbol, tsub}];
structure(#form{type = func}) ->
    [{repeat, ';', funcl}, {token, stop}];
structure(#expr{type = receive_expr}) ->
    [{token, 'receive'},
     {optional, [{repeat, ';', exprcl}]},
     {optional, [{token, 'after'}, {symbol, aftercl}]},
     {token, 'end'}];
structure(#typexp{type = list, tag = nonempty}) ->
    [{token, '['}, {symbol, tsub}, {token, ','},
     {token, '.'}, {token, '.'}, {token, '.'}, {token, ']'}];
structure(#expr{type = case_expr}) ->
    [{token, 'case'}, {symbol, headcl}, {token, 'of'},
     {repeat, ';', exprcl}, {token, 'end'}];
structure(#expr{type = infix_expr, value = '--'}) ->
    [{symbol, esub}, {token, '--'}, {symbol, esub}];
structure(#expr{type = string}) -> [{token, string}];
structure(#expr{type = record_expr}) ->
    [{token, '#'}, {token, atom}, {symbol, esub}];
structure(#form{type = export, paren = default}) ->
    [{token, '-'}, {token, export}, {token, '('},
     {symbol, eattr}, {token, ')'}, {token, stop}];
structure(#typexp{type = tuple}) ->
    [{token, '{'}, {optional, [{repeat, ',', tsub}]},
     {token, '}'}];
structure(#expr{type = tuple}) ->
    [{token, '{'}, {optional, [{repeat, ',', esub}]},
     {token, '}'}];
structure(#clause{type = pexpr}) -> [{symbol, pattern}];
structure(#form{type = module, paren = no}) ->
    [{token, '-'}, {token, module}, {token, atom},
     {token, stop}];
structure(#expr{type = if_expr}) ->
    [{token, 'if'}, {repeat, ';', exprcl}, {token, 'end'}];
structure(#expr{type = infix_expr, value = '>='}) ->
    [{symbol, esub}, {token, '>='}, {symbol, esub}];
structure(#clause{type = block}) ->
    [{repeat, ',', body}];
structure(#typexp{type = union}) ->
    [{symbol, tsub}, {token, '|'}, {repeat, '|', tsub}];
structure(#typexp{type = call}) ->
    [{symbol, tsub}, {symbol, tsub}];
structure(#expr{type = atom}) -> [{token, atom}];
structure(#expr{type = infix_expr, value = ','}) ->
    [{symbol, esub}, {token, ','}, {symbol, esub}];
structure(#form{type = record, paren = no}) ->
    [{token, '-'}, {token, record}, {token, atom},
     {token, ','}, {token, '{'},
     {optional, [{repeat, ',', tattr}]}, {token, '}'},
     {token, stop}];
structure(#typexp{type = module_qualifier}) ->
    [{symbol, tsub}, {token, ':'}, {symbol, tsub}];
structure(#form{type = record, paren = default}) ->
    [{token, '-'}, {token, record}, {token, '('},
     {token, atom}, {token, ','}, {token, '{'},
     {optional, [{repeat, ',', tattr}]}, {token, '}'},
     {token, ')'}, {token, stop}];
structure(#expr{type = cons}) ->
    [{token, '['},
     {optional,
      [{symbol, esub},
       {optional, [{token, '|'}, {symbol, esub}]}]},
     {token, ']'}];
structure(#form{type = type, paren = yes,
		tag = type}) ->
    [{token, '-'}, {token, type}, {token, '('},
     {symbol, tattr}, {symbol, tattr}, {token, '::'},
     {symbol, tattr}, {token, ')'}, {token, stop}];
structure(#expr{type = infix_expr, value = 'bsr'}) ->
    [{symbol, esub}, {token, 'bsr'}, {symbol, esub}];
structure(#expr{type = joker}) -> [{token, '_'}];
structure(#form{type = import, paren = default}) ->
    [{token, '-'}, {token, import}, {token, '('},
     {symbol, eattr}, {token, ','}, {symbol, eattr},
     {token, ')'}, {token, stop}];
structure(#clause{type = fundef}) ->
    [{symbol, name}, {token, '('},
     {optional, [{repeat, ',', pattern}]}, {token, ')'},
     {optional, [{token, 'when'}, {symbol, guard}]},
     {token, '->'}, {repeat, ',', body}];
structure(#form{type = spec, paren = default,
		tag = ref}) ->
    [{token, '-'}, {token, spec}, {symbol, tattr},
     {token, '::'}, {symbol, tattr}, {token, stop}];
structure(#expr{type = mstring}) ->
    [{token, string}, {symbol, esub}];
structure(#expr{type = infix_expr, value = ';'}) ->
    [{symbol, esub}, {token, ';'}, {symbol, esub}];
structure(#expr{type = infix_expr, value = 'xor'}) ->
    [{symbol, esub}, {token, 'xor'}, {symbol, esub}];
structure(#expr{type = prefix_expr, value = 'bnot'}) ->
    [{token, 'bnot'}, {symbol, esub}];
structure(#expr{type = prefix_expr, value = '-'}) ->
    [{token, '-'}, {symbol, esub}];
structure(#typexp{type = vardef}) ->
    [{symbol, tsub}, {token, '::'}, {symbol, tsub}];
structure(#typexp{type = func, tag = any}) ->
    [{token, 'fun'}, {token, '('}, {token, ')'}];
structure(#typexp{type = guard}) ->
    [{token, atom}, {symbol, tsub}];
structure(#expr{type = infix_expr, value = ':'}) ->
    [{symbol, esub}, {token, ':'}, {symbol, esub}];
structure(#expr{type = infix_expr, value = 'and'}) ->
    [{symbol, esub}, {token, 'and'}, {symbol, esub}];
structure(#expr{type = infix_expr, value = '/'}) ->
    [{symbol, esub}, {token, '/'}, {symbol, esub}];
structure(#expr{type = try_expr}) ->
    [{token, 'try'}, {symbol, headcl},
     {optional, [{token, 'of'}, {repeat, ';', exprcl}]},
     {optional, [{token, 'catch'}, {repeat, ';', catchcl}]},
     {optional, [{token, 'after'}, {symbol, aftercl}]},
     {token, 'end'}];
structure(#expr{type = prefix_expr, value = 'not'}) ->
    [{token, 'not'}, {symbol, esub}];
structure(#typexp{type = list, tag = any}) ->
    [{token, '['}, {symbol, tsub}, {token, ']'}];
structure(#expr{type = binary}) ->
    [{token, '<<'}, {optional, [{repeat, ',', esub}]},
     {token, '>>'}];
structure(#expr{type = infix_expr, value = 'or'}) ->
    [{symbol, esub}, {token, 'or'}, {symbol, esub}];
structure(#expr{type = field_list}) ->
    [{token, '{'}, {optional, [{repeat, ',', esub}]},
     {token, '}'}];
structure(#form{type = import, paren = no}) ->
    [{token, '-'}, {token, import}, {symbol, eattr},
     {token, ','}, {symbol, eattr}, {token, stop}];
structure(#expr{type = infix_expr, value = '*'}) ->
    [{symbol, esub}, {token, '*'}, {symbol, esub}];
structure(#expr{type = infix_expr, value = '=/='}) ->
    [{symbol, esub}, {token, '=/='}, {symbol, esub}];
structure(#expr{type = send_expr}) ->
    [{symbol, esub}, {token, '!'}, {symbol, esub}];
structure(#form{type = module, paren = default}) ->
    [{token, '-'}, {token, module}, {token, '('},
     {token, atom}, {token, ')'}, {token, stop}];
structure(#form{type = file, paren = no}) ->
    [{token, '-'}, {token, file}, {symbol, eattr},
     {token, ','}, {symbol, eattr}, {token, stop}];
structure(#expr{type = list}) -> [{repeat, ',', esub}];
structure(#expr{type = xatom2}) ->
    [{symbol, esub}, {symbol, esub}];
structure(#form{type = attrib, paren = no}) ->
    [{token, '-'}, {token, atom}, {symbol, eattr},
     {token, stop}];
structure(#form{type = attrib, paren = default}) ->
    [{token, '-'}, {token, atom}, {token, '('},
     {symbol, eattr}, {token, ')'}, {token, stop}];
structure(#typexp{type = bin_unit}) ->
    [{symbol, tsub}, {token, ':'}, {symbol, tsub},
     {token, '*'}, {symbol, tsub}];
structure(#expr{type = infix_expr, value = '<'}) ->
    [{symbol, esub}, {token, '<'}, {symbol, esub}];
structure(#expr{type = record_joker_field}) ->
    [{token, '_'}, {token, '='}, {symbol, esub}];
structure(#form{type = lex, tag = empty}) ->
    [{token, eol}];
structure(#expr{type = prefix_expr, value = '+'}) ->
    [{token, '+'}, {symbol, esub}];
structure(#expr{type = infix_expr, value = 'bsl'}) ->
    [{symbol, esub}, {token, 'bsl'}, {symbol, esub}];
structure(#expr{type = list_gen}) ->
    [{symbol, exprcl}, {token, '<-'}, {symbol, exprcl}];
structure(#typexp{type = binary}) ->
    [{token, '<<'},
     {optional,
      [{symbol, tsub},
       {optional, [{token, ','}, {symbol, tsub}]}]},
     {token, '>>'}];
structure(#typexp{type = interval}) ->
    [{symbol, tsub}, {token, '.'}, {token, '.'},
     {symbol, tsub}];
structure(#expr{type = fun_expr}) ->
    [{token, 'fun'}, {repeat, ';', exprcl}, {token, 'end'}];
structure(#expr{type = list_comp}) ->
    [{token, '['}, {symbol, exprcl}, {token, '||'},
     {symbol, exprcl}, {token, ']'}];
structure(#expr{type = infix_expr, value = '=<'}) ->
    [{symbol, esub}, {token, '=<'}, {symbol, esub}];
structure(#expr{type = infix_expr, value = 'band'}) ->
    [{symbol, esub}, {token, 'band'}, {symbol, esub}];
structure(#expr{type = size_qualifier}) ->
    [{symbol, esub}, {token, ':'}, {symbol, esub}];
structure(#typexp{type = joker}) -> [{token, '_'}];
structure(#typexp{type = bin_base}) ->
    [{symbol, tsub}, {token, ':'}, {symbol, tsub}];
structure(#expr{type = bin_comp}) ->
    [{token, '<<'}, {symbol, exprcl}, {token, '||'},
     {symbol, exprcl}, {token, '>>'}];
structure(D) -> erlang:error({unknown_structure, D}).

attribs(#typexp{type = variable}) ->
    [{{tlex, 1}, #typexp.tag}];
attribs(#typexp{type = spec_field}) ->
    [{{tlex, 1}, #typexp.tag}];
attribs(#expr{type = record_update}) ->
    [{{elex, 2}, #expr.value}];
attribs(#expr{type = record_index}) ->
    [{{elex, 2}, #expr.value}];
attribs(#expr{type = record_field}) ->
    [{{elex, 1}, #expr.value}];
attribs(#expr{type = variable}) ->
    [{{elex, 1}, #expr.value}];
attribs(#expr{type = integer}) ->
    [{{elex, 1}, #expr.value}];
attribs(#typexp{type = integer}) ->
    [{{tlex, 1}, #typexp.tag}];
attribs(#typexp{type = atom}) ->
    [{{tlex, 1}, #typexp.tag}];
attribs(#expr{type = bit_size_expr}) ->
    [{{elex, 2}, #expr.value}];
attribs(#expr{type = record_access}) ->
    [{{elex, 2}, #expr.value}];
attribs(#typexp{type = record}) ->
    [{{tlex, 2}, #typexp.tag}];
attribs(#expr{type = float}) ->
    [{{elex, 1}, #expr.value}];
attribs(#expr{type = char}) ->
    [{{elex, 1}, #expr.value}];
attribs(#expr{type = string}) ->
    [{{elex, 1}, #expr.value}];
attribs(#expr{type = record_expr}) ->
    [{{elex, 2}, #expr.value}];
attribs(#form{type = module, paren = no}) ->
    [{{flex, 3}, #form.tag}];
attribs(#expr{type = atom}) ->
    [{{elex, 1}, #expr.value}];
attribs(#form{type = record, paren = no}) ->
    [{{flex, 3}, #form.tag}];
attribs(#form{type = record, paren = default}) ->
    [{{flex, 4}, #form.tag}];
attribs(#expr{type = mstring}) ->
    [{{elex, 1}, #expr.value}];
attribs(#form{type = module, paren = default}) ->
    [{{flex, 4}, #form.tag}];
attribs(#form{type = attrib, paren = no}) ->
    [{{flex, 2}, #form.tag}];
attribs(#form{type = attrib, paren = default}) ->
    [{{flex, 2}, #form.tag}];
attribs(_) -> [].

lexlink(form) -> flex;
lexlink(clause) -> clex;
lexlink(expr) -> elex;
lexlink(typexp) -> tlex;
lexlink(C) -> erlang:error({unknown_class, C}).

parentlink(form) -> [];
parentlink(clause) ->
    [aftercl, catchcl, exprcl, funcl, headcl];
parentlink(expr) ->
    [body, eattr, esub, guard, name, pattern, texpr, tmout];
parentlink(typexp) -> [tattr, tsub];
parentlink(C) -> erlang:error({unknown_class, C}).
