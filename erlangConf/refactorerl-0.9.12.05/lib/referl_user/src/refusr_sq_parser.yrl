Nonterminals semantic_query query_seq query filter qs
             exp100 exp200 exp300 exp400 exp_max. %property.

Terminals    '.' '[' ']' '{' '}' '(' ')' '+' ':' '?'
             atom int string comparator
             'not' 'and' 'or' 'like'. %'in'.

Rootsymbol   semantic_query.


semantic_query -> atom           : [{initial_selector, element(3, '$1')}].
semantic_query -> atom query_seq : [{initial_selector, element(3, '$1')}| '$2'].
semantic_query -> query_seq      : '$1'.
semantic_query -> '?'            : [{help, initial_selectors}].


query_seq -> '[' filter ']' query_seq : [{filter, '$2'}| '$4'].
query_seq -> '[' filter ']'           : [{filter, '$2'}].
query_seq -> '.' query query_seq      : ['$2'| '$3'].
query_seq -> '.' query                : ['$2'].
query_seq -> ':' atom                 : [{statistics, element(3, '$2')}].

query_seq -> ':' '?'                  : [{help, statistics}].
query_seq -> '[' '?' ']'              : [{help, filters}].
query_seq -> '.' '?'                  : [{help, queries}].


query -> atom           : {selector, element(3, '$1')}.
query -> '{' qs '}' int : {iteration, {seq, '$2'}, {mult, element(3, '$4')}}.
query -> '(' qs ')' int : {closure, {seq, '$2'}, {mult, element(3, '$4')}}.
query -> '(' qs ')' '+' : {closure, {seq, '$2'}, {mult, infinite}}.

qs -> query_seq       : '$1'.
qs -> query query_seq : ['$1'| '$2'].
qs -> query           : ['$1'].

filter  -> exp100                   : '$1'.
%%Disjunction
exp100  -> exp200 'or' exp100       : {'or', '$1', '$3'}.
exp100  -> exp200                   : '$1'.
%%Conjunction
exp200  -> exp300 'and' exp200      : {'and', '$1', '$3'}.
exp200  -> exp300                   : '$1'.
%%Comparison
exp300  -> exp400 comparator exp400 : {element(3, '$2'), '$1', '$3'}.
exp300  -> exp400 'like' exp400     : {'like', '$1', '$3'}.
exp300  -> exp400                   : '$1'.
%%Negation
exp400  -> 'not' exp_max            : {'not', '$2'}.
exp400  -> exp_max                  : '$1'.
%%Parenthesis
exp_max -> '(' exp100 ')'           : '$2'.
%%Embedded queries
exp_max -> query_seq                : {seq, '$1'}.
%%Simple values
exp_max -> atom                     : element(3, '$1').
exp_max -> int                      : element(3, '$1').
exp_max -> string                   : element(3, '$1').
