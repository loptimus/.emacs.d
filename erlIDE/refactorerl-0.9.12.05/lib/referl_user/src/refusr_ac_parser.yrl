Expect 40.

Nonterminals semantic_query query_seq query filter qs
             exp100 exp200 exp300 exp400 exp_max
             maybe_query incomplete_query complete_query
             maybe_filter incomplete_filter complete_filter
             maybe_iteration incomplete_iteration complete_iteration
             maybe_closure incomplete_closure complete_closure
	     maybe_stat incomplete_stat complete_stat.
             %property.

Terminals    '.' '[' ']' '{' '}' '(' ')' '+' ':' '?'
             atom int string comparator
             'not' 'and' 'or' 'like'. %'in'.

Rootsymbol   semantic_query.


semantic_query -> atom           : [{complete, {initial_selector, element(3, '$1')}}].
semantic_query -> atom query_seq : [{complete, {initial_selector, element(3, '$1')}}| '$2'].
semantic_query -> query_seq      : '$1'.
semantic_query -> '?'            : [{complete, {help, initial_selectors}}].


query_seq -> maybe_filter             : '$1'.
query_seq -> maybe_query query_seq    : ['$1' | '$2'].
query_seq -> maybe_query              : ['$1'].
query_seq -> maybe_stat               : ['$1'].

query_seq -> '[' '?' ']'              : [{complete, {help, filters}}].
query_seq -> '.' '?'                  : [{complete, {help, queries}}].

maybe_query -> complete_query         : '$1'.
maybe_query -> incomplete_query       : '$1'.

complete_query -> '.' query           : '$2'.

incomplete_query -> '.'               : {incomplete, {'query', '.'}}.

query -> atom                   : {complete, {selector, element(3, '$1')}}.
query -> maybe_iteration        : '$1'.
query -> maybe_closure          : '$1'.

maybe_stat -> complete_stat	: '$1'.
maybe_stat -> incomplete_stat	: '$1'.

complete_stat -> ':' atom	: {complete, {statistics, element(3, '$2')}}.
complete_stat -> ':' '?'	: {complete, {help, statistics}}.

incomplete_stat -> ':'		: {incomplete, {statistics, ':'}}.

maybe_filter -> complete_filter                 : '$1'.
maybe_filter -> incomplete_filter               : '$1'.

complete_filter -> '[' filter ']' query_seq     : [{complete, {filter, '$2'}} | '$4'].
complete_filter -> '[' filter ']'               : [{complete, {filter, '$2'}}].

incomplete_filter -> '['                        : [{incomplete, {filter, '['}}].
incomplete_filter -> '[' filter                 : [{incomplete, {filter, '$2'}}].


maybe_iteration -> complete_iteration   : '$1'.
maybe_iteration -> incomplete_iteration : '$1'.

complete_iteration -> '{' qs '}' int    : {complete, {iteration, {seq, '$2'}, {mult, element(3, '$4')}}}.

incomplete_iteration -> '{'             : {incomplete, {iteration, element(1, '$1')}}.
incomplete_iteration -> '{' qs          : {incomplete, {iteration, '$2'}}.
incomplete_iteration -> '{' qs '}'      : {incomplete, {iteration, element(1, '$3')}}.

maybe_closure -> complete_closure       : '$1'.
maybe_closure -> incomplete_closure     : '$1'.

complete_closure -> '(' qs ')' int      : {complete, {closure, {seq, '$2'}, {mult, element(3, '$4')}}}.
complete_closure -> '(' qs ')' '+'      : {complete, {closure, {seq, '$2'}, {mult, infinite}}}.

incomplete_closure -> '('               : {incomplete, {closure, element(1, '$1')}}.
incomplete_closure -> '(' qs            : {incomplete, {closure, '$2'}}.
incomplete_closure -> '(' qs ')'        : {incomplete, {closure, element(1, '$3')}}.

qs -> query_seq       : '$1'.
qs -> query query_seq : ['$1'| '$2'].
qs -> query           : ['$1'].





filter  -> exp100                   : '$1'.
%%Disjunction
exp100  -> exp200 'or' exp100       : {complete,   {'or', '$1', '$3'}}.
exp100  -> exp200 'or'              : {incomplete, {'or', '$1'}}.
exp100  -> exp200                   : '$1'.
%%Conjunction
exp200  -> exp300 'and' exp200      : {complete,   {'and', '$1', '$3'}}.
exp200  -> exp300 'and'             : {incomplete, {'and', '$1'}}.
exp200  -> exp300                   : '$1'.
%%Comparison
exp300  -> exp400 comparator exp400 : {complete,   {element(3, '$2'), '$1', '$3'}}.
exp300  -> exp400 comparator        : {incomplete, {comparator, '$1'}}.
exp300  -> exp400 'like' exp400     : {complete,   {'like', '$1', '$3'}}.
exp300  -> exp400 'like'            : {incomplete, {'like', '$1'}}.
exp300  -> exp400                   : '$1'.
%%Negation
exp400  -> 'not' exp_max            : {complete,   {'not', '$2'}}.
exp400  -> 'not'                    : {incomplete, {'not'}}.
exp400  -> exp_max                  : '$1'.
%%Parenthesis
exp_max -> '(' exp100 ')'           : '$2'.
%%Embedded queries
exp_max -> query_seq                : {complete, {seq, '$1'}}.
%%Simple values
exp_max -> atom                     : element(3, '$1').
exp_max -> int                      : element(3, '$1').
exp_max -> string                   : element(3, '$1').
