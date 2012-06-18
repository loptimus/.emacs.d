Nonterminals
  ExpComp EFldList_field_list@rep_esub ExpAtomic TSgnInt EString
  ERecordIdx CPattern_pattern@rep_body CPattern EAndalso
  TRecord_record@rep_tsub FFunSpec EALstHead CPBinary
  CBlock_block@rep_body CFunExp_funexpr@rep_pattern TArgList
  TGrdList TField EField FFile TTuple ETuple EAFunRef
  FFunction_func@rep_funcl TCall ECall ECase_case_expr@rep_exprcl
  EBinElem Exp100 Exp200 Exp300 Exp400 Exp500 Exp600 Exp700 Exp800
  Exp900 CE200 TTuple_tuple@rep_tsub ELstHead_list@rep_esub
  EALstHead_list@rep_esub ETuple_tuple@rep_esub
  EATuple_tuple@rep_esub TFldSpec TBinSpec EArgList EFldList
  TSpecUnion EXAtom1 FRecord_record_default@rep_tattr ExpNoPar
  EXAtom2 ELstCompr TFunc EFunc TypGuard ESend EConj
  ETry_try_expr@rep_exprcl ETry_try_expr@rep_catchcl ExpBin0
  ExpBin1 TFunArgs_varlist@rep_tsub TSpecUnion_spec_union@rep_tsub
  TInt ExpAConst EInt CFunction_fundef@rep_body
  EFunc_fun_expr@rep_exprcl TAtom EXAtom EAtom
  CFunction_fundef@rep_pattern TUnion EColon ECmpOp ELstOp EAddOp
  EMulOp EPreOp EBinOp TFunArgs ERecordAccess TLimitInt ExpConst
  FModule Type TypName ERecordUpdate EATuple EReceive TPolySig
  TGrdList_guardlist@rep_tsub Expr EAllAtomOrVar CCompr ECompr
  TGrdFunSig CCompr_compr@rep_body Exp150 Exp750 CE150 TVar
  EIntOrVar EChar EVar EFilter TGuard ExpGrd EFloat CGrd ExpMax
  Form FFunction CFunction TypFunction Exp160 CE160
  EArgList_arglist@rep_esub EBitT EBinSize ECase EMatch ECatch ETry
  CGrd_guard@rep_body EDisj EBlock CBlock TypUnion
  TArgList_arglist@rep_tsub TList EList EAList EFunName TFunSig
  CAfter_timeout@rep_body CFunExp FAttrib TUnion_union@rep_tsub
  EBinElem_binary_field@rep_esub Guards TExtName FEmpty
  CFunExp_funexpr@rep_body TParen EParen CAfter FRecord ERecord
  TBinary EReceive_receive_expr@rep_exprcl EAllAtom TTypVar
  ERecordExpr EAFunList_funlist@rep_esub FExport FImport TRecord
  ELstHead EOrelse EBinary EBinCompr TypSpec TFunRef ERecordOrMax
  CHBinary EIf_if_expr@rep_exprcl CHExp CPExp CExp
  FRecord_record_no@rep_tattr ExpAttr EBinary_binary@rep_esub
  TypFunSpec EAFunList FTypDef EIf TypFunSig EAParen ELstGen
  EBinGen.

Terminals
  'andalso' 'begin' '<=' '--' '<<' 'if' '<-' variable '=:=' '++'
  '::' 'after' 'fun' 'when' 'rem' 'receive' opaque spec integer
  'bsr' 'bsl' 'try' 'catch' 'case' 'orelse' import export float
  record char '}' '|' '{' type 'not' 'bnot' 'xor' 'or' 'bxor' 'bor'
  stop '_' ']' atom eol '[' 'of' string 'end' 'and' 'band' '>' '='
  '||' '<' ';' ':' '/' '.' '=/=' '/=' '-' ',' '+' '*' ')' '(' file
  module '#' '!' '>>' '>=' '->' '==' '=<' 'div'.

Rootsymbol Form.

EBinGen ->
    CPBinary '<=' CExp :
    build(#expr{type = binary_gen},
	  [{exprcl, '$1'}, {elex, tn('$2')}, {exprcl, '$3'}]).
ELstGen ->
    CPExp '<-' CExp :
    build(#expr{type = list_gen},
	  [{exprcl, '$1'}, {elex, tn('$2')}, {exprcl, '$3'}]).
EAParen ->
    '(' ExpAttr ')' :
    build(#expr{type = parenthesis},
	  [{elex, tn('$1')}, {esub, '$2'}, {elex, tn('$3')}]).
TypFunSig ->
    TFunSig :
    '$1'.
TypFunSig ->
    TPolySig :
    '$1'.
EIf ->
    'if' EIf_if_expr@rep_exprcl 'end' :
    build(#expr{type = if_expr},
	  [{elex, tn('$1')}, '$2', {elex, tn('$3')}]).
FTypDef ->
    '-' type TAtom TFunArgs '::' TypSpec stop :
    build(#form{type = type, paren = default, tag = type},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {tattr, '$3'},
	   {tattr, '$4'}, {flex, tn('$5')}, {tattr, '$6'},
	   {flex, tn('$7')}]).
FTypDef ->
    '-' opaque TAtom TFunArgs '::' TypSpec stop :
    build(#form{type = type, paren = default, tag = opaque},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {tattr, '$3'},
	   {tattr, '$4'}, {flex, tn('$5')}, {tattr, '$6'},
	   {flex, tn('$7')}]).
FTypDef ->
    '-' type '(' TAtom TFunArgs '::' TypSpec ')' stop :
    build(#form{type = type, paren = yes, tag = type},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {tattr, '$4'}, {tattr, '$5'}, {flex, tn('$6')},
	   {tattr, '$7'}, {flex, tn('$8')}, {flex, tn('$9')}]).
FTypDef ->
    '-' opaque '(' TAtom TFunArgs '::' TypSpec ')' stop :
    build(#form{type = type, paren = yes, tag = opaque},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {tattr, '$4'}, {tattr, '$5'}, {flex, tn('$6')},
	   {tattr, '$7'}, {flex, tn('$8')}, {flex, tn('$9')}]).
EAFunList ->
    '[' EAFunList_funlist@rep_esub ']' :
    build(#expr{type = funlist},
	  [{elex, tn('$1')}, '$2', {elex, tn('$3')}]).
EAFunList ->
    '[' ']' :
    build(#expr{type = funlist},
	  [{elex, tn('$1')}, {elex, tn('$2')}]).
TypFunSpec ->
    TFunSig :
    '$1'.
TypFunSpec ->
    TGrdFunSig :
    '$1'.
EBinary_binary@rep_esub ->
    EBinary_binary@rep_esub ',' EBinElem :
    ['$1', {elex, tn('$2')}, {esub, '$3'}].
EBinary_binary@rep_esub ->
    EBinElem :
    [{esub, '$1'}].
ExpAttr ->
    EAParen :
    '$1'.
ExpAttr ->
    ExpAtomic :
    '$1'.
ExpAttr ->
    ExpAConst :
    '$1'.
ExpAttr ->
    EAFunRef :
    '$1'.
FRecord_record_no@rep_tattr ->
    FRecord_record_no@rep_tattr ',' TFldSpec :
    ['$1', {flex, tn('$2')}, {tattr, '$3'}].
FRecord_record_no@rep_tattr ->
    TFldSpec :
    [{tattr, '$1'}].
CExp ->
    Expr :
    build(#clause{type = expr}, [{body, '$1'}]).
CPExp ->
    Expr :
    build(#clause{type = pexpr}, [{pattern, '$1'}]).
CHExp ->
    Expr :
    build(#clause{type = hexpr}, [{body, '$1'}]).
EIf_if_expr@rep_exprcl ->
    EIf_if_expr@rep_exprcl ';' CGrd :
    ['$1', {elex, tn('$2')}, {exprcl, '$3'}].
EIf_if_expr@rep_exprcl ->
    CGrd :
    [{exprcl, '$1'}].
CHBinary ->
    EBinary :
    build(#clause{type = hexpr}, [{body, '$1'}]).
ERecordOrMax ->
    ERecord :
    '$1'.
ERecordOrMax ->
    ExpMax :
    '$1'.
TFunRef ->
    TypName '/' TInt :
    build(#typexp{type = arity_qualifier},
	  [{tsub, '$1'}, {tlex, tn('$2')}, {tsub, '$3'}]).
TypSpec ->
    TTypVar :
    '$1'.
TypSpec ->
    TypUnion :
    '$1'.
EBinCompr ->
    '<<' CHBinary '||' CCompr '>>' :
    build(#expr{type = bin_comp},
	  [{elex, tn('$1')}, {exprcl, '$2'}, {elex, tn('$3')},
	   {exprcl, '$4'}, {elex, tn('$5')}]).
EBinary ->
    '<<' EBinary_binary@rep_esub '>>' :
    build(#expr{type = binary},
	  [{elex, tn('$1')}, '$2', {elex, tn('$3')}]).
EBinary ->
    '<<' '>>' :
    build(#expr{type = binary},
	  [{elex, tn('$1')}, {elex, tn('$2')}]).
EOrelse ->
    CE160 'orelse' CE150 :
    build(#expr{type = infix_expr, value = 'orelse'},
	  [{exprcl, '$1'}, {elex, tn('$2')}, {exprcl, '$3'}]).
ELstHead ->
    ELstHead_list@rep_esub :
    build(#expr{type = list}, ['$1']).
TRecord ->
    '#' atom '{' TRecord_record@rep_tsub '}' :
    build(#typexp{type = record, tag = tv('$2')},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}, {tlex, tn('$3')},
	   '$4', {tlex, tn('$5')}]).
TRecord ->
    '#' atom '{' '}' :
    build(#typexp{type = record, tag = tv('$2')},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}, {tlex, tn('$3')},
	   {tlex, tn('$4')}]).
FImport ->
    '-' import '(' EAtom ',' EAFunList ')' stop :
    build(#form{type = import, paren = default},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {eattr, '$4'}, {flex, tn('$5')}, {eattr, '$6'},
	   {flex, tn('$7')}, {flex, tn('$8')}]).
FImport ->
    '-' import EAtom ',' EAFunList stop :
    build(#form{type = import, paren = no},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {eattr, '$3'},
	   {flex, tn('$4')}, {eattr, '$5'}, {flex, tn('$6')}]).
FExport ->
    '-' export '(' EAFunList ')' stop :
    build(#form{type = export, paren = default},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {eattr, '$4'}, {flex, tn('$5')}, {flex, tn('$6')}]).
FExport ->
    '-' export EAFunList stop :
    build(#form{type = export, paren = no},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {eattr, '$3'},
	   {flex, tn('$4')}]).
EAFunList_funlist@rep_esub ->
    EAFunList_funlist@rep_esub ',' EAFunRef :
    ['$1', {elex, tn('$2')}, {esub, '$3'}].
EAFunList_funlist@rep_esub ->
    EAFunRef :
    [{esub, '$1'}].
ERecordExpr ->
    '#' atom EFldList :
    build(#expr{type = record_expr, value = tv('$2')},
	  [{elex, tn('$1')}, {elex, tn('$2')}, {esub, '$3'}]).
TTypVar ->
    TVar '::' TypSpec :
    build(#typexp{type = vardef},
	  [{tsub, '$1'}, {tlex, tn('$2')}, {tsub, '$3'}]).
EAllAtom ->
    EAtom :
    '$1'.
EAllAtom ->
    EXAtom :
    '$1'.
EReceive_receive_expr@rep_exprcl ->
    EReceive_receive_expr@rep_exprcl ';' CPattern :
    ['$1', {elex, tn('$2')}, {exprcl, '$3'}].
EReceive_receive_expr@rep_exprcl ->
    CPattern :
    [{exprcl, '$1'}].
TBinary ->
    '<<' TBinSpec ',' TBinSpec '>>' :
    build(#typexp{type = binary},
	  [{tlex, tn('$1')}, {tsub, '$2'}, {tlex, tn('$3')},
	   {tsub, '$4'}, {tlex, tn('$5')}]).
TBinary ->
    '<<' TBinSpec '>>' :
    build(#typexp{type = binary},
	  [{tlex, tn('$1')}, {tsub, '$2'}, {tlex, tn('$3')}]).
TBinary ->
    '<<' '>>' :
    build(#typexp{type = binary},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}]).
ERecord ->
    ERecordIdx :
    '$1'.
ERecord ->
    ERecordExpr :
    '$1'.
ERecord ->
    ERecordAccess :
    '$1'.
ERecord ->
    ERecordUpdate :
    '$1'.
FRecord ->
    '-' record '(' atom ',' '{' FRecord_record_default@rep_tattr '}' ')' stop :
    build(#form{type = record, paren = default,
		tag = tv('$4')},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {flex, tn('$4')}, {flex, tn('$5')}, {flex, tn('$6')},
	   '$7', {flex, tn('$8')}, {flex, tn('$9')},
	   {flex, tn('$10')}]).
FRecord ->
    '-' record '(' atom ',' '{' '}' ')' stop :
    build(#form{type = record, paren = default,
		tag = tv('$4')},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {flex, tn('$4')}, {flex, tn('$5')}, {flex, tn('$6')},
	   {flex, tn('$7')}, {flex, tn('$8')}, {flex, tn('$9')}]).
FRecord ->
    '-' record atom ',' '{' FRecord_record_no@rep_tattr '}' stop :
    build(#form{type = record, paren = no, tag = tv('$3')},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {flex, tn('$4')}, {flex, tn('$5')}, '$6',
	   {flex, tn('$7')}, {flex, tn('$8')}]).
FRecord ->
    '-' record atom ',' '{' '}' stop :
    build(#form{type = record, paren = no, tag = tv('$3')},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {flex, tn('$4')}, {flex, tn('$5')}, {flex, tn('$6')},
	   {flex, tn('$7')}]).
CAfter ->
    Expr '->' CAfter_timeout@rep_body :
    build(#clause{type = timeout},
	  [{tmout, '$1'}, {clex, tn('$2')}, '$3']).
EParen ->
    '(' Expr ')' :
    build(#expr{type = parenthesis},
	  [{elex, tn('$1')}, {esub, '$2'}, {elex, tn('$3')}]).
TParen ->
    '(' TypSpec ')' :
    build(#typexp{type = paren},
	  [{tlex, tn('$1')}, {tsub, '$2'}, {tlex, tn('$3')}]).
CFunExp_funexpr@rep_body ->
    CFunExp_funexpr@rep_body ',' Expr :
    ['$1', {clex, tn('$2')}, {body, '$3'}].
CFunExp_funexpr@rep_body ->
    Expr :
    [{body, '$1'}].
FEmpty ->
    eol :
    build(#form{type = lex, tag = empty},
	  [{flex, tn('$1')}]).
TExtName ->
    TAtom ':' TAtom :
    build(#typexp{type = module_qualifier},
	  [{tsub, '$1'}, {tlex, tn('$2')}, {tsub, '$3'}]).
Guards ->
    EDisj :
    '$1'.
Guards ->
    ExpGrd :
    '$1'.
EBinElem_binary_field@rep_esub ->
    EBinElem_binary_field@rep_esub '-' EBitT :
    ['$1', {elex, tn('$2')}, {esub, '$3'}].
EBinElem_binary_field@rep_esub ->
    EBitT :
    [{esub, '$1'}].
TUnion_union@rep_tsub ->
    TUnion_union@rep_tsub '|' Type :
    ['$1', {tlex, tn('$2')}, {tsub, '$3'}].
TUnion_union@rep_tsub ->
    Type :
    [{tsub, '$1'}].
FAttrib ->
    '-' atom '(' ExpAttr ')' stop :
    build(#form{type = attrib, paren = default,
		tag = tv('$2')},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {eattr, '$4'}, {flex, tn('$5')}, {flex, tn('$6')}]).
FAttrib ->
    '-' atom ExpNoPar stop :
    build(#form{type = attrib, paren = no, tag = tv('$2')},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {eattr, '$3'},
	   {flex, tn('$4')}]).
CFunExp ->
    '(' CFunExp_funexpr@rep_pattern ')' 'when' Guards '->' CFunExp_funexpr@rep_body :
    build(#clause{type = funexpr},
	  [{clex, tn('$1')}, '$2', {clex, tn('$3')},
	   {clex, tn('$4')}, {guard, '$5'}, {clex, tn('$6')},
	   '$7']).
CFunExp ->
    '(' CFunExp_funexpr@rep_pattern ')' '->' CFunExp_funexpr@rep_body :
    build(#clause{type = funexpr},
	  [{clex, tn('$1')}, '$2', {clex, tn('$3')},
	   {clex, tn('$4')}, '$5']).
CFunExp ->
    '(' ')' 'when' Guards '->' CFunExp_funexpr@rep_body :
    build(#clause{type = funexpr},
	  [{clex, tn('$1')}, {clex, tn('$2')}, {clex, tn('$3')},
	   {guard, '$4'}, {clex, tn('$5')}, '$6']).
CFunExp ->
    '(' ')' '->' CFunExp_funexpr@rep_body :
    build(#clause{type = funexpr},
	  [{clex, tn('$1')}, {clex, tn('$2')}, {clex, tn('$3')},
	   '$4']).
CAfter_timeout@rep_body ->
    CAfter_timeout@rep_body ',' Expr :
    ['$1', {clex, tn('$2')}, {body, '$3'}].
CAfter_timeout@rep_body ->
    Expr :
    [{body, '$1'}].
TFunSig ->
    TArgList '->' TypSpec :
    build(#typexp{type = fun_sig},
	  [{tsub, '$1'}, {tlex, tn('$2')}, {tsub, '$3'}]).
EFunName ->
    EAtom :
    '$1'.
EFunName ->
    EAllAtomOrVar ':' EAllAtomOrVar :
    build(#expr{type = infix_expr, value = ':'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EAList ->
    '[' EALstHead '|' ExpAttr ']' :
    build(#expr{type = cons},
	  [{elex, tn('$1')}, {esub, '$2'}, {elex, tn('$3')},
	   {esub, '$4'}, {elex, tn('$5')}]).
EAList ->
    '[' EALstHead ']' :
    build(#expr{type = cons},
	  [{elex, tn('$1')}, {esub, '$2'}, {elex, tn('$3')}]).
EAList ->
    '[' ']' :
    build(#expr{type = cons},
	  [{elex, tn('$1')}, {elex, tn('$2')}]).
EList ->
    '[' ELstHead '|' Expr ']' :
    build(#expr{type = cons},
	  [{elex, tn('$1')}, {esub, '$2'}, {elex, tn('$3')},
	   {esub, '$4'}, {elex, tn('$5')}]).
EList ->
    '[' ELstHead ']' :
    build(#expr{type = cons},
	  [{elex, tn('$1')}, {esub, '$2'}, {elex, tn('$3')}]).
EList ->
    '[' ']' :
    build(#expr{type = cons},
	  [{elex, tn('$1')}, {elex, tn('$2')}]).
TList ->
    '[' ']' :
    build(#typexp{type = list, tag = empty},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}]).
TList ->
    '[' TypSpec ']' :
    build(#typexp{type = list, tag = any},
	  [{tlex, tn('$1')}, {tsub, '$2'}, {tlex, tn('$3')}]).
TList ->
    '[' TypSpec ',' '.' '.' '.' ']' :
    build(#typexp{type = list, tag = nonempty},
	  [{tlex, tn('$1')}, {tsub, '$2'}, {tlex, tn('$3')},
	   {tlex, tn('$4')}, {tlex, tn('$5')}, {tlex, tn('$6')},
	   {tlex, tn('$7')}]).
TArgList_arglist@rep_tsub ->
    TArgList_arglist@rep_tsub ',' TypSpec :
    ['$1', {tlex, tn('$2')}, {tsub, '$3'}].
TArgList_arglist@rep_tsub ->
    TypSpec :
    [{tsub, '$1'}].
TypUnion ->
    TUnion :
    '$1'.
TypUnion ->
    Type :
    '$1'.
CBlock ->
    CBlock_block@rep_body :
    build(#clause{type = block}, ['$1']).
EBlock ->
    'begin' CBlock 'end' :
    build(#expr{type = block_expr},
	  [{elex, tn('$1')}, {exprcl, '$2'}, {elex, tn('$3')}]).
EDisj ->
    ExpGrd ';' Guards :
    build(#expr{type = infix_expr, value = ';'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
CGrd_guard@rep_body ->
    CGrd_guard@rep_body ',' Expr :
    ['$1', {clex, tn('$2')}, {body, '$3'}].
CGrd_guard@rep_body ->
    Expr :
    [{body, '$1'}].
ETry ->
    'try' CBlock 'of' ETry_try_expr@rep_exprcl 'catch' ETry_try_expr@rep_catchcl 'after' CBlock 'end' :
    build(#expr{type = try_expr},
	  [{elex, tn('$1')}, {headcl, '$2'}, {elex, tn('$3')},
	   '$4', {elex, tn('$5')}, '$6', {elex, tn('$7')},
	   {aftercl, '$8'}, {elex, tn('$9')}]).
ETry ->
    'try' CBlock 'of' ETry_try_expr@rep_exprcl 'catch' ETry_try_expr@rep_catchcl 'end' :
    build(#expr{type = try_expr},
	  [{elex, tn('$1')}, {headcl, '$2'}, {elex, tn('$3')},
	   '$4', {elex, tn('$5')}, '$6', {elex, tn('$7')}]).
ETry ->
    'try' CBlock 'of' ETry_try_expr@rep_exprcl 'after' CBlock 'end' :
    build(#expr{type = try_expr},
	  [{elex, tn('$1')}, {headcl, '$2'}, {elex, tn('$3')},
	   '$4', {elex, tn('$5')}, {aftercl, '$6'},
	   {elex, tn('$7')}]).
ETry ->
    'try' CBlock 'of' ETry_try_expr@rep_exprcl 'end' :
    build(#expr{type = try_expr},
	  [{elex, tn('$1')}, {headcl, '$2'}, {elex, tn('$3')},
	   '$4', {elex, tn('$5')}]).
ETry ->
    'try' CBlock 'catch' ETry_try_expr@rep_catchcl 'after' CBlock 'end' :
    build(#expr{type = try_expr},
	  [{elex, tn('$1')}, {headcl, '$2'}, {elex, tn('$3')},
	   '$4', {elex, tn('$5')}, {aftercl, '$6'},
	   {elex, tn('$7')}]).
ETry ->
    'try' CBlock 'catch' ETry_try_expr@rep_catchcl 'end' :
    build(#expr{type = try_expr},
	  [{elex, tn('$1')}, {headcl, '$2'}, {elex, tn('$3')},
	   '$4', {elex, tn('$5')}]).
ETry ->
    'try' CBlock 'after' CBlock 'end' :
    build(#expr{type = try_expr},
	  [{elex, tn('$1')}, {headcl, '$2'}, {elex, tn('$3')},
	   {aftercl, '$4'}, {elex, tn('$5')}]).
ETry ->
    'try' CBlock 'end' :
    build(#expr{type = try_expr},
	  [{elex, tn('$1')}, {headcl, '$2'}, {elex, tn('$3')}]).
ECatch ->
    'catch' CExp :
    build(#expr{type = catch_expr},
	  [{elex, tn('$1')}, {exprcl, '$2'}]).
EMatch ->
    Exp150 '=' Exp100 :
    build(#expr{type = match_expr},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ECase ->
    'case' CExp 'of' ECase_case_expr@rep_exprcl 'end' :
    build(#expr{type = case_expr},
	  [{elex, tn('$1')}, {headcl, '$2'}, {elex, tn('$3')},
	   '$4', {elex, tn('$5')}]).
EBinSize ->
    ExpBin1 ':' ExpMax :
    build(#expr{type = size_qualifier},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EBitT ->
    EAtom :
    '$1'.
EBitT ->
    EAllAtom ':' integer :
    build(#expr{type = bit_size_expr, value = tv('$3')},
	  [{esub, '$1'}, {elex, tn('$2')}, {elex, tn('$3')}]).
EArgList_arglist@rep_esub ->
    EArgList_arglist@rep_esub ',' Expr :
    ['$1', {elex, tn('$2')}, {esub, '$3'}].
EArgList_arglist@rep_esub ->
    Expr :
    [{esub, '$1'}].
CE160 ->
    Exp160 :
    build(#clause{type = expr}, [{body, '$1'}]).
Exp160 ->
    EAndalso :
    '$1'.
Exp160 ->
    Exp200 :
    '$1'.
TypFunction ->
    TSpecUnion :
    '$1'.
TypFunction ->
    TypFunSpec :
    '$1'.
CFunction ->
    EAtom '(' CFunction_fundef@rep_pattern ')' 'when' Guards '->' CFunction_fundef@rep_body :
    build(#clause{type = fundef},
	  [{name, '$1'}, {clex, tn('$2')}, '$3', {clex, tn('$4')},
	   {clex, tn('$5')}, {guard, '$6'}, {clex, tn('$7')},
	   '$8']).
CFunction ->
    EAtom '(' CFunction_fundef@rep_pattern ')' '->' CFunction_fundef@rep_body :
    build(#clause{type = fundef},
	  [{name, '$1'}, {clex, tn('$2')}, '$3', {clex, tn('$4')},
	   {clex, tn('$5')}, '$6']).
CFunction ->
    EAtom '(' ')' 'when' Guards '->' CFunction_fundef@rep_body :
    build(#clause{type = fundef},
	  [{name, '$1'}, {clex, tn('$2')}, {clex, tn('$3')},
	   {clex, tn('$4')}, {guard, '$5'}, {clex, tn('$6')},
	   '$7']).
CFunction ->
    EAtom '(' ')' '->' CFunction_fundef@rep_body :
    build(#clause{type = fundef},
	  [{name, '$1'}, {clex, tn('$2')}, {clex, tn('$3')},
	   {clex, tn('$4')}, '$5']).
FFunction ->
    FFunction_func@rep_funcl stop :
    build(#form{type = func}, ['$1', {flex, tn('$2')}]).
Form ->
    FModule :
    '$1'.
Form ->
    FExport :
    '$1'.
Form ->
    FImport :
    '$1'.
Form ->
    FTypDef :
    '$1'.
Form ->
    FRecord :
    '$1'.
Form ->
    FFunSpec :
    '$1'.
Form ->
    FFile :
    '$1'.
Form ->
    FAttrib :
    '$1'.
Form ->
    FFunction :
    '$1'.
Form ->
    FEmpty :
    '$1'.
ExpMax ->
    EParen :
    '$1'.
ExpMax ->
    ExpAtomic :
    '$1'.
ExpMax ->
    ExpConst :
    '$1'.
ExpMax ->
    ExpComp :
    '$1'.
CGrd ->
    Guards '->' CGrd_guard@rep_body :
    build(#clause{type = guard},
	  [{guard, '$1'}, {clex, tn('$2')}, '$3']).
EFloat ->
    float :
    build(#expr{type = float, value = tv('$1')},
	  [{elex, tn('$1')}]).
ExpGrd ->
    EConj :
    '$1'.
ExpGrd ->
    Expr :
    '$1'.
TGuard ->
    TTypVar :
    '$1'.
TGuard ->
    atom TArgList :
    build(#typexp{type = guard},
	  [{tlex, tn('$1')}, {tsub, '$2'}]).
EFilter ->
    CExp :
    build(#expr{type = filter}, [{exprcl, '$1'}]).
EVar ->
    variable :
    build(#expr{type = variable, value = tv('$1')},
	  [{elex, tn('$1')}]).
EVar ->
    '_' :
    build(#expr{type = joker}, [{elex, tn('$1')}]).
EChar ->
    char :
    build(#expr{type = char, value = tv('$1')},
	  [{elex, tn('$1')}]).
EIntOrVar ->
    EInt :
    '$1'.
EIntOrVar ->
    EVar :
    '$1'.
TVar ->
    variable :
    build(#typexp{type = variable, tag = tv('$1')},
	  [{tlex, tn('$1')}]).
TVar ->
    '_' :
    build(#typexp{type = joker}, [{tlex, tn('$1')}]).
CE150 ->
    Exp150 :
    build(#clause{type = expr}, [{body, '$1'}]).
Exp750 ->
    ERecord :
    '$1'.
Exp750 ->
    Exp800 :
    '$1'.
Exp150 ->
    EOrelse :
    '$1'.
Exp150 ->
    Exp160 :
    '$1'.
CCompr_compr@rep_body ->
    CCompr_compr@rep_body ',' ECompr :
    ['$1', {clex, tn('$2')}, {body, '$3'}].
CCompr_compr@rep_body ->
    ECompr :
    [{body, '$1'}].
TGrdFunSig ->
    TFunSig 'when' TypGuard :
    build(#typexp{type = spec_guard},
	  [{tsub, '$1'}, {tlex, tn('$2')}, {tsub, '$3'}]).
ECompr ->
    EFilter :
    '$1'.
ECompr ->
    ELstGen :
    '$1'.
ECompr ->
    EBinGen :
    '$1'.
CCompr ->
    CCompr_compr@rep_body :
    build(#clause{type = compr}, ['$1']).
EAllAtomOrVar ->
    EVar :
    '$1'.
EAllAtomOrVar ->
    EAllAtom :
    '$1'.
Expr ->
    ECatch :
    '$1'.
Expr ->
    Exp100 :
    '$1'.
TGrdList_guardlist@rep_tsub ->
    TGrdList_guardlist@rep_tsub ',' TGuard :
    ['$1', {tlex, tn('$2')}, {tsub, '$3'}].
TGrdList_guardlist@rep_tsub ->
    TGuard :
    [{tsub, '$1'}].
TPolySig ->
    '(' '.' '.' '.' ')' '->' TypSpec :
    build(#typexp{type = poly_sig},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}, {tlex, tn('$3')},
	   {tlex, tn('$4')}, {tlex, tn('$5')}, {tlex, tn('$6')},
	   {tsub, '$7'}]).
EReceive ->
    'receive' EReceive_receive_expr@rep_exprcl 'after' CAfter 'end' :
    build(#expr{type = receive_expr},
	  [{elex, tn('$1')}, '$2', {elex, tn('$3')},
	   {aftercl, '$4'}, {elex, tn('$5')}]).
EReceive ->
    'receive' EReceive_receive_expr@rep_exprcl 'end' :
    build(#expr{type = receive_expr},
	  [{elex, tn('$1')}, '$2', {elex, tn('$3')}]).
EReceive ->
    'receive' 'after' CAfter 'end' :
    build(#expr{type = receive_expr},
	  [{elex, tn('$1')}, {elex, tn('$2')}, {aftercl, '$3'},
	   {elex, tn('$4')}]).
EReceive ->
    'receive' 'end' :
    build(#expr{type = receive_expr},
	  [{elex, tn('$1')}, {elex, tn('$2')}]).
EATuple ->
    '{' EATuple_tuple@rep_esub '}' :
    build(#expr{type = tuple},
	  [{elex, tn('$1')}, '$2', {elex, tn('$3')}]).
EATuple ->
    '{' '}' :
    build(#expr{type = tuple},
	  [{elex, tn('$1')}, {elex, tn('$2')}]).
ERecordUpdate ->
    ERecordOrMax '#' atom EFldList :
    build(#expr{type = record_update, value = tv('$3')},
	  [{esub, '$1'}, {elex, tn('$2')}, {elex, tn('$3')},
	   {esub, '$4'}]).
TypName ->
    TAtom :
    '$1'.
TypName ->
    TExtName :
    '$1'.
Type ->
    TTuple :
    '$1'.
Type ->
    TRecord :
    '$1'.
Type ->
    TList :
    '$1'.
Type ->
    TBinary :
    '$1'.
Type ->
    TLimitInt :
    '$1'.
Type ->
    TVar :
    '$1'.
Type ->
    TAtom :
    '$1'.
Type ->
    TFunc :
    '$1'.
Type ->
    TParen :
    '$1'.
Type ->
    TCall :
    '$1'.
FModule ->
    '-' module '(' atom ')' stop :
    build(#form{type = module, paren = default,
		tag = tv('$4')},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {flex, tn('$4')}, {flex, tn('$5')}, {flex, tn('$6')}]).
FModule ->
    '-' module atom stop :
    build(#form{type = module, paren = no, tag = tv('$3')},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {flex, tn('$4')}]).
ExpConst ->
    ETuple :
    '$1'.
ExpConst ->
    EList :
    '$1'.
ExpConst ->
    EBinary :
    '$1'.
TLimitInt ->
    TSgnInt :
    '$1'.
TLimitInt ->
    TSgnInt '.' '.' TSgnInt :
    build(#typexp{type = interval},
	  [{tsub, '$1'}, {tlex, tn('$2')}, {tlex, tn('$3')},
	   {tsub, '$4'}]).
ERecordAccess ->
    ERecordOrMax '#' atom '.' EAtom :
    build(#expr{type = record_access, value = tv('$3')},
	  [{esub, '$1'}, {elex, tn('$2')}, {elex, tn('$3')},
	   {elex, tn('$4')}, {esub, '$5'}]).
TFunArgs ->
    '(' TFunArgs_varlist@rep_tsub ')' :
    build(#typexp{type = varlist},
	  [{tlex, tn('$1')}, '$2', {tlex, tn('$3')}]).
TFunArgs ->
    '(' ')' :
    build(#typexp{type = varlist},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}]).
EBinOp ->
    '+' ExpMax :
    build(#expr{type = prefix_bit_expr, value = '+'},
	  [{elex, tn('$1')}, {esub, '$2'}]).
EBinOp ->
    '-' ExpMax :
    build(#expr{type = prefix_bit_expr, value = '-'},
	  [{elex, tn('$1')}, {esub, '$2'}]).
EBinOp ->
    'bnot' ExpMax :
    build(#expr{type = prefix_bit_expr, value = 'bnot'},
	  [{elex, tn('$1')}, {esub, '$2'}]).
EBinOp ->
    'not' ExpMax :
    build(#expr{type = prefix_bit_expr, value = 'not'},
	  [{elex, tn('$1')}, {esub, '$2'}]).
EPreOp ->
    '+' Exp700 :
    build(#expr{type = prefix_expr, value = '+'},
	  [{elex, tn('$1')}, {esub, '$2'}]).
EPreOp ->
    '-' Exp700 :
    build(#expr{type = prefix_expr, value = '-'},
	  [{elex, tn('$1')}, {esub, '$2'}]).
EPreOp ->
    'bnot' Exp700 :
    build(#expr{type = prefix_expr, value = 'bnot'},
	  [{elex, tn('$1')}, {esub, '$2'}]).
EPreOp ->
    'not' Exp700 :
    build(#expr{type = prefix_expr, value = 'not'},
	  [{elex, tn('$1')}, {esub, '$2'}]).
EMulOp ->
    Exp500 '/' Exp600 :
    build(#expr{type = infix_expr, value = '/'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EMulOp ->
    Exp500 '*' Exp600 :
    build(#expr{type = infix_expr, value = '*'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EMulOp ->
    Exp500 'div' Exp600 :
    build(#expr{type = infix_expr, value = 'div'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EMulOp ->
    Exp500 'rem' Exp600 :
    build(#expr{type = infix_expr, value = 'rem'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EMulOp ->
    Exp500 'band' Exp600 :
    build(#expr{type = infix_expr, value = 'band'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EMulOp ->
    Exp500 'and' Exp600 :
    build(#expr{type = infix_expr, value = 'and'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EAddOp ->
    Exp400 '+' Exp500 :
    build(#expr{type = infix_expr, value = '+'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EAddOp ->
    Exp400 '-' Exp500 :
    build(#expr{type = infix_expr, value = '-'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EAddOp ->
    Exp400 'bor' Exp500 :
    build(#expr{type = infix_expr, value = 'bor'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EAddOp ->
    Exp400 'bxor' Exp500 :
    build(#expr{type = infix_expr, value = 'bxor'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EAddOp ->
    Exp400 'bsl' Exp500 :
    build(#expr{type = infix_expr, value = 'bsl'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EAddOp ->
    Exp400 'bsr' Exp500 :
    build(#expr{type = infix_expr, value = 'bsr'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EAddOp ->
    Exp400 'or' Exp500 :
    build(#expr{type = infix_expr, value = 'or'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EAddOp ->
    Exp400 'xor' Exp500 :
    build(#expr{type = infix_expr, value = 'xor'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ELstOp ->
    Exp400 '++' Exp300 :
    build(#expr{type = infix_expr, value = '++'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ELstOp ->
    Exp400 '--' Exp300 :
    build(#expr{type = infix_expr, value = '--'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ECmpOp ->
    Exp300 '==' Exp300 :
    build(#expr{type = infix_expr, value = '=='},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ECmpOp ->
    Exp300 '/=' Exp300 :
    build(#expr{type = infix_expr, value = '/='},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ECmpOp ->
    Exp300 '=<' Exp300 :
    build(#expr{type = infix_expr, value = '=<'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ECmpOp ->
    Exp300 '<' Exp300 :
    build(#expr{type = infix_expr, value = '<'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ECmpOp ->
    Exp300 '>=' Exp300 :
    build(#expr{type = infix_expr, value = '>='},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ECmpOp ->
    Exp300 '>' Exp300 :
    build(#expr{type = infix_expr, value = '>'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ECmpOp ->
    Exp300 '=:=' Exp300 :
    build(#expr{type = infix_expr, value = '=:='},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ECmpOp ->
    Exp300 '=/=' Exp300 :
    build(#expr{type = infix_expr, value = '=/='},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
EColon ->
    Exp900 ':' Exp750 :
    build(#expr{type = infix_expr, value = ':'},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
TUnion ->
    Type '|' TUnion_union@rep_tsub :
    build(#typexp{type = union},
	  [{tsub, '$1'}, {tlex, tn('$2')}, '$3']).
CFunction_fundef@rep_pattern ->
    CFunction_fundef@rep_pattern ',' Expr :
    ['$1', {clex, tn('$2')}, {pattern, '$3'}].
CFunction_fundef@rep_pattern ->
    Expr :
    [{pattern, '$1'}].
EAtom ->
    atom :
    build(#expr{type = atom, value = tv('$1')},
	  [{elex, tn('$1')}]).
EXAtom ->
    EXAtom1 :
    '$1'.
EXAtom ->
    EXAtom2 :
    '$1'.
TAtom ->
    atom :
    build(#typexp{type = atom, tag = tv('$1')},
	  [{tlex, tn('$1')}]).
EFunc_fun_expr@rep_exprcl ->
    EFunc_fun_expr@rep_exprcl ';' CFunExp :
    ['$1', {elex, tn('$2')}, {exprcl, '$3'}].
EFunc_fun_expr@rep_exprcl ->
    CFunExp :
    [{exprcl, '$1'}].
CFunction_fundef@rep_body ->
    CFunction_fundef@rep_body ',' Expr :
    ['$1', {clex, tn('$2')}, {body, '$3'}].
CFunction_fundef@rep_body ->
    Expr :
    [{body, '$1'}].
EInt ->
    integer :
    build(#expr{type = integer, value = tv('$1')},
	  [{elex, tn('$1')}]).
ExpAConst ->
    EATuple :
    '$1'.
ExpAConst ->
    EAList :
    '$1'.
ExpAConst ->
    EBinary :
    '$1'.
TInt ->
    integer :
    build(#typexp{type = integer, tag = tv('$1')},
	  [{tlex, tn('$1')}]).
TSpecUnion_spec_union@rep_tsub ->
    TSpecUnion_spec_union@rep_tsub ';' TypFunSpec :
    ['$1', {tlex, tn('$2')}, {tsub, '$3'}].
TSpecUnion_spec_union@rep_tsub ->
    TypFunSpec :
    [{tsub, '$1'}].
TFunArgs_varlist@rep_tsub ->
    TFunArgs_varlist@rep_tsub ',' TVar :
    ['$1', {tlex, tn('$2')}, {tsub, '$3'}].
TFunArgs_varlist@rep_tsub ->
    TVar :
    [{tsub, '$1'}].
ExpBin1 ->
    EBinOp :
    '$1'.
ExpBin1 ->
    ExpMax :
    '$1'.
ExpBin0 ->
    EBinSize :
    '$1'.
ExpBin0 ->
    ExpBin1 :
    '$1'.
ETry_try_expr@rep_catchcl ->
    ETry_try_expr@rep_catchcl ';' CPattern :
    ['$1', {elex, tn('$2')}, {catchcl, '$3'}].
ETry_try_expr@rep_catchcl ->
    CPattern :
    [{catchcl, '$1'}].
ETry_try_expr@rep_exprcl ->
    ETry_try_expr@rep_exprcl ';' CPattern :
    ['$1', {elex, tn('$2')}, {exprcl, '$3'}].
ETry_try_expr@rep_exprcl ->
    CPattern :
    [{exprcl, '$1'}].
EConj ->
    Expr ',' ExpGrd :
    build(#expr{type = infix_expr, value = ','},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ESend ->
    Exp150 '!' Exp100 :
    build(#expr{type = send_expr},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
TypGuard ->
    TGuard :
    '$1'.
TypGuard ->
    TGrdList :
    '$1'.
EFunc ->
    'fun' EFunc_fun_expr@rep_exprcl 'end' :
    build(#expr{type = fun_expr},
	  [{elex, tn('$1')}, '$2', {elex, tn('$3')}]).
EFunc ->
    'fun' EFunName '/' EIntOrVar :
    build(#expr{type = implicit_fun},
	  [{elex, tn('$1')}, {esub, '$2'}, {elex, tn('$3')},
	   {esub, '$4'}]).
TFunc ->
    'fun' '(' ')' :
    build(#typexp{type = func, tag = any},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}, {tlex, tn('$3')}]).
TFunc ->
    'fun' '(' TypFunSig ')' :
    build(#typexp{type = func, tag = sig},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}, {tsub, '$3'},
	   {tlex, tn('$4')}]).
ELstCompr ->
    '[' CHExp '||' CCompr ']' :
    build(#expr{type = list_comp},
	  [{elex, tn('$1')}, {exprcl, '$2'}, {elex, tn('$3')},
	   {exprcl, '$4'}, {elex, tn('$5')}]).
EXAtom2 ->
    EAtom EXAtom1 :
    build(#expr{type = xatom2},
	  [{esub, '$1'}, {esub, '$2'}]).
ExpNoPar ->
    ExpAtomic :
    '$1'.
ExpNoPar ->
    ExpAConst :
    '$1'.
ExpNoPar ->
    EAFunRef :
    '$1'.
FRecord_record_default@rep_tattr ->
    FRecord_record_default@rep_tattr ',' TFldSpec :
    ['$1', {flex, tn('$2')}, {tattr, '$3'}].
FRecord_record_default@rep_tattr ->
    TFldSpec :
    [{tattr, '$1'}].
EXAtom1 ->
    '.' EAtom EXAtom1 :
    build(#expr{type = xatom},
	  [{elex, tn('$1')}, {esub, '$2'}, {esub, '$3'}]).
EXAtom1 ->
    '.' EAtom :
    build(#expr{type = xatom},
	  [{elex, tn('$1')}, {esub, '$2'}]).
TSpecUnion ->
    TypFunSpec ';' TSpecUnion_spec_union@rep_tsub :
    build(#typexp{type = spec_union},
	  [{tsub, '$1'}, {tlex, tn('$2')}, '$3']).
EFldList ->
    '{' EFldList_field_list@rep_esub '}' :
    build(#expr{type = field_list},
	  [{elex, tn('$1')}, '$2', {elex, tn('$3')}]).
EFldList ->
    '{' '}' :
    build(#expr{type = field_list},
	  [{elex, tn('$1')}, {elex, tn('$2')}]).
EArgList ->
    '(' EArgList_arglist@rep_esub ')' :
    build(#expr{type = arglist},
	  [{elex, tn('$1')}, '$2', {elex, tn('$3')}]).
EArgList ->
    '(' ')' :
    build(#expr{type = arglist},
	  [{elex, tn('$1')}, {elex, tn('$2')}]).
TBinSpec ->
    TVar ':' TInt :
    build(#typexp{type = bin_base},
	  [{tsub, '$1'}, {tlex, tn('$2')}, {tsub, '$3'}]).
TBinSpec ->
    TVar ':' TVar '*' TInt :
    build(#typexp{type = bin_unit},
	  [{tsub, '$1'}, {tlex, tn('$2')}, {tsub, '$3'},
	   {tlex, tn('$4')}, {tsub, '$5'}]).
TFldSpec ->
    atom '=' Expr '::' TypSpec :
    build(#typexp{type = spec_field, tag = tv('$1')},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}, {texpr, '$3'},
	   {tlex, tn('$4')}, {tsub, '$5'}]).
TFldSpec ->
    atom '=' Expr :
    build(#typexp{type = spec_field, tag = tv('$1')},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}, {texpr, '$3'}]).
TFldSpec ->
    atom '::' TypSpec :
    build(#typexp{type = spec_field, tag = tv('$1')},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}, {tsub, '$3'}]).
TFldSpec ->
    atom :
    build(#typexp{type = spec_field, tag = tv('$1')},
	  [{tlex, tn('$1')}]).
EATuple_tuple@rep_esub ->
    EATuple_tuple@rep_esub ',' ExpAttr :
    ['$1', {elex, tn('$2')}, {esub, '$3'}].
EATuple_tuple@rep_esub ->
    ExpAttr :
    [{esub, '$1'}].
ETuple_tuple@rep_esub ->
    ETuple_tuple@rep_esub ',' Expr :
    ['$1', {elex, tn('$2')}, {esub, '$3'}].
ETuple_tuple@rep_esub ->
    Expr :
    [{esub, '$1'}].
EALstHead_list@rep_esub ->
    EALstHead_list@rep_esub ',' ExpAttr :
    ['$1', {elex, tn('$2')}, {esub, '$3'}].
EALstHead_list@rep_esub ->
    ExpAttr :
    [{esub, '$1'}].
ELstHead_list@rep_esub ->
    ELstHead_list@rep_esub ',' Expr :
    ['$1', {elex, tn('$2')}, {esub, '$3'}].
ELstHead_list@rep_esub ->
    Expr :
    [{esub, '$1'}].
TTuple_tuple@rep_tsub ->
    TTuple_tuple@rep_tsub ',' TypSpec :
    ['$1', {tlex, tn('$2')}, {tsub, '$3'}].
TTuple_tuple@rep_tsub ->
    TypSpec :
    [{tsub, '$1'}].
CE200 ->
    Exp200 :
    build(#clause{type = expr}, [{body, '$1'}]).
Exp900 ->
    EXAtom :
    '$1'.
Exp900 ->
    ExpMax :
    '$1'.
Exp800 ->
    EColon :
    '$1'.
Exp800 ->
    Exp900 :
    '$1'.
Exp700 ->
    ECall :
    '$1'.
Exp700 ->
    Exp750 :
    '$1'.
Exp600 ->
    EPreOp :
    '$1'.
Exp600 ->
    Exp700 :
    '$1'.
Exp500 ->
    EMulOp :
    '$1'.
Exp500 ->
    Exp600 :
    '$1'.
Exp400 ->
    EAddOp :
    '$1'.
Exp400 ->
    Exp500 :
    '$1'.
Exp300 ->
    ELstOp :
    '$1'.
Exp300 ->
    Exp400 :
    '$1'.
Exp200 ->
    ECmpOp :
    '$1'.
Exp200 ->
    Exp300 :
    '$1'.
Exp100 ->
    EMatch :
    '$1'.
Exp100 ->
    ESend :
    '$1'.
Exp100 ->
    Exp150 :
    '$1'.
EBinElem ->
    ExpBin0 '/' EBinElem_binary_field@rep_esub :
    build(#expr{type = binary_field},
	  [{esub, '$1'}, {elex, tn('$2')}, '$3']).
EBinElem ->
    ExpBin0 :
    build(#expr{type = binary_field}, [{esub, '$1'}]).
ECase_case_expr@rep_exprcl ->
    ECase_case_expr@rep_exprcl ';' CPattern :
    ['$1', {elex, tn('$2')}, {exprcl, '$3'}].
ECase_case_expr@rep_exprcl ->
    CPattern :
    [{exprcl, '$1'}].
ECall ->
    Exp800 EArgList :
    build(#expr{type = application},
	  [{esub, '$1'}, {esub, '$2'}]).
TCall ->
    TypName TArgList :
    build(#typexp{type = call},
	  [{tsub, '$1'}, {tsub, '$2'}]).
FFunction_func@rep_funcl ->
    FFunction_func@rep_funcl ';' CFunction :
    ['$1', {flex, tn('$2')}, {funcl, '$3'}].
FFunction_func@rep_funcl ->
    CFunction :
    [{funcl, '$1'}].
EAFunRef ->
    EAtom '/' EInt :
    build(#expr{type = funref},
	  [{esub, '$1'}, {elex, tn('$2')}, {esub, '$3'}]).
ETuple ->
    '{' ETuple_tuple@rep_esub '}' :
    build(#expr{type = tuple},
	  [{elex, tn('$1')}, '$2', {elex, tn('$3')}]).
ETuple ->
    '{' '}' :
    build(#expr{type = tuple},
	  [{elex, tn('$1')}, {elex, tn('$2')}]).
TTuple ->
    '{' TTuple_tuple@rep_tsub '}' :
    build(#typexp{type = tuple},
	  [{tlex, tn('$1')}, '$2', {tlex, tn('$3')}]).
TTuple ->
    '{' '}' :
    build(#typexp{type = tuple},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}]).
FFile ->
    '-' file '(' EString ',' EInt ')' stop :
    build(#form{type = file, paren = default},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {eattr, '$4'}, {flex, tn('$5')}, {eattr, '$6'},
	   {flex, tn('$7')}, {flex, tn('$8')}]).
FFile ->
    '-' file EString ',' EInt stop :
    build(#form{type = file, paren = no},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {eattr, '$3'},
	   {flex, tn('$4')}, {eattr, '$5'}, {flex, tn('$6')}]).
EField ->
    atom '=' Expr :
    build(#expr{type = record_field, value = tv('$1')},
	  [{elex, tn('$1')}, {elex, tn('$2')}, {esub, '$3'}]).
EField ->
    atom :
    build(#expr{type = record_field, value = tv('$1')},
	  [{elex, tn('$1')}]).
EField ->
    '_' '=' Expr :
    build(#expr{type = record_joker_field},
	  [{elex, tn('$1')}, {elex, tn('$2')}, {esub, '$3'}]).
TField ->
    TAtom '::' TypSpec :
    build(#typexp{type = field},
	  [{tsub, '$1'}, {tlex, tn('$2')}, {tsub, '$3'}]).
TGrdList ->
    TGuard ',' TGrdList_guardlist@rep_tsub :
    build(#typexp{type = guardlist},
	  [{tsub, '$1'}, {tlex, tn('$2')}, '$3']).
TArgList ->
    '(' TArgList_arglist@rep_tsub ')' :
    build(#typexp{type = arglist},
	  [{tlex, tn('$1')}, '$2', {tlex, tn('$3')}]).
TArgList ->
    '(' ')' :
    build(#typexp{type = arglist},
	  [{tlex, tn('$1')}, {tlex, tn('$2')}]).
CFunExp_funexpr@rep_pattern ->
    CFunExp_funexpr@rep_pattern ',' Expr :
    ['$1', {clex, tn('$2')}, {pattern, '$3'}].
CFunExp_funexpr@rep_pattern ->
    Expr :
    [{pattern, '$1'}].
CBlock_block@rep_body ->
    CBlock_block@rep_body ',' Expr :
    ['$1', {clex, tn('$2')}, {body, '$3'}].
CBlock_block@rep_body ->
    Expr :
    [{body, '$1'}].
CPBinary ->
    EBinary :
    build(#clause{type = pexpr}, [{pattern, '$1'}]).
EALstHead ->
    EALstHead_list@rep_esub :
    build(#expr{type = list}, ['$1']).
FFunSpec ->
    '-' spec TypName TypFunction stop :
    build(#form{type = spec, paren = default, tag = name},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {tattr, '$3'},
	   {tattr, '$4'}, {flex, tn('$5')}]).
FFunSpec ->
    '-' spec '(' TypName TypFunction ')' stop :
    build(#form{type = spec, paren = yes, tag = name},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {tattr, '$4'}, {tattr, '$5'}, {flex, tn('$6')},
	   {flex, tn('$7')}]).
FFunSpec ->
    '-' spec TFunRef '::' TypFunction stop :
    build(#form{type = spec, paren = default, tag = ref},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {tattr, '$3'},
	   {flex, tn('$4')}, {tattr, '$5'}, {flex, tn('$6')}]).
FFunSpec ->
    '-' spec '(' TFunRef '::' TypFunction ')' stop :
    build(#form{type = spec, paren = yes, tag = ref},
	  [{flex, tn('$1')}, {flex, tn('$2')}, {flex, tn('$3')},
	   {tattr, '$4'}, {flex, tn('$5')}, {tattr, '$6'},
	   {flex, tn('$7')}, {flex, tn('$8')}]).
TRecord_record@rep_tsub ->
    TRecord_record@rep_tsub ',' TField :
    ['$1', {tlex, tn('$2')}, {tsub, '$3'}].
TRecord_record@rep_tsub ->
    TField :
    [{tsub, '$1'}].
EAndalso ->
    CE200 'andalso' CE160 :
    build(#expr{type = infix_expr, value = 'andalso'},
	  [{exprcl, '$1'}, {elex, tn('$2')}, {exprcl, '$3'}]).
CPattern ->
    Expr 'when' Guards '->' CPattern_pattern@rep_body :
    build(#clause{type = pattern},
	  [{pattern, '$1'}, {clex, tn('$2')}, {guard, '$3'},
	   {clex, tn('$4')}, '$5']).
CPattern ->
    Expr '->' CPattern_pattern@rep_body :
    build(#clause{type = pattern},
	  [{pattern, '$1'}, {clex, tn('$2')}, '$3']).
CPattern_pattern@rep_body ->
    CPattern_pattern@rep_body ',' Expr :
    ['$1', {clex, tn('$2')}, {body, '$3'}].
CPattern_pattern@rep_body ->
    Expr :
    [{body, '$1'}].
ERecordIdx ->
    '#' atom '.' EAtom :
    build(#expr{type = record_index, value = tv('$2')},
	  [{elex, tn('$1')}, {elex, tn('$2')}, {elex, tn('$3')},
	   {esub, '$4'}]).
EString ->
    string :
    build(#expr{type = string, value = tv('$1')},
	  [{elex, tn('$1')}]).
EString ->
    string EString :
    build(#expr{type = mstring, value = tv('$1')},
	  [{elex, tn('$1')}, {esub, '$2'}]).
TSgnInt ->
    TInt :
    '$1'.
TSgnInt ->
    '-' TInt :
    build(#typexp{type = negate},
	  [{tlex, tn('$1')}, {tsub, '$2'}]).
ExpAtomic ->
    EString :
    '$1'.
ExpAtomic ->
    EAtom :
    '$1'.
ExpAtomic ->
    EChar :
    '$1'.
ExpAtomic ->
    EInt :
    '$1'.
ExpAtomic ->
    EVar :
    '$1'.
ExpAtomic ->
    EFloat :
    '$1'.
EFldList_field_list@rep_esub ->
    EFldList_field_list@rep_esub ',' EField :
    ['$1', {elex, tn('$2')}, {esub, '$3'}].
EFldList_field_list@rep_esub ->
    EField :
    [{esub, '$1'}].
ExpComp ->
    EBlock :
    '$1'.
ExpComp ->
    EReceive :
    '$1'.
ExpComp ->
    EIf :
    '$1'.
ExpComp ->
    ECase :
    '$1'.
ExpComp ->
    EFunc :
    '$1'.
ExpComp ->
    ETry :
    '$1'.
ExpComp ->
    ELstCompr :
    '$1'.
ExpComp ->
    EBinCompr :
    '$1'.
Erlang code.

-vsn("$Rev: 5585 $").
-include("core.hrl").
-import(?Syn, [build/2]).
tv({_, _, {T=#token{}, _}}) -> reflib_token:get_value(T).
tn({_, _, {_, N}}) -> N.

