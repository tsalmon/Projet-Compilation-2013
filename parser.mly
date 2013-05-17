%{
  (** This module implements syntactic analysis. *)

  open AST

  let parse_error = Error.error "during parsing"

%}


%token RPAREN LPAREN RCROCH LCROCH RACOLA LACOLA LCHEVR RCHEVR TYPE FLECH FLECHB AND OR NOT DEFFUN REC IS VAL DEF WITH PZERO PONE EOF
%token DEFTYPE COMMA POINTCOMMA IN WHERE END POINT BAR
%token CASE AT IF THEN ELSE FUN DO 
%token<string> VAR_OR_TYPE CONSTR STRING OPAND OPOR OPCOMP PLUS MULT DIV MOD MINUS EQ EQS TILDE
%token<char> CHAR
%token<int>INT
%start<AST.program> program

%left OR
%left AND


%left MULT DIV MOD
%left PLUS MINUS 
%left OPCOMP LCHEVR RCHEVR
%left OPAND
%left OPOR
%left EQ

%%

program: d=definitions EOF {d}
    

definitions: 
d=definition* { d }


definition:  (* DVal | DType *)
(* Dval => vdefinition *)
x=vdefinition { DVal x } 
(* DType => type type_id = type *)
| TYPE v=type_identifier EQ t=typ { DType (v,[],t) } 
(* DType => type type_id  < type_id {l , type_id }l >  = type *)
| TYPE v=type_identifier LCHEVR l=type_identifiers RCHEVR EQ t=typ { DType (v,l,t) } 
 

vdefinition:
VAL b=binding EQ e=expr{ Simple(b,e) }
| DEF m=mutuallyR { MutuallyRecursive (m) }

mutuallyR:
v=argument_identifier  arg=arguments* t=deftyp? EQ e=expr WITH m=mutuallyR { (Binding(v,None),Sugar.mk_fundef arg t e )::m} 
| v=argument_identifier  arg=arguments* t=deftyp? EQ e=expr { [(Binding(v,None),Sugar.mk_fundef arg t e)]} 

deftyp:
DEFTYPE t=typ {t}

arguments:
LPAREN b=binding RPAREN {b}

binding:
a=argument_identifier DEFTYPE t=typ {Binding(a,Some t)}
| a=argument_identifier { Binding(a,None) }

argument_identifier:
a=value_identifier { Named a}
|{Unnamed}


typ:
i=type_identifier LCHEVR t=types RCHEVR { TVar(i,t) } (* types est non optionelle, or dans la definition de la grammaire il l'est *)
| i=type_identifier { TVar(i,[]) } 
| t=typ FLECH y=typ { TArrow(t,y) }
| LACOLA c=constructor_types_sum RACOLA { TSum c } (* TSum et TProd indetermination , separtion du construtor en deux definition *) 
| LACOLA c=constructor_types_prod RACOLA { TProd c }
| LACOLA c=constructor_type LACOLA{ TSum [c] }
| LACOLA RACOLA { TSum [] } 
| REC i=type_identifier IS t=typ { TRec(i,t) }
| LPAREN t=typ RPAREN { t }



types:
t=typ COMMA l=types { t::l }
|t=typ {[t]}


constructor_types_sum:
c=constructor_type PLUS l=constructor_types_sum { c::l }
|c=constructor_type {[c]}

constructor_types_prod:
c=constructor_type MULT l=constructor_types_prod { c::l }
|c=constructor_type {[c]}

constructor_type: 
i=constructor_identifier t=typ? { TConstructor(i,t) } 




expr:  (* ambiguité on separe sequencement des autres expr pour ne pas que sa boucle *)
e=exprseq {e}

| e=expr POINT r=expr { Sugar.mk_postfix_application e r }
| e=expr r=exprprincipal  { EApp (e,r) }

| AT t=typ LACOLA c=constructor_definitions RACOLA { EProd(Some t,c) }
| LACOLA c=constructor_definitions RACOLA { EProd(None,c) }

| FUN DEFFUN e=expr { Sugar.mk_fun [ Binding (Unnamed, Some (TSum [TConstructor ( CIdentifier "U", None)])) ] None e} (* sugar mis a l'arache car il marchai pas *)
| FUN arg=arguments+ DEFTYPE t=typ? DEFFUN e=expr { Sugar.mk_fun arg t e}
| FUN arg=arguments+ DEFFUN e=expr { Sugar.mk_fun arg None e}

| IF a=expr THEN b=expr { Sugar.mk_ifthen a b }
| IF a=expr THEN b=expr ELSE c=expr{ Sugar.mk_ifthenelse a b c }

| v=vdefinition IN e=expr { EDef (v,e) }
| e=expr WHERE v=vdefinition END { Sugar.mk_where e v }

|e=exprprincipal{e}


exprprincipal: 
e =exprfinal {e}

| c=constructor_identifier AT t=typ e=crochexpr? { ESum(c,Some t,e) }
(*| c=constructor_identifier AT t=typ { ESum(c,Some t,None) }*)
| c=constructor_identifier e=crochexpr? { ESum(c,None,e) }
(*| c=constructor_identifier { ESum(c,None,None) }*)

| LPAREN e=expr RPAREN { e }
| LPAREN e=expr DEFTYPE t=typ RPAREN { EAnnot(e,t) }


| e=expropnivA {e}

| e=exprunopminus {e}
| e=exprunoptilde {e}

|  CASE AT t=typ LACOLA BAR? b=branches RACOLA { ECase(Some t,b) }
| CASE LACOLA BAR? b=branches RACOLA { ECase(None,b) }



| DO LACOLA e=expr RACOLA { Sugar.mk_do e } 



exprseq:  
e=seq { ESeq e }

expropnivA:
| a=expropnivA EQS b=expropnivA { EApp(EApp(EVar (Identifier ":="),a),b) }
|e=expropnivB{e}

expropnivB:
| a=expropnivB OPOR b=expropnivB { EApp(EApp(EVar (Identifier "||"),a),b) }
|e=expropnivC{e}

expropnivC:
| a=expropnivC OPAND b=expropnivC { EApp(EApp(EVar (Identifier "&&"),a),b) }
|e=expropnivD{e}

expropnivD:
| a=expropnivD o=OPCOMP b=expropnivD { EApp(EApp(EVar (Identifier o),a),b) }
| a=expropnivD EQ b=expropnivD { EApp(EApp(EVar (Identifier "="),a),b) }
| a=expropnivD LCHEVR b=expropnivD { EApp(EApp(EVar (Identifier "<"),a),b) }
| a=expropnivD RCHEVR b=expropnivD { EApp(EApp(EVar (Identifier ">"),a),b) }
| e=exprunoptilde {e}
|e=expropnivE{e}

expropnivE:
| a=expropnivE PLUS b=expropnivE { EApp(EApp(EVar (Identifier "+"),a),b) }
| a=expropnivE MINUS b=expropnivE { EApp(EApp(EVar (Identifier "-"),a),b) }
|e=expropnivF{e}

expropnivF:
| LPAREN e=expropnivA RPAREN {e}
| a=expropnivF MULT b=expropnivF { EApp(EApp(EVar (Identifier "*"),a),b) }
| a=expropnivF DIV b=expropnivF { EApp(EApp(EVar (Identifier "/"),a),b) }
| a=expropnivF MOD b=expropnivF { EApp(EApp(EVar (Identifier "%"),a),b) }
| e=exprunopminus {e}
| e=exprfinal {e}


exprfinal:
 e=INT { EInt e}
| e=CHAR { EChar e}
| e=STRING { EString e}
| e=value_identifier { EVar e }

exprunopminus:
 MINUS e=INT { EApp(EVar (Identifier "-"),EInt e) }
(* MINUS e=INT %prec Unop{ EApp(EVar (Identifier "-"),EInt e) }*)
(*| MINUS e=value_identifier %prec Unop{ EApp(EVar (Identifier "-"),EVar e) }*)
| MINUS e=value_identifier { EApp(EVar (Identifier "-"),EVar e) }
(*| MINUS LPAREN e=expr RPAREN %prec Unop{ EApp(EVar (Identifier "-"), e) }*)
| MINUS LPAREN e=expr RPAREN{ EApp(EVar (Identifier "-"), e) }
exprunoptilde:
(* TILDE e=INT %prec Unop{ EApp(EVar (Identifier "~"),EInt e) }*)
 TILDE e=INT{ EApp(EVar (Identifier "~"),EInt e) }
(*| TILDE e=value_identifier %prec Unop{ EApp(EVar (Identifier "~"),EVar e) }*)
| TILDE e=value_identifier { EApp(EVar (Identifier "~"),EVar e) }
| TILDE LPAREN e=expr RPAREN { EApp(EVar (Identifier "~"), e) }
(*| TILDE LPAREN e=expr RPAREN %prec Unop{ EApp(EVar (Identifier "~"), e) }*)

crochexpr:
LCROCH e=expr RCROCH { e }

seq:
e=exprprincipal POINTCOMMA s=seq { e::s }
|LPAREN e=seq RPAREN{ e }
|e=exprprincipal  { [e] }

constructor_definition:
c=constructor_identifier FLECHB e=expr { (c,Some e) }
|c=constructor_identifier { (c,None) }

constructor_definitions:
c=constructor_definition COMMA l=constructor_definitions { c::l }
|c=constructor_definition {[c]}

branche:
p=pattern DEFFUN e=expr { Branch(p,e) }

branches:
b=branche BAR l=branches {b::l}
|b=branche {[b]}

pattern:
c=constructor_identifier AT t=typ LCROCH p=pattern RCROCH { PSum(c,Some t,Some p) } 
| c=constructor_identifier LCROCH p=pattern RCROCH { PSum(c,None,Some p) }
| c=constructor_identifier AT t=typ  { PSum(c,Some t,None) }
| c=constructor_identifier { PSum(c,None,None) }
| LACOLA c=patternproduits RACOLA{ PProd (None,c) }
| AT t=typ LACOLA c=patternproduits RACOLA{ PProd (Some t,c) }
| p=patternopnivA {p}
| LPAREN p=pattern RPAREN { p }
| p=patternfinal {p}

patternopnivA:
 a=patternopnivA OR b=patternopnivA { POr (a,b) }
| p=patternopnivB {p}

patternopnivB:
 a=patternopnivB AND b=patternopnivB  { PAnd (a,b) }
| p=patternopnivC {p}

patternopnivC:
LPAREN p=patternopnivA RPAREN {p}
| NOT p=patternopnivC { PNot p }
| p=patternfinal {p}

patternfinal:
 PZERO { PZero }
| p=value_identifier { PVar p }
| PONE { POne }

patternproduits:
c=patternproduit COMMA l=patternproduits { c::l }
|c=patternproduit { [c] }

patternproduit:
c=constructor_identifier FLECH p=pattern { (c,Some p) }
|c=constructor_identifier { (c,None) }



value_identifier:
i=VAR_OR_TYPE { Identifier i }

constructor_identifier:
i=CONSTR { CIdentifier i }

(*%inline unop:
i=MINUS { Identifier i }
| i=TILDE { Identifier i }
*)

type_identifier:
i=VAR_OR_TYPE { TIdentifier i }

type_identifiers:
t=type_identifier COMMA l=type_identifiers { t::l }
|t=type_identifier { [t] }

