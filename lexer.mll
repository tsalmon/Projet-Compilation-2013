{
 (** This module implements lexical analysis. *)
  open Parser
  open Char
 }

let layout = [' ' '\n' '\t']
let type_id_and_var_id = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let constr_id = ['A'-'Z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let int_id = ['0'-'9']+ | "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ | "0b" ['0'-'1']+
let hexa = "0x" ['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']
let atom =['\000'-'\033' '\035'-'\038' '\040'-'\091''\093'-'\255'] | '\t' | '\n' | '\b' | '\r'
let char_id = atom
let slash = ['\'' '\"' '\\' 't' 'n' 'b' 'r']
let string_id = (atom | '\\'slash) * 


rule main = parse
  | layout+ {main lexbuf}
  | "**" ([^'\n'])* '\n'{ main lexbuf }
  | "**" ([^'\n'])* eof {EOF}
  | "(*" ([^'*']) "*)"{ main lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LCROCH }
  | ']' { RCROCH }
  | '{' { LACOLA }
  | '}' { RACOLA }
  | "->" { FLECH }
  | "<-" { FLECHB }
  | "&&" { OPAND ("&&") }
  | "||" { OPOR ("||") }
  | "|" { BAR }
  | "+" { PLUS ("+") }
  | "-" { MINUS ("-") }
  | "*" { MULT ("*") }
  | "/" { DIV ("/") }
  | "%" { MOD ("%") }
  | "!=" { OPCOMP ("!=") }
  | ":=" { OPCOMP (":=") }
  | "=>" { DEFFUN }
  | "=" { EQ ("=") }
  | "<=" { OPCOMP ("<=") }
  | ">=" { OPCOMP (">=") }
  | '<' { LCHEVR }
  | '>' { RCHEVR }
  | "~" { TILDE ("~") }
  | ":" { DEFTYPE }
  | ";" { POINTCOMMA }
  | "," { COMMA }
  | "in" { IN }
  | "where" { WHERE }
  | "end" { END }
  | "." { POINT }
  | "case" { CASE }
  | "at" { AT }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fun" { FUN }
  | "do" { DO }
  | "rec" { REC }
  | "is" { IS }
  | "val" { VAL }
  | "def"{ DEF }
  | "with" { WITH }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | "_" { PONE }
  | "0" { PZERO }
  | "type" { TYPE }
  | int_id  as c { INT (int_of_string(c)) }  (*a finir au hex et binaire et changer les type *)
  | type_id_and_var_id { VAR_OR_TYPE (Lexing.lexeme lexbuf) }
  | constr_id  { CONSTR (Lexing.lexeme lexbuf) }
  | '\'' '\\' ((('2'['0'-'4']['0'-'9'])|("24"['0'-'5']) |('0'['0'-'9']['0'-'9'])| ('1'['0'-'9']['0'-'9']))as n) '\'' {CHAR(chr(int_of_string(n)))}
  | '\'' '\\' (hexa as h)  '\'' {CHAR (chr(int_of_string(h))) }  
  | '\'' '\\' (slash as h)  '\'' {CHAR (h) }	     
  | '\'' ('\\' ( (['0'-'1']['0'-'9']['0'-'9']) | ('2'['0'-'4']['0'-'9']) | ('2' '5' ['0'-'5']) )as c )  '\'' { CHAR (chr(int_of_string(c))) }
  | ['\''](atom as c) ['\''] { CHAR (c) }
  | ['\"'](string_id as c)['\"'] { STRING (c) } (* a finir pour tout les caractere *)
  | eof                 { EOF }
  | _                   { failwith "unexpected character" }
