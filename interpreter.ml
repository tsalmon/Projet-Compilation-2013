open AST 
open Runtime

exception Finished

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv

(*let e = Env.empty () *)

(*
  | (DVal v)::b ->  
    let e=(Env.declare (AST.Named(AST.Identifier "x"))  e) in 
  (Env.define (AST.Named(AST.Identifier "x")) (Runtime.VInt 30) e);
  e
*)
    
(* call by ipr_vdef *)
let ipr_expr ev = function
  | EInt(i) -> print_int(i);print_string("\n");failwith "int"
  | EChar(c) -> failwith "char"
  | EString(s) -> failwith "string"
  | EVar(v) -> failwith "var"
  | ESum(c, t , e ) -> failwith "sum"
  | EProd(t, c)-> failwith "prod"
  | EAnnot(e, t) -> failwith "annot"
  | ESeq(e) -> failwith "seq"
  | EDef(v, e) -> failwith "def"
  | EApp(e1, e2) -> failwith "app"
  | ECase(t, b) -> failwith "case"
  | EFun( b, e) -> failwith "fun"

let ipr_arg_id e = function
  | Named(Identifier(s)) -> s
  | Unnamed -> "_"

let ipr_binding = function 
  | Binding(ar, t) -> ipr_arg_id ar 


(* call by ipr_def *)
let ipr_vdef e = function
  | Simple (Binding(ar,t), EInt(i)) ->   
    let x=(Env.declare ar  e) in 
    (Env.define ar (Runtime.VInt i) x); x
  | Simple (b, exp) -> failwith "simple"
  | MutuallyRecursive m -> failwith "mutually"

(* call by ipr_program *)
let ipr_def  e = function 
  | DType (identifier, identifiers, t) -> failwith "dtype"
  | DVal  v -> ipr_vdef e v
    
(* call by program *)
let rec ipr_program e = function
  | [] -> e
  | a::b -> ipr_program (ipr_def e a) b
    
let program : AST.program -> env = function
  | a -> ipr_program (Env.empty ()) a


(*
let program : AST.program -> env = function
  | _ -> 
    let x=(Env.declare (AST.Named(AST.Identifier "x"))  (Env.empty ()))
    in Env.define (AST.Named(AST.Identifier("x"))) (Runtime.VInt 10) x; x
*)
