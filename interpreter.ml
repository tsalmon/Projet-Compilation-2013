open AST 
open Runtime

exception Finished

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv


let e = Env.empty () ;;

let vdefinition v  = function
  | Simple (binding,expr) -> let eA=(Env.declare (AST.Named(AST.Identifier "x"))  e) in (Env.define (AST.Named(AST.Identifier "x")) (Runtime.VInt 3) eA);eA
 (* | MutuallyRecursive (binding, expr):: -> e'*)
  |  _ -> failwith "vedefinition error"


let program : AST.program -> env = function
  | (DVal v)::b -> begin match v with
      | Simple (Binding(a_i,_),expr) -> let eA=(Env.declare (a_i)  e) in (Env.define (a_i)  (Runtime.VInt 3) eA);eA
      end ;
  | a::b -> failwith "autre a::b"
  | _ -> failwith "non reconnu"

