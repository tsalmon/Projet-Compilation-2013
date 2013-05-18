open AST 
open Runtime

exception Finished

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv


let e = Env.empty () ;;

let program : AST.program -> env = function
  | (DVal v)::b ->  let eA=(Env.declare (AST.Named(AST.Identifier "x"))  e) in (Env.define (AST.Named(AST.Identifier "x")) (Runtime.VInt 3) eA);eA
  | a::b -> failwith "autre a::b"
  | _ -> failwith "non reconnu"

