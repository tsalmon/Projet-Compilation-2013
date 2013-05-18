open AST 
open Runtime

exception Finished

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv


let e = Env.empty () ;;

let program : AST.program -> env = function
  | (DVal e)::b -> Env.empty ()
  | a::b -> failwith "autre a::b"
  | _ -> failwith "non reconnu"

