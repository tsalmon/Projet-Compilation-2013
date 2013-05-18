open AST 
open Runtime

exception Finished

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv

let inter_vdefinition  = ""

let program : AST.program -> env = function
  | (DVal a)::b -> Env.empty ()
  | a::b -> failwith "autre a::b"
  | _ -> failwith "non reconnu"
