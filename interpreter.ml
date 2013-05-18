open AST 
open Runtime

exception Finished

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv



let program : AST.program -> env = function
   | _ -> e
   | _ -> failwith "non reconu"

