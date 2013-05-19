open AST 
open Runtime

exception Finished

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv


let e = ref (Env.empty ()) ;;




let rec program: AST.program -> env  =function 
  | (DVal v)::b -> begin match v with
      | Simple (Binding(a_i,_),expr) ->  e:=(Env.declare (a_i)  !e) ; (Env.define (a_i)  (Runtime.VInt 3) !e); program b;
      end ; 
  | [] -> !e
  | _ -> failwith "non reconnu"

(*
let rec programm e : AST.program -> env  =function 
  | (DVal v)::b -> begin match v with
      | Simple (Binding(a_i,_),expr) -> let eA=(Env.declare (a_i)  e) in (Env.define (a_i)  (Runtime.VInt 3) eA); programm eA b;
      end ; 
  | [] -> e
  | _ -> failwith "non reconnu"

let program : AST.program -> env = function
  | a -> let eA=(Env.declare (AST.Named(AST.Identifier "f"))  ea) in (Env.define (AST.Named(AST.Identifier "f"))  (Runtime.VInt 15) eA); programm eA a;

  | (DVal v)::b -> begin match v with
      | Simple (Binding(a_i,_),expr) -> let eA=(Env.declare (a_i)  e) in (Env.define (a_i)  (Runtime.VInt 3) eA);eA
      end ; program b
  | [] -> e
  | _ -> failwith "non reconnu"
*)


