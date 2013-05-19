open AST 
open Runtime

exception Finished

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv


let e = ref (Env.empty ()) ;;

(*
let rec expression_appr= function 
   | (EApp(exprA,exprB),EInt i) -> ( Primitive.apply (expression_appr(exprA,exprB)) (Runtime.VInt i))
   | (EVar v,EInt i) -> (Primitive.apply (Primitive.lookup v) (Runtime.VInt i))
*)

let expression_app= function 
   (* |(EApp(exprA,exprB),EInt i) -> Runtime.VInt (Primitive.apply (expression_appr(exprA,exprB)) (Runtime.VInt i))*)
    |(EVar v,EInt i) ->  i

let expression = function 
   | EInt i             -> Runtime.VInt i
   | EChar c            -> Runtime.VChar c
   | EString chaine     -> Runtime.VString chaine
   | EVar id            -> Env.lookup (AST.Named id) !e
   | ESum (_,_,_)       -> failwith "expr non fonctionnel"
   | EProd (_,_)        -> failwith "expr non fonctionnel"
   | EAnnot (_,_)       -> failwith "expr non fonctionnel"
   | ESeq (_)         -> failwith "expr non fonctionnel"
   | EDef (_,_)         -> failwith "expr non fonctionnel"
   | EApp(exprA,exprB) ->  Runtime.VInt (expression_app (exprA,exprB))
   | ECase(_,_)         -> failwith "expr non fonctionnel"
   | EFun (_,_)         -> failwith "expr non fonctionnel"

let rec program: AST.program -> env  = function 
  | (DVal v)::b -> begin match v with
      | Simple (Binding(a_i,_),expr) ->  e:=(Env.declare (a_i)  !e) ; (Env.define (a_i)  (expression expr)  !e); program b;
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


