open AST 
open Runtime

exception Finished

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv


let e = ref (Env.empty ()) ;;

             

let rec expression env= function 
   | EInt i             -> Runtime.VInt i
   | EChar c            -> Runtime.VChar c
   | EString chaine     -> Runtime.VString chaine
   | EVar id            -> if (Primitive.identifier id) then (Primitive.lookup id) else (Env.lookup (AST.Named id) env)
   | ESum (_,_,_)       -> failwith "expr non fonctionnel"
   | EProd (_,_)        -> failwith "expr non fonctionnel"
   | EAnnot (_,_)       -> failwith "expr non fonctionnel"
   | ESeq (_)           -> failwith "expr non fonctionnel"
   | EDef (_,_)         -> failwith "expr non fonctionnel"
   | EApp(exprA,exprB)  ->  (expression_app ((expression env exprA),(expression env exprB)))
   | ECase(_,_)         -> failwith "expr non fonctionnel"
   | EFun (b,f) -> VClosure((Env.empty ()), Branch(POne, EFun(b,f))::[])


and expression_app = function 
   | (VPrimitive p, i) -> Primitive.apply p  i
   | ( (  VClosure (env,Branch(POne , EFun(Binding(a_i,_),EFun(b,f)))::[] )  )  , i ) ->  VClosure((Env.bind a_i i env), Branch(POne, EFun(b,f))::[])
   | ( (  VClosure (env,Branch(POne , EFun(Binding(a_i,_),expr))::[] )  )  , i ) ->  expression (Env.bind a_i i env) expr
   | _ -> failwith "app non reconu"




let rec program: AST.program -> env  = function 
  | (DVal v)::b -> begin match v with
      | Simple (Binding(a_i,_),expr) ->  e:=(Env.declare (a_i)  !e) ; (Env.define (a_i)  (expression !e expr)  !e); program b;
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


