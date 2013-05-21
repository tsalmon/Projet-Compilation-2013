open AST 
open Runtime

exception Finished

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv


let e = ref (Env.empty ()) ;;

             

let rec expression env= function 
   | EInt i                -> Runtime.VInt i
   | EChar c               -> Runtime.VChar c
   | EString chaine        -> Runtime.VString chaine
   | EVar id               -> if (Primitive.identifier id) then (Primitive.lookup id) else (Env.lookup (AST.Named id) env)
   | ESum ( c, t, Some e)  -> VStruct([(c, Some (expression env e))]) (*;VStruct([(c, expression env e)])*)
   | ESum (c, _, _ )       -> VStruct([(c, None)])
   | EProd (_, c)          -> VStruct (expression_prod env c) (*failwith "prod expr non fonctionnel"*)
   | EProd (_,_)           -> failwith "expr non fonctionnel"
   | EAnnot (expr,_)       -> expression env expr 
   | ESeq (exprs)          -> expresion_seq env exprs
   | EDef (v,expr)         -> expression (vdefinition env v) expr
   | EApp(exprA,exprB)     ->  (expression_app ((expression env exprA),(expression env exprB)))
   | ECase(_,c)            -> VClosure(env,c)
   | EFun (b,f)            -> VClosure((env), Branch(POne, EFun(b,f))::[])

and expresion_seq env = function 
   | expr::[] -> expression env expr
   | expr::exprs -> expression env expr ; expresion_seq env exprs;
   | [] -> failwith "seq vide"
   | _ -> failwith"seq non reconnu"
    

and expression_app = function 
   | (VPrimitive p, i) -> Primitive.apply p  i
   | ( (  VClosure (env,Branch(POne , EFun(Binding(a_i,_),expr))::[] )  )  , i ) ->  expression (Env.bind a_i i env) expr
   | (VClosure(env,Branch(p,e)::l),s)-> let (envf,rf)= (closure env s p) in if rf then (expression envf e) else (expression_app (VClosure(env,l),s))
   |  (VClosure(env,[]),s) -> Runtime.VInt 0
   | _ -> failwith "app non reconu"

and closure env s = function
   | PSum (cp,_,Some pp) -> let r = match s with 
        VStruct((cs,Some ps)::[]) -> if cp=cs then (closure env ps pp)  else (env,false) | _-> (env,false) in r;
   | PSum (cp,_,None) -> let r = match s with 
        VStruct((cs,None)::[]) -> if cp=cs then (env,true)  else (env,false) | _-> (env,false) in r;
   | PProd(_,c) -> closure_prod env s c
   | PAnd(a,b) -> let (ae,ar)=(closure env s a) in if ar then (closure ae s b) else (ae,false)  
   | POr (a,b)-> let (ae,ar)=(closure env s a) in if ar then (ae,true) else (closure env s b)
   | PNot(p) -> let (a,b)= (closure env s p) in (a,(not b))
   | PVar v ->  ((Env.bind (Named v) s env),true) 
   | POne -> (env,true)
   | _ -> (env,false)

and closure_prod env s = function
   | (cp,Some pp)::lp -> let r = match s with 
        VStruct((cs,Some ps)::ls) -> if cp=cs then ( let (a,b)=(closure env ps pp) in if b then (closure_prod a  (VStruct(ls)) lp) else (env,false))  else (env,false) | _-> (env,false) in r;
   | (cp,_)::lp -> let r = match s with 
        VStruct((cs,None)::ls) -> if cp=cs then  (closure_prod env  (VStruct(ls)) lp)   else (env,false) | _-> (env,false) in r; 
   | [] -> if s=(VStruct([])) then (env,true) else (env,false);  

and vdefinition env = function 
  | Simple (Binding(a_i,_),expr) ->  (Env.bind (a_i)  (expression env expr)  env)
  | MutuallyRecursive ((Binding(a_i,_),expr)::l) -> let x= (Env.bind (a_i)  (expression env expr)  env) in vdefinition x (MutuallyRecursive(l))
  | MutuallyRecursive([])-> env

and expression_prod env = function
   | [] -> []
   | (c,Some e)::q -> (c, Some (expression env e))::(expression_prod env q)
   | (c,_)::q -> (c, None)::(expression_prod env q)


let rec program: AST.program -> env  = function 
  | (DVal v)::b -> e:=(vdefinition !e v) ;program b;
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


