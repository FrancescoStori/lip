open Ast
open Types

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


let bind (current_memory) locazione value =
  (*this function is the new memory*)
  fun search_loc -> 
    if search_loc = locazione then value else current_memory search_loc 


let rec sem_decl (e, l) = function
  | [] -> (e, l)
  | (IntVar x)::d -> let e' = bind_env e x (IVar l) in sem_decl (e', l+1) d
  | (Fun (f, x, c, ex))::d -> let e' = bind_env e f (IFun (x, c, ex)) in sem_decl (e', l) d


let rec trace1_expr st = function
  | Var(x) -> (Const(apply st x), st)

  | Not(True) -> (False, st)
  | Not(False) -> (True, st)
  | Not(e) -> let (e',st') = trace1_expr st e in (Not(e'), st')

  | And(True, True) -> (True, st)
  | And(False, _) -> (False, st)
  | And(_, False) -> (False, st)
  | And(True, e) -> let (e', st') = trace1_expr st e in (And(True, e'), st') 
  | And(e1, e2) -> let (e1', st') = trace1_expr st e1 in (And(e1', e2), st')
  
  | Or(True, _) -> (True, st)
  | Or(_, True) -> (True, st)
  | Or(False, False) -> (False, st)
  | Or(False, e) -> let (e', st') = trace1_expr st e in (Or(False, e'), st') 
  | Or(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Or(e1', e2), st')
  
  | Add(Const(n1), Const(n2)) -> (Const(n1+n2), st)
  | Add(Const(n1), e) -> let (e', st') = trace1_expr st e in (Add(Const(n1), e'), st')
  | Add(e1, e2) -> let (e1',st') = trace1_expr st e1 in (Add(e1',e2), st')

  | Sub(Const(n1), Const(n2)) -> (Const(n1-n2), st)
  | Sub(Const(n1), e) -> let (e', st') = trace1_expr st e in (Sub(Const(n1), e'), st')
  | Sub(e1, e2) -> let (e1',st') = trace1_expr st e1 in (Sub(e1',e2), st')

  | Mul(Const(n1), Const(n2)) -> (Const(n1*n2), st)
  | Mul(Const(n1), e) -> let (e', st') = trace1_expr st e in (Mul(Const(n1), e'), st')
  | Mul(e1, e2) -> let (e1',st') = trace1_expr st e1 in (Mul(e1',e2), st')

  | Eq(Const(n1), Const(n2)) -> ((if n1 = n2 then True else False) , st)
  | Eq(Const(n), e) -> let (e', st') = trace1_expr st e in (Eq(Const(n), e'), st')
  | Eq(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Eq(e1',e2), st')

  | Leq(Const(n1), Const(n2)) -> ((if n1 <= n2 then True else False) , st)
  | Leq(Const(n), e) -> let (e', st') = trace1_expr st e in (Leq(Const(n), e'), st')
  | Leq(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Leq(e1',e2), st')

  | Call(f, Const(n)) -> let (x, c, er) = apply_fun st f  in
    let st' = make_state ((bind_env (topenv st) x (IVar (getloc st)))::(getenv st)) (getmem st) ((getloc st) +1) in
      let st'' = (bind_ivar st' x n) in (CallExec(c,er), st'')

  | Call(f, e) -> let (e', st') = trace1_expr st e in (Call(f,e'), st')

  | CallExec(c, e) -> (match trace1_cmd (Cmd(c, st)) with
    | St st' -> (CallRet(e), st')
    | Cmd(c', st') -> (CallExec(c', e), st'))
    
  | CallRet(Const(n)) -> (Const(n), setenv st (popenv st))
  | CallRet(e) -> let (e', st') = trace1_expr st e in (CallRet(e'), st')

  | _ -> raise NoRuleApplies

and trace1_cmd = function
    St _ -> raise NoRuleApplies

  | Cmd(c, st) -> match c with
      Skip -> St st

    | Assign(x, Const(n)) -> (match (topenv st) x with
        IVar l -> St (make_state (getenv st) (bind (getmem st) l n) (getloc st))
      | IFun _ -> raise (TypeError "Assign"))

    | Assign(x, e) -> let (e',st') = trace1_expr st e in trace1_cmd (Cmd(Assign(x, e'), st'))

    | Seq(c1, c2) -> (match trace1_cmd (Cmd(c1, st)) with
        St st' -> Cmd(c2, st')
      | Cmd(c1', st') -> Cmd(Seq(c1', c2), st'))
    
    | If(True,c1,_) -> Cmd(c1,st)
    | If(False,_,c2) -> Cmd(c2,st)
    | If(e,c1,c2) -> let (e',st') = trace1_expr st e in Cmd(If(e',c1,c2),st')

    | While(e,c) -> Cmd(If(e,Seq(c,While(e,c)),Skip),st)



let rec trace_rec n_rec t =
  if n_rec <= 0 then [t]
  else try
    let t' = trace1_cmd t
    in t::(trace_rec (n_rec-1) t')
  with NoRuleApplies -> [t]

let trace (n_rec : int) (Prog(d, commands)) = 
  (* dichiarazioni inizio programma (int, fun, etc.) *)
  let (e,l) = sem_decl ((topenv state0), (getloc state0)) d  (*sem_decl su env vuoto e loc 0; produce un environment con le dichiarazioni salvate in stack e una locazione l che è la prima locazione disponibile *)

  in let st = (setenv state0 [e]) in (* crea uno stato del tutto vuoto se non per la env list = [e] *)
  let st' = (setloc st l)
  in trace_rec n_rec (Cmd(commands, st'))


