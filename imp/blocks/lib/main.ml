open Ast
open Types



let apply st x = match ((topenv st) x) with
  | BVar l -> (getmem st) l
  | IVar l -> (getmem st) l

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


let rec eval_expr st = function 
  | True -> Bool true
  | False -> Bool false
  | Var(x) -> apply st x
  | Const(n) -> Int n
  | Not(e) -> (match (eval_expr st e) with
        Bool b -> Bool (not b)
      | _ -> raise (TypeError "Not"))
  | And(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
        (Bool b1, Bool b2) -> Bool (b1 && b2)
      | _ -> raise (TypeError "And"))
  | Or(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
        (Bool b1, Bool b2) -> Bool (b1 || b2)
      | _ -> raise (TypeError "Or"))
  | Add(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
          (Int n1, Int n2) -> Int (n1+n2)
        | _ -> raise (TypeError "Add"))
  | Sub(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
          (Int n1, Int n2) -> Int (n1-n2)
        | _ -> raise (TypeError "Sub"))
  | Mul(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
          (Int n1, Int n2) -> Int (n1*n2)
        | _ -> raise (TypeError "Mul"))
  | Eq(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
(*         (Bool b1, Bool b2) -> Bool (b1 = b2) *)
      | (Int n1, Int n2) -> Bool (n1 = n2)
      | _ -> raise (TypeError "Eq"))
  | Leq(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
      (Int n1, Int n2) -> Bool (n1 <= n2)
    | _ -> raise (TypeError "Leq"))



let bind (current_memory) locazione value =
  (*this function is the new memory*)
  fun search_loc -> 
    if search_loc = locazione then value else current_memory search_loc 


let rec sem_decl (e, l) = function
  | [] -> (e, l)
  | (IntVar x)::d -> let e' = bind e x (IVar l) in sem_decl (e', l+1) d
  | (BoolVar x)::d -> let e' = bind e x (BVar l) in sem_decl (e', l+1) d


let rec trace1 = function
    St _ -> raise NoRuleApplies
  | Cmd(c, st) -> match c with
      Skip -> St st
    | Assign(x, e) -> (match (eval_expr st e, topenv st x) with
      | (Int v, IVar l) -> St (make_state  (getenv st) (bind (getmem st) l (Int v)) (getloc st))
      | (Bool v, BVar l) -> St (make_state (getenv st) (bind (getmem st) l (Bool v)) (getloc st))
      | _ -> raise (TypeError "Assign")
    )

    | Seq(c1, c2) -> (match trace1 (Cmd(c1, st)) with
        St st' -> Cmd(c2, st')
      | Cmd(c1', st') -> Cmd(Seq(c1', c2), st'))
    
    | If(e, c1, c2) -> (match (eval_expr st e) with
      | Bool e' -> if e' then Cmd(c1, st) else Cmd(c2, st)
      | _ -> raise (TypeError "If"))
      
    | While(e, c) -> (match eval_expr st e with
      | Bool true -> Cmd(Seq(c, While(e,c)), st)
      | Bool false -> St st
      | _ -> raise (TypeError "While"))
    
    | Decl(d_list, c1) -> let (e,l) = sem_decl (topenv st, getloc st) d_list in
      Cmd(Block(c1), make_state (e::(getenv st)) (getmem st) l)
    
    | Block(c) -> (match (trace1 (Cmd(c, st))) with
      | St st' -> St (setenv st' (popenv st'))
      | Cmd(c', st') -> Cmd(Block(c'), st'))

let rec trace_rec n_rec t =
  if n_rec <= 0 then [t]
  else try
    let t' = trace1 t
    in t::(trace_rec (n_rec-1) t')
  with NoRuleApplies -> [t]

let trace (n_rec : int) commands = trace_rec n_rec (Cmd(commands, state0))