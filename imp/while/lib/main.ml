open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


let rec eval_expr st = function 
  | True -> Bool true
  | False -> Bool false
  | Var(x) -> st x
  | Const(n) -> Nat n
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
          (Nat n1, Nat n2) -> Nat (n1+n2)
        | _ -> raise (TypeError "Add"))
  | Sub(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
          (Nat n1, Nat n2) -> Nat (n1-n2)
        | _ -> raise (TypeError "Sub"))
  | Mul(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
          (Nat n1, Nat n2) -> Nat (n1*n2)
        | _ -> raise (TypeError "Mul"))
  | Eq(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
(*         (Bool b1, Bool b2) -> Bool (b1 = b2) *)
      | (Nat n1, Nat n2) -> Bool (n1 = n2)
      | _ -> raise (TypeError "Eq"))
  | Leq(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
      (Nat n1, Nat n2) -> Bool (n1 <= n2)
    | _ -> raise (TypeError "Leq"))



let bot = fun x -> raise (UnboundVar x)

(* 
   The `bind` function creates a new state by binding a given identifier (`id`) to a value (`value`) 
   in the current state (`current_state`). The resulting state is a function that, when given an 
   identifier (`search_id`), returns the bound value if `search_id` matches `id`, or delegates 
   the lookup to the `current_state` function otherwise.
   
   Parameters:
   - current_state: A function representing the current state, which maps identifiers to values.
   - id: The identifier to bind to the new value.
   - value: The value to bind to the identifier.
   
   Returns:
   - A new state function that incorporates the new binding.
*)
let bind current_state id value =
  (*this function is the new state*)
  fun search_id -> 
    if search_id = id then value else current_state search_id

let rec trace1 = function
    St _ -> raise NoRuleApplies
  | Cmd(c, st) -> match c with
      Skip -> St st
    | Assign(x, e) -> St (bind st x (eval_expr st e))
    
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

let rec trace_rec n_rec t =
  if n_rec <= 0 then [t]
  else try
    let t' = trace1 t
    in t::(trace_rec (n_rec-1) t')
  with NoRuleApplies -> [t]

let trace n_rec commands = trace_rec n_rec (Cmd(commands, bot))