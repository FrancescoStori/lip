open Ast
open Types
open Prettyprint

type exprtype = BoolT | NatT

let string_of_type = function
    BoolT -> "Bool"
  | NatT -> "Nat"


let unBool = function (Bool e) -> e | _ -> failwith "boh"
let unNat = function (Nat e) -> e | _ -> failwith "boh"


let string_of_val = function
    Bool true -> "True"
  | Bool false -> "False"
  | Nat n -> string_of_int n

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Not e -> " not "^(string_of_expr e)
  | And(e1,e2) -> (string_of_expr e1) ^ " and " ^ (string_of_expr e2)
  | Or(e1,e2)-> (string_of_expr e1) ^ " or " ^ (string_of_expr e2)

  

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


let rec typecheck (e : expr) = function
  | True -> BoolT
  | False -> BoolT 
  | Var(_) -> NatT
  | Const(_) -> NatT
  | Not(e) when typecheck e = BoolT -> BoolT
  | And(e1, e2) when (typecheck e1 = BoolT) && (typecheck e2 = BoolT) -> BoolT
  | Or(e1, e2) when (typecheck e1 = BoolT) || (typecheck e2 = BoolT) -> BoolT
  | Add(e1, e2) when  -> NatT
  | Sub(e1, e2) -> NatT
  | Mul(e1, e2) -> NatT
  | Eq(e1, e2) -> BoolT
  | Leq(e1, e2) -> BoolT
  | _ -> raise (TypeError "Errore: l'espressione non è ben tipata")

exception NoRuleApplies


let bind (st : state) (x : ide) v : state = fun y -> if y = x then v else st y

let rec trace1 = function
  | Not(e) when e = True || e = False -> if e = True then False else True
  | Not(e) -> Not(trace1 e)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval_expr (d : state) = function
    True -> Bool true
  | False -> Bool false
  | Var(e) when (typecheck1 e) = Nat -> let (Nat x) = (d e) in Nat x
  | Const(e) -> failwith "boh"
  | Not(e) -> Bool (not (unBool (eval_expr d e)))
  | And(e1, e2) -> Bool ((unBool (eval_expr d e1)) && (unBool (eval_expr d e2)))
  | Or(e1, e2) -> Bool ((unBool (eval_expr d e1)) || (unBool (eval_expr d e2)))
  | Add(e1, e2) -> failwith "boh"
  | Sub(e1, e2) -> failwith "boh"
  | Mul(e1, e2) -> failwith "boh"
  | Eq(e1, e2) -> failwith "boh"
  | Leq(e1, e2) -> failwith "boh"
  | _ -> failwith "boh"
