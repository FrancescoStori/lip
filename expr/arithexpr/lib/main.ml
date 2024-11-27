open Ast

type exprval =
    Bool of bool
  | Nat of int

let unBool = function (Bool e) -> e | _ -> failwith "boh"
let unNat = function (Nat e) -> e | _ -> failwith "boh"


let string_of_val = function
    Bool true -> "True"
  | Bool false -> "False"
  | Nat n -> string_of_int n

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not e -> "!"^(string_of_expr e)
  | And(e1,e2) -> (string_of_expr e1) ^ "&&" ^ (string_of_expr e2)
  | Or(e1,e2)-> (string_of_expr e1) ^ "||" ^ (string_of_expr e2)
  | Zero -> "0"
  | Succ e -> "Succ"^(string_of_expr e)
  | Pred e -> "Pred"^(string_of_expr e)
  | IsZero e -> "IsZero" ^ (string_of_expr e)
  

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec trace1 = function
  | If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e1, e2, e3) -> If(trace1 e1, e2, e3)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval = function
    True -> Bool true
  | False -> Bool false
  | If(e1,e2,e3) -> if (unBool (eval e1)) then eval e2 else eval e3
  | Zero -> Nat 0
  | Succ(e) -> Nat ((unNat(eval e)) + 1)
  | Pred(e) when (unNat(eval e)) > 0 -> Nat((unNat(eval e)) - 1)
  | IsZero(e) when (unNat(eval e)) = 0 -> Bool true
  | IsZero(e) when (unNat(eval e)) > 0 -> Bool false 
  | _ -> failwith "boh"