open Ast

exception TypeError of string;;

type exprval =
    Bool of bool
  | Nat of int

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
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not e -> " not "^(string_of_expr e)
  | And(e1,e2) -> (string_of_expr e1) ^ " and " ^ (string_of_expr e2)
  | Or(e1,e2)-> (string_of_expr e1) ^ " or " ^ (string_of_expr e2)
  | Zero -> "0"
  | Succ e -> "Succ "^(string_of_expr e)
  | Pred e -> "Pred "^(string_of_expr e)
  | IsZero e -> "IsZero " ^ (string_of_expr e)
  

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


let rec typecheck = function 
  True -> BoolT
| False -> BoolT
| If(e1,e2,e3) when typecheck e1 = BoolT && typecheck e2 = typecheck e3 -> typecheck e2
| If(e1, e2, e3) when typecheck e1 = BoolT && typecheck e2 <> typecheck e3 -> 
    raise (TypeError ((string_of_expr e2)^" has type "^(string_of_type(typecheck e2))^", but "^(string_of_expr e3)^" has type "^(string_of_type(typecheck e3))))

| If (e1,_,_) when typecheck e1 <> BoolT -> 
    raise (TypeError (string_of_expr e1^" has type "^(string_of_type(typecheck e1))^", but type Bool was expected"))

| Not(e) when typecheck e = BoolT -> BoolT
| Not(e) when typecheck e <> BoolT ->
   raise (TypeError (string_of_expr e^" has type "^(string_of_type(typecheck e))^", but type Bool was expected"))

| And(e1,e2) when typecheck e1 = BoolT && typecheck e2 = BoolT -> BoolT
| And(e1,e2) when typecheck e1 <> BoolT && typecheck e2 <> BoolT -> 
    raise (TypeError (string_of_expr e1^" has type "^(string_of_type(typecheck e1))^" and "^(string_of_expr e2)^" has type "^(string_of_type(typecheck e2))^ ", but type Bool was expected"))
| And(e1,_) when typecheck e1 <> BoolT -> 
    raise (TypeError (string_of_expr e1^" has type "^(string_of_type(typecheck e1))^ ", but type Bool was expected"))
| And(_,e2) when typecheck e2 <> BoolT -> 
    raise (TypeError (string_of_expr e2^" has type "^(string_of_type(typecheck e2))^ ", but type Bool was expected"))

| Or(e1,e2) when typecheck e1 = BoolT && typecheck e2 = BoolT -> BoolT
| Or (e1,e2) when typecheck e1 <> BoolT && typecheck e2 <> BoolT -> 
    raise (TypeError (string_of_expr e1^" has type "^(string_of_type(typecheck e1))^" and "^(string_of_expr e2)^" has type "^(string_of_type(typecheck e2))^ ", but type Bool was expected"))
| Or(e1,_) when typecheck e1 <> BoolT -> 
    raise (TypeError (string_of_expr e1^" has type "^(string_of_type(typecheck e1))^ ", but type Bool was expected"))
| Or(_,e2) when typecheck e2 <> BoolT -> 
    raise (TypeError (string_of_expr e2^" has type "^(string_of_type(typecheck e2))^ ", but type Bool was expected"))

| Zero -> NatT
| Succ(e) when typecheck e = NatT -> NatT
| Succ(e) when typecheck e <> NatT -> 
    raise (TypeError (string_of_expr e^" has type "^(string_of_type(typecheck e))^ ", but type Nat was expected"))
| Pred(e) when typecheck e = NatT -> NatT
| Pred(e) when typecheck e <> NatT -> 
    raise (TypeError (string_of_expr e^" has type "^(string_of_type(typecheck e))^ ", but type Nat was expected"))
| IsZero(e) when typecheck e = NatT -> BoolT
| IsZero(e) when typecheck e <> NatT -> 
    raise (TypeError (string_of_expr e^" has type "^(string_of_type(typecheck e))^ ", but type Nat was expected"))
    
| _ -> raise (TypeError "Errore: l'espressione non è ben tipata")

exception NoRuleApplies

let rec is_nv = function
    Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e1, e2, e3) -> If(trace1 e1, e2, e3)
  | Not(e) when e = True || e = False -> if e = True then False else True
  | Not(e) -> Not(trace1 e)
  | Succ(e) -> Succ(trace1 e)
  | Pred(Succ(e)) when is_nv e -> e
  | Pred(e) -> Pred(trace1 e)
  | IsZero(Zero) -> True
  | IsZero(Succ(e))  when is_nv e-> False
  | IsZero(e) -> IsZero(trace1 e)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval = function
    True -> Bool true
  | False -> Bool false
  | If(e1,e2,e3) -> if (unBool (eval e1)) then eval e2 else eval e3
  | Not(e) -> Bool (not (unBool (eval e)))
  | Zero -> Nat 0
  | Succ(e) -> Nat ((unNat(eval e)) + 1)
  | Pred(e) when (unNat(eval e)) > 0 -> Nat((unNat(eval e)) - 1)
  | IsZero(e) when (unNat(eval e)) = 0 -> Bool true
  | IsZero(e) when (unNat(eval e)) > 0 -> Bool false 
  | _ -> failwith "boh"
