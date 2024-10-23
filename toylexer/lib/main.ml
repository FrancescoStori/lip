open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl



(*let uniq l = List.sort_uniq (fun e e' -> if e = e' then 0 else -1 ) l;;*)

let uniq l = List.fold_left 
  (fun l' e -> if (List.for_all (fun e' -> e' <> e) l') then e::l' else l') [] l;;

let get_n n l = 
  List.fold_left (fun l' e -> if (List.length l' < n) then e::l' else l') [] l
  |> List.rev
;;

let count_occ e l = (List.filter (fun e' -> e' = e) l) |> List.length;;

(* frequency : int -> 'a list -> ('a * int) list *)

let frequency n (l) =
  let ul = uniq l in
  List.fold_left (fun l' e_ul -> (e_ul, count_occ e_ul l)::l') [] ul
  |> List.sort (fun (_, x) (_, y) -> if x=y then 0 else if x>y then -1 else 1)
  |> get_n n
;;
