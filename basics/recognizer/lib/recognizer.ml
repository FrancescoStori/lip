(*
let charlist_of_string (lst : string) : word =
    lst > String.to_seq > List.of_seq;;
*)

let rec step_l1 q w = match (q, w) with
  (_,a::w') when (a='0') || (a='1') -> step_l1 1 w'
  | (1,[]) -> q
  | (0, []) -> -1
  | _ -> -1
;;

let lang1 w = (step_l1 0 w) = 1;;


let rec step_l2 q w = match (q, w) with
  (0,a::w') when a='0' -> step_l2 1 w'
  | (1,a::_) when a='0' -> -1
  | (_, a::w') when a='1'-> step_l2 1 w'
  | (1, []) -> 1
  | _ -> -1
;;

let lang2 w = (step_l2 0 w) = 1;;


let rec step_l3 q w = match (q,w) with
  (0,a::w') when a = '0' -> step_l3 1 w'
  | (1, '0'::[]) -> 1
  | (1, a::w') when a = '0' || a = '1' -> step_l3 1 w'
  | _ -> -1
;;

let lang3 w = (step_l3 0 w) = 1;;

let rec step_l4 q w = match (q,w) with
  (0,a::w') when a = '0' -> step_l4 0 w' 
  | (0, a::w') when a = '1' -> step_l4 1 w'
  | (1, a::w') when a = '0' -> step_l4 1 w'
  | (1, a::w') when a = '1' -> step_l4 2 w'
  | (2, a::w') when a = '0' -> step_l4 2 w'
  | (2, []) -> 2
  | (_, _) -> -1
;;

let lang4 w = (step_l4 0 w) = 2;;

let rec step_l5 q w = match (q, w) with
  (q',a::b::w') when (q' = 0 || q'= 1) && a = b && (a = '0' || a ='1') -> step_l5 1 w'
  | (1, []) -> 1
  | _ -> -1
;;
let lang5 w = (step_l5 0 w) = 1;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5];;
                  
let belongsTo w = List.map (fun f -> f w) recognizers;;
  
