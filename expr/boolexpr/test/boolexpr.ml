open BoolexprLib.Main
open BoolexprLib.Ast

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

  let test_trace1 expr exp_result =
    (expr |> parse |> trace1) = exp_result


(* ### Unit tests for task 4 *)

let%test "test_eval_1" = test_eval "false" false
let%test "test_eval_2" = test_eval "true" true
let%test "test_eval_3" = test_eval "if true then false else true" false
let%test "test_eval_4" = test_eval "if false then false else true" true
let%test "test_eval_5" = 
  test_eval "if true then (if true then false else true) else (if true then true else false)" false

let%test "test_eval_6" =
  test_eval "if (if false then false else false) then (if false then true else false) else (if true then false else true)" false

let%test "test_eval_7" =
  test_eval "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" false

(* ### Unit tests for task 5 *)

let%test "test_trace1_1" = try ( let e = (parse "if true then false else true" |> trace1) in e = False) with NoRuleApplies -> false
let%test "test_trace1_2" = try ( let e = (parse "if false then true else true" |> trace1) in e = True) with NoRuleApplies -> false
let%test "test_trace1_3" = try ( let e = (parse "if (if true then false else false) then false else true" |> trace1) in e = If(False,False,True)) with NoRuleApplies -> false

let%test "test_trace1_4" = try (let e = (parse "false" |> trace1) in e = False) with NoRuleApplies -> (is_value (parse "false"))
let%test "test_trace1_5" = try (let e = (parse "true" |> trace1) in e = True) with NoRuleApplies -> (is_value (parse "true"))

let%test "test_trace1_6" = let e = (parse "if (if false then false else false) then (if false then true else false) else (if true then false else true)" |> trace) in
    List.length e <= 10
  

(* ### Unit tests for task 6 *)

let%test "test_and_or_1" = test_eval "if true && true then false else true" false
let%test "test_and_or_2" = test_eval "if true && false then false else true" true
let%test "test_and_or_3" = test_eval "if true || false then false else true" false
let%test "test_and_or_4" = test_eval "if false || false then false else true" true