open ArithexprLib.Ast
open ArithexprLib.Main


(**********************************************************************
 Test big-step semantics
 **********************************************************************)

let weval e = try Some (eval e)
  with _ -> None

let test_bigstep expr exp_result =
  (expr |> parse |> weval) = exp_result
  
let%test "test_bigstep1" = test_bigstep "if true then true else false and false" (Some (Bool true))

let%test "test_bigstep2" = test_bigstep "if true then false else false or true" (Some (Bool false))

let%test "test_bigstep3" = test_bigstep "succ 0" (Some (Nat 1))

let%test "test_bigstep4" = test_bigstep "succ succ succ pred pred succ succ pred succ pred succ 0" (Some (Nat 3))

let%test "test_bigstep5" = test_bigstep "iszero pred succ 0" (Some (Bool true))

let%test "test_bigstep6" = test_bigstep "iszero pred succ 0 and not iszero succ pred succ 0" (Some (Bool true))

let%test "test_bigstep7" = test_bigstep "iszero true" None

let%test "test_bigstep8" = test_bigstep "succ iszero 0" None

let%test "test_bigstep9" = test_bigstep "not 0" None

let%test "test_bigstep10" = test_bigstep "pred 0" None

let%test "test_bigstep11" = test_bigstep "pred pred succ 0" None


