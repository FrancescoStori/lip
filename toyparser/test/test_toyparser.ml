open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9

let%test "test_eval_2" = parse "1 + 5 + 3" |> eval = Ok 9

let%test "test_eval_3" = parse "(1 - 5) + 12" |> eval = Ok 8

let%test "test_eval_4" = parse "(1 + 5) * 2" |> eval = Ok 12

let%test "test_eval_5" = parse "6 / 2" |> eval = Ok 3

let%test "test_eval_6" = parse "6 / 0" |> eval = Error "Error: tried to divide 6 by zero"

let%test "test_eval_7" = parse "0x1a / 2" |> eval = Ok 13