open Middle

let compile_mir code flags =
  match
    Driver.Entry.stan2mir "entry_test_model" (`Code code) flags (fun _ -> ())
  with
  | Ok mir -> mir
  | Error error -> failwith (Test_utils.error_to_string ~code error)

let%expect_test "stan2mir returns transformed and optimized MIR" =
  let flags =
    { Driver.Flags.default with
      optimization_level= Analysis_and_optimization.Optimize.O1 } in
  let mir = compile_mir "model { target += 1 + 2; }" flags in
  Fmt.pr "%a@." Fmt.(list ~sep:cut Stmt.Located.pp) mir.log_prob;
  [%expect {|
    { target += 3;
    }
    |}]

let%expect_test "stan2mir reports frontend errors" =
  let code = "model { target += missing; }" in
  match compile_mir code Driver.Flags.default with
  | _ -> print_endline "unexpected success"
  | exception Failure error ->
      print_endline error;
      [%expect
        {|
        Semantic error in 'string', line 1, column 18 to column 25:
           -------------------------------------------------
             1:  model { target += missing; }
                                   ^
           -------------------------------------------------

        Identifier "missing" not in scope. Did you mean "is_inf"?
        |}]
