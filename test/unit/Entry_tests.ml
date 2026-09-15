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

let%expect_test "force_vectorize_loops runs only the vectorization pass" =
  let flags =
    { Driver.Flags.default with
      debug_settings=
        { Driver.Flags.default.debug_settings with
          force_vectorize_loops= Some true } } in
  let mir =
    compile_mir
      {|
      data { int N; vector[N] y; }
      parameters { real mu; }
      model {
        target += 1 + 2;
        for (n in 1:N) target += normal_lpdf(y[n] | mu, 1);
      }
      |}
      flags in
  (* the constant sum is left alone (no partial evaluation) and the loop is
     hoisted to one density call *)
  Fmt.pr "%a@." Fmt.(list ~sep:cut Stmt.Located.pp) mir.log_prob;
  [%expect
    {|
    real mu = (FnReadParam(constrain Identity)(dims())(mem_pattern AoS))__();{
      target += (1 + 2);
      target += normal_lpdf(y[1:N], mu, promote(1, real, data));
    }
    |}]

let%expect_test "force_vectorize_loops false disables the pass at Oexperimental"
    =
  let flags =
    { Driver.Flags.default with
      optimization_level= Analysis_and_optimization.Optimize.Oexperimental
    ; debug_settings=
        { Driver.Flags.default.debug_settings with
          force_vectorize_loops= Some false } } in
  let mir =
    compile_mir
      {|
      data { int N; vector[N] y; }
      parameters { real mu; }
      model {
        for (n in 1:N) target += normal_lpdf(y[n] | mu, 1);
      }
      |}
      flags in
  Fmt.pr "%a@." Fmt.(list ~sep:cut Stmt.Located.pp) mir.log_prob;
  [%expect
    {|
    data real lcm_sym8__;data real lcm_sym7__;data int lcm_sym6__;
    real mu = (FnReadParam(constrain Identity)(dims())(mem_pattern AoS))__();{
      if((N >= 1)) {
        target += normal_lpdf(y[1], mu, promote(1, real, data));
        for(n in 2:N) {
          target += normal_lpdf(y[n], mu, promote(1, real, data));
        }
      }
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
