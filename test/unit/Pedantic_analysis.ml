open Core_kernel
open Frontend
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Pedantic_analysis
open Middle
open Analysis_and_optimization.Dataflow_types

let semantic_check_program ast =
  Option.value_exn
    (Result.ok
       (Semantic_check.semantic_check_program
          (Option.value_exn (Result.ok ast))))

let sigma_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        parameters {
          real sigma;
          real sigma_a;
          real<lower=0> sigma_b;
          real<lower=0, upper=1> sigma_c;
          real<lower=1> sigma_d;
          real<lower=1, upper=2> sigma_e;
          real logsigma_f;
        }
        model {}
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Unbounded sigma warning" =
  print_warn_pedantic sigma_example ;
  [%expect
    {|
      Warning: Your Stan program has an unconstrained parameter "sigma" whose name begins with "sigma". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.
      Warning: Your Stan program has an unconstrained parameter "sigma_a" whose name begins with "sigma". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.
      Warning: Your Stan program has an unconstrained parameter "sigma_d" whose name begins with "sigma". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.
      Warning: Your Stan program has an unconstrained parameter "sigma_e" whose name begins with "sigma". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.
    |}]

let uniform_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        parameters {
          real x;
        }
        model {
          x ~ uniform(0, 1);
          1 ~ uniform(0, x);
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Uniform warning" =
  print_warn_pedantic uniform_example ;
  [%expect
    {|
      Warning: At 'string', line 6, column 10 to column 28, your Stan program has a uniform distribution on variable x. The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).
    |}]
