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

let example1_program =
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

let%expect_test "Print pedantic mode warnings" =
  print_warn_pedantic example1_program ;
  [%expect
    {|
      Warning: Your Stan program has an unconstrained parameter "sigma" whose name begins with "sigma". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.
      Warning: Your Stan program has an unconstrained parameter "sigma_a" whose name begins with "sigma". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.
      Warning: Your Stan program has an unconstrained parameter "sigma_d" whose name begins with "sigma". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.
      Warning: Your Stan program has an unconstrained parameter "sigma_e" whose name begins with "sigma". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.
    |}]
