open Std
open Common
open Middle

let inline source =
  Gensym.reset_danger_use_cautiously ();
  Test_utils.mir_of_string source
  |> Analysis_and_optimization.Optimize.function_inlining

let rec events_expr events (e : Expr.Typed.t) =
  let events =
    match e.pattern with
    | FunApp (StanLib ((("exp" | "log" | "normal_rng") as name), _, _), _) ->
        events @ [name]
    | _ -> events in
  Expr.Pattern.fold events_expr events e.pattern

let rec events_stmt events (s : Stmt.Located.t) =
  let events =
    match s.pattern with
    | NRFunApp (CompilerInternal FnPrint, _) -> events @ ["print"]
    | _ -> events in
  Stmt.Pattern.fold events_expr events_stmt events s.pattern

let print_events statements =
  List.fold_left ~f:events_stmt ~init:[] statements
  |> String.concat ~sep:", " |> print_endline

let%expect_test "evaluate a repeated scalar actual once" =
  let mir =
    inline
      {|
      functions {
        real piecewise(real x) {
          if (x > 0) return x * x;
          return -x;
        }
        real single_use(real x) { return x; }
      }
      parameters { real theta; }
      model {
        target += piecewise(exp(theta));
        target += piecewise(theta);
        target += piecewise(2.0);
        target += single_use(log(theta));
      }
      |}
  in
  print_events mir.log_prob;
  [%expect {| exp, log |}]

let%expect_test "bind scalar actuals in argument evaluation order" =
  let mir =
    inline
      {|
      functions {
        real twice(real x, real y) { return x * x + y * y; }
        real announce(real x) { print(x); return x; }
      }
      generated quantities {
        real a = twice(normal_rng(0, 1), announce(2));
        real b = twice(announce(3), normal_rng(4, 1));
      }
      |}
  in
  print_events mir.generate_quantities;
  [%expect {| print, normal_rng, normal_rng, print |}]

let%expect_test "bind repeated scalar actuals for void functions" =
  let mir =
    inline
      {|
      functions { void twice(real x) { print(x, x); } }
      generated quantities { twice(normal_rng(0, 1)); }
      |}
  in
  print_events mir.generate_quantities;
  [%expect {| normal_rng, print |}]
