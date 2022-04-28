open Middle

module type PARTIAL_EVALUATOR = sig
  val try_eval_expr : Expr.Typed.t -> Expr.Typed.t
  val eval_prog : Program.Typed.t -> Program.Typed.t
end

module Make (StdLibrary : Frontend.Std_library_utils.Library) :
  PARTIAL_EVALUATOR
