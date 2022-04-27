open Middle

module type PartialEvaluator = sig
  val try_eval_expr : Expr.Typed.t -> Expr.Typed.t
  val eval_prog : Program.Typed.t -> Program.Typed.t
end

module Make (StdLib : Frontend.Std_library_utils.Library) : PartialEvaluator
