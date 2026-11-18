(** Why a type is demoted from SoA to AoS *)
type reason

val pp_reason : Format.formatter -> reason -> unit
val get_warnings : unit -> (Middle.Location_span.t * reason) list
val reset_warnings : unit -> unit

val pp_mem_patterns : Format.formatter -> Middle.Program.Typed.t -> unit
(** Print every eligible variable in the program and what memory pattern it has
*)

val query_initial_demotable_stmt :
     bool
  -> string Std.Set.Poly.t
  -> Middle.Stmt.Located.t
  -> string Std.Set.Poly.t

val query_demotable_stmt :
     string Std.Set.Poly.t
  -> Middle.Stmt.Located.Non_recursive.t
  -> string Std.Set.Poly.t

val modify_expr_pattern :
     ?force_demotion:bool
  -> string Std.Set.Poly.t
  -> Dataflow_types.ExprSet.elt Middle.Expr.Pattern.t
  -> Dataflow_types.ExprSet.elt Middle.Expr.Pattern.t

val modify_stmt_pattern :
     (Dataflow_types.ExprSet.elt, Middle.Stmt.Located.t) Middle.Stmt.Pattern.t
  -> string Std.Set.Poly.t
  -> (Dataflow_types.ExprSet.elt, Middle.Stmt.Located.t) Middle.Stmt.Pattern.t
