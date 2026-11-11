open Std
open Middle
open Dataflow_types

type access = Dependence.Access_function.t Dependence.Accesses.access

type reaching_definition =
  {access: access; statement_id: label option; loop_ids: label list}

type stmt_info =
  { id: label
  ; pattern: (Expr.Typed.t, label) Stmt.Pattern.t
  ; meta: Location_span.t
  ; controls: controls
  ; loop_ids: label list
  ; accesses: access list
  ; reaching_definitions: reaching_definition Set.Poly.t
  ; writers: (string * label) Set.Poly.t }

val build_dependence_info :
  Program.Typed.t -> Stmt.Located.t -> stmt_info LabelMap.t

val build_log_prob_dependence_info : Program.Typed.t -> stmt_info LabelMap.t

val transitive_dependence_ids :
  stmt_info LabelMap.t -> label -> label Set.Poly.t

val var_dependence_ids :
     stmt_info LabelMap.t
  -> ?blockers:string Set.Poly.t
  -> string Set.Poly.t
  -> label
  -> label Set.Poly.t

val used_vars_at : stmt_info LabelMap.t -> label Set.Poly.t -> string Set.Poly.t
val pp_pattern : (Expr.Typed.t, label) Stmt.Pattern.t Fmt.t
val pp_controls : loop_name:(label -> string) -> controls Fmt.t

val pp_definitions :
  loop_name:(label -> string) -> reaching_definition Set.Poly.t Fmt.t

val pp_dependences : stmt_info Fmt.t
