open Std
open Middle
open Dataflow_types

type location = Dependence.Access_function.t Dependence.Accesses.location

module Writer_dependencies : sig
  (** A variable maps to the unique statement labels that may supply its value.
  *)
  type t

  val for_variable : string -> t -> label Set.Poly.t

  val writer_ids : string Set.Poly.t -> t -> label Set.Poly.t
  (** All writer labels, excluding the blocked variables. *)

  val writer_ids_for_variables :
    string Set.Poly.t -> string Set.Poly.t -> t -> label Set.Poly.t
  (** Writer labels for the selected variables, excluding blockers. Missing
      variables contribute no writers. *)
end

(** Immutable facts available before the reaching-definition fixed point. *)
type statement =
  { id: label
  ; pattern: (Expr.Typed.t, label) Stmt.Pattern.t
  ; meta: Location_span.t
  ; controls: controls
  ; loop_ids: label list
  ; effects: Dependence.Access_function.t Dependence.Accesses.t }

(** Completed analysis; empty results always mean no facts were found, rather
    than that the corresponding analysis has not run yet. *)
type stmt_info = {statement: statement; writers: Writer_dependencies.t}

val build_dependence_info :
  Program.Typed.t -> Stmt.Located.t -> stmt_info LabelMap.t

val build_log_prob_dependence_info : Program.Typed.t -> stmt_info LabelMap.t

val transitive_dependence_ids :
  stmt_info LabelMap.t -> label -> label Set.Poly.t

val variable_dependence_ids :
     stmt_info LabelMap.t
  -> string Set.Poly.t
  -> string Set.Poly.t
  -> label
  -> label Set.Poly.t

val read_variables_at :
  stmt_info LabelMap.t -> label Set.Poly.t -> string Set.Poly.t
