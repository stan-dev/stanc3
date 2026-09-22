(** Loop vectorization by pi-block code generation (Allen and Kennedy 1987 §5.2;
    design doc [design-docs/active/vectorize-loop-fission.md]): a statement that
    is a pi-block of its own and widens becomes one vector statement, the rest
    of the body stays in loops with the original header, emitted in dependence
    order. *)

open Middle

val vectorize_loops : Program.Typed.t -> Program.Typed.t
(** Rewrite every loop, innermost first, recording one report per source loop.
*)

(** What became of one leaf of a loop body. *)
type outcome =
  | Hoisted  (** one vector statement *)
  | Recurrence of Dependence_analysis.edge
      (** the leaf depends on an earlier iteration of itself *)
  | In_cycle of Dataflow_types.label list
      (** the leaf and the listed leaves depend on each other *)
  | Effectful  (** prints, rejects or calls a user function as a statement *)
  | Not_widened of string  (** the statement has no vector form, and why *)

(** What happened to one loop. *)
type decision =
  | Left_alone of string
      (** a whole-loop check failed before any analysis: a bound with effects,
          [break] or [continue], or a bound variable written in the body *)
  | Analyzed of
      { map: Dependence_analysis.dep_info_map
      ; graph: Dependence_analysis.loop_graph
      ; outcomes: (Dataflow_types.label * outcome) list }

type loop_report =
  { loc: Location_span.t
  ; header: string  (** the loop variable and bounds, [n in 1:N] *)
  ; decision: decision }

val loop_reports : unit -> loop_report list
(** The reports collected by the last [vectorize_loops] run, in program order.
*)

val pp_loop_report : loop_report Fmt.t
(** One loop of the [--debug-loop-vectorization] report. *)
