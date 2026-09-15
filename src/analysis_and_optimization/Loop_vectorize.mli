(** Loop vectorization by pi-block code generation (design doc
    [design-docs/active/vectorize-loop-fission.md]). *)

open Middle

val vectorize_loops : Program.Typed.t -> Program.Typed.t
(** Rewrite every loop, innermost first: statements that widen to a Stan Math
    container signature become vector statements, the rest stay in sequential
    loops emitted in dependence order. Loops that do not change are returned
    unchanged. *)

(** Why one leaf statement of a loop was or was not hoisted. *)
type hoist_outcome =
  | Hoisted
  | Reduced of string  (** hoisted as [s += sum(...)] for the accumulator [s] *)
  | Recurrence of Dataflow_types.loop_edge  (** true/output self-edge *)
  | In_cycle of int list  (** the other members of its pi-block *)
  | Effectful  (** print, reject or a user-defined function call *)
  | Not_widened of string  (** the reason widening refused *)

(** What happened to one loop. *)
type decision =
  | Left_alone of string
      (** a whole-loop check failed before any analysis: a bound with effects,
          break/continue, or a bound variable written in the body *)
  | Analyzed of
      { graph: Dataflow_types.loop_dependence_graph
      ; blocks: int list list
      ; outcomes: (int * hoist_outcome) list }

type loop_report =
  { loc: Location_span.t
  ; loopvar: string
  ; lower: Expr.Typed.t
  ; upper: Expr.Typed.t
  ; decision: decision }

val loop_reports : unit -> loop_report list
(** The reports collected by the last [vectorize_loops] run, in program order.
*)

val pp_loop_report : loop_report Fmt.t
(** One loop of the [--debug-loop-vectorization] report. *)
