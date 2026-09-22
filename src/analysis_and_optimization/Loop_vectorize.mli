(** Loop vectorization by pi-block code generation (design doc
    [design-docs/active/vectorize-loop-fission.md]). This stage only reports the
    dependence graph and pi-blocks of every source loop, behind
    [--debug-loop-vectorization]. *)

open Middle

val vectorize_loops : Program.Typed.t -> Program.Typed.t
(** Analyse every loop, innermost first, recording one report per source loop;
    the program is returned unchanged. *)

(** What happened to one loop. *)
type decision =
  | Left_alone of string
      (** a whole-loop check failed before any analysis: a bound with effects,
          [break] or [continue], or a bound variable written in the body *)
  | Analyzed of
      { map: Dependence_analysis.dep_info_map
      ; graph: Dependence_analysis.loop_graph }

type loop_report =
  { loc: Location_span.t
  ; header: string  (** the loop variable and bounds, [n in 1:N] *)
  ; decision: decision }

val loop_reports : unit -> loop_report list
(** The reports collected by the last [vectorize_loops] run, in program order.
*)

val pp_loop_report : loop_report Fmt.t
(** One loop of the [--debug-loop-vectorization] report. *)
