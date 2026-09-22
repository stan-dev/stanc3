(** Loop vectorization by pi-block code generation (Allen and Kennedy 1987 §5.2;
    design doc [design-docs/active/vectorize-loop-fission.md]): a statement that
    is a pi-block of its own and widens becomes one vector statement, the rest
    of the body stays in loops with the original header, emitted in dependence
    order. *)

open Middle

val vectorize_loops : Program.Typed.t -> Program.Typed.t
(** Rewrite every loop, innermost first, recording one report per source loop.
*)

val loop_reports : unit -> string list
(** The [--debug-loop-vectorization] report of the last [vectorize_loops] run,
    one block per source loop in program order: the loop header, then either the
    reason the loop was left alone or one line per leaf with the leaf's outcome,
    the edges and the pi-blocks. *)
