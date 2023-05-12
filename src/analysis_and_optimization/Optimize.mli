(* Code for optimization passes on the MIR *)
open Optimize_intf

val all_optimizations : optimization_settings
val no_optimizations : optimization_settings

type optimization_level = O0 | O1 | Oexperimental

val level_optimizations : optimization_level -> optimization_settings

(** Produce an optimizer for the MIR which is parameterized by the
    given library of functions. These are used in the partial evaluator
    and memory optimizations
    *)
module Make (StdLibrary : Frontend.Std_library_utils.Library) : OPTIMIZER
