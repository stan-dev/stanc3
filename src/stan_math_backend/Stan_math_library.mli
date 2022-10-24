(** This module stores a table of all signatures from the Stan
    math C++ library which are exposed to Stan, and some helper
    functions for dealing with those signatures.
*)

include Frontend.Std_library_utils.Library

(** These functions are used by the drivers to display
    all available functions and distributions. They are
    not part of the Library interface since different drivers
    for different backends would likely want different behavior
    here *)

val pretty_print_all_math_sigs : unit Fmt.t
val pretty_print_all_math_distributions : unit Fmt.t

(** These functions related to variadic functions
    are specific to this backend and used
    during code generation *)

(* reduce_sum helpers *)
val is_reduce_sum_fn : string -> bool
