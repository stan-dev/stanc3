open Core_kernel
open Middle
open Factor_graph

val print_warn_pedantic :
  Program.Typed.t -> unit
(**
   Print all pedantic mode warnings to stderr.
*)

val print_warn_uninitialized :
  Program.Typed.t -> unit
(**
   Print warnings about each variable which is used before being initialized
*)

val print_warn_sigma_unbounded :
  Program.Typed.t -> unit
(**
   Print warnings about unbounded parameters prefixed with "sigma".
*)

val print_warn_unused_params :
  factor_graph -> Program.Typed.t -> unit
(**
   Print warnings about parameters that don't appear outside of the parameter block.
*)


val print_warn_hard_constrained :
  Program.Typed.t -> unit
(**
   Print warnings about putting hard upper and lower constraints on parameters
*)


val print_warn_multi_twiddles :
  Program.Typed.t -> unit
(**
   Print a warning for each parameter which is on the left-hand side of more than one twiddle, or equivalent TargetPE
*)

val print_warn_unscaled_constants :
  Program.Typed.t -> unit
(**
   Print warnings about using unscaled constants
*)


val list_unscaled_constants :
  Program.Typed.t -> (Location_span.t * string) Set.Poly.t
(**
   Return a set of each constant and corresponding location whose magnitude is < 0.1 or > 10
*)

val list_multi_twiddles :
  Program.Typed.t -> (string * Location_span.t Set.Poly.t) Set.Poly.t
(**
   Return the set of parameters which are on the LHS of multiple twiddle statements, along with the locations of those statements.
*)

val list_hard_constrained:
  Program.Typed.t -> string Set.Poly.t
(**
   Return the set of parameters with upper/lower bounds, which are not (0, 1) or (-1, 1)
*)

val list_sigma_unbounded :
  Program.Typed.t -> string Set.Poly.t
(**
   Return a set of parameters whose names are prefixed with sigma, and which are not bounded below by zero.
*)

val list_unused_params :
  factor_graph -> Program.Typed.t -> string Set.Poly.t
(**
   Return the set of parameters which are not referenced outside of the parameter block.
*)
