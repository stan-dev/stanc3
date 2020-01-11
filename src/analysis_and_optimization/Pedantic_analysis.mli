open Core_kernel
open Middle

val print_warn_pedantic :
  Program.Typed.t -> unit
(**
   Print all pedantic mode warnings to stderr.
*)

val print_warn_sigma_unbounded :
  Program.Typed.t -> unit
(**
   Print warnings about unbounded parameters prefixed with "sigma".
*)

val print_warn_uniform :
   Program.Typed.t -> unit
(**
   Print warnings about using the uniform distribution
*)


val list_uniform :
  Program.Typed.t -> (Location_span.t * string) Set.Poly.t
(**
   Return a set each location and corresponding parameter name throughout the program that the uniform distribution is used.
*)

val list_sigma_unbounded :
  Program.Typed.t -> string Set.Poly.t
(**
   Return a set of parameters whose names are prefixed with sigma, and which are not bounded below by zero.
*)
