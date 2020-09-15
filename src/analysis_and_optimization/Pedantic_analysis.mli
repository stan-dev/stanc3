open Middle

val print_warn_pedantic : Program.Typed.t -> unit
(**
   Print all pedantic mode warnings to stderr.
*)

val print_warn_uninitialized : Program.Typed.t -> unit
(**
   Print warnings about each variable which is used before being initialized
*)

val warning_set :
     Middle.Program.Typed.t
  -> (Middle.Location_span.t * string) Core_kernel.Set.Poly.t

val uninitialized_warnings :
     Middle.Program.Typed.t
  -> (Middle.Location_span.t * string) Core_kernel.Set.Poly.t

val print_warning_set :
  (Middle.Location_span.t * string) Core_kernel.Set.Poly.t -> Base.unit
