open Std
open Middle

val mir_uninitialized_variables :
  Program.Typed.t -> (Location_span.t * string) Set.Poly.t
(** Returns (read location, variable name) pairs for the reads that some path
    reaches without an assignment to the variable. The regions checked are each
    function body, transformed data, the log density (transformed data, then the
    model block) and generated quantities. Data, parameters and function
    arguments count as initialized. *)
