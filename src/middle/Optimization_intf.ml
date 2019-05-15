open Mir

module type S = sig
  (* variant of possible optimization levels *)
  type optimization_opts

  (* parse level from string, for use in e.g. command line argument parser *)
  val optimization_opts_of_string :
    string -> (optimization_opts, string) result

  val default_optimization_opts : optimization_opts
  val optimize : opts:optimization_opts -> typed_prog -> typed_prog
end
