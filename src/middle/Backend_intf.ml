open Mir

module type S = sig
  type backend_opts

  val backend_opts_of_string : string -> (backend_opts, string) result
  val default_backend_opts : backend_opts

  (* the type of backend representation *)
  type repr

  val mir_to_repr : opts:backend_opts -> typed_prog -> repr
  val mir_to_string : opts:backend_opts -> typed_prog -> string
end
