open Core_kernel

module type S = sig
  (* options for specifying the behaviour of the frontend *)
  type frontend_opts

  val frontend_opts_of_string : (frontend_opts, string) result
  val default_frontend_opts : frontend_opts

  type frontend_error

  val render_error : frontend_error -> string

  val mir_of_file :
       opts:frontend_opts
    -> file:string
    -> (Mir.typed_prog, frontend_error) result

  val mir_of_string :
    opts:frontend_opts -> str:string -> (Mir.typed_prog, frontend_error) result
end

module type Frontend = sig
  module type S = S
end
