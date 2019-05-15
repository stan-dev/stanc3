open Mir
open Core_kernel

module type S = sig
  (* options for specifying the behaviour of the frontend *)
  type frontend_opts

  val frontend_opts_of_string : (frontend_opts, string) result
  val default_frontend_opts : frontend_opts

  (* the type of semantic errors *)
  type semantic_error

  (* the type of syntax errors *)
  type syntax_error
  type frontend_error = (syntax_error, semantic_error) Either.t

  val render_error : frontend_error -> string

  val mir_of_file :
       opts:frontend_opts
    -> file:string
    -> (typed_prog, frontend_error list) result

  val mir_of_string :
       opts:frontend_opts
    -> str:string
    -> (typed_prog, frontend_error list) result
end
