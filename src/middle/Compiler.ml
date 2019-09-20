include Core_kernel

module type Frontend = sig
  (* options for specifying the behaviour of the frontend *)
  type frontend_opts

  val frontend_opts_of_string : (frontend_opts, string) result
  val default_frontend_opts : frontend_opts

  type frontend_error

  val render_error : frontend_error -> string

  val mir_of_file :
       opts:frontend_opts
    -> file:string
    -> (Program.Typed.t, frontend_error) result

  val mir_of_string :
       opts:frontend_opts
    -> str:string
    -> (Program.Typed.t, frontend_error) result
end

module type Backend = sig
  type backend_opts

  val backend_opts_of_string : string -> (backend_opts, string) result
  val default_backend_opts : backend_opts

  (* the type of backend representation *)
  type repr

  val mir_to_repr : opts:backend_opts -> Program.Typed.t -> repr
  val mir_to_string : opts:backend_opts -> Program.Typed.t -> string
end

module type Optimization = sig
  (* variant of possible optimization levels *)
  type optimization_opts

  (* parse level from string, for use in e.g. command line argument parser *)
  val optimization_opts_of_string :
    string -> (optimization_opts, string) result

  val default_optimization_opts : optimization_opts
  val optimize : opts:optimization_opts -> Program.Typed.t -> Program.Typed.t
end

module Compiler = struct
  module type S = sig
    type frontend_error
    type compiler_opts_error
    type compiler_opts

    val default_compiler_opts : compiler_opts

    val compiler_opts_of_string :
      string -> (compiler_opts, compiler_opts_error list) result

    val compile_from_file :
      opts:compiler_opts -> file:string -> (string, frontend_error) result
  end

  module Make (F : Frontend) (O : Optimization) (B : Backend) :
    S with type frontend_error := F.frontend_error = struct
    type compiler_opts =
      { frontend_opts: F.frontend_opts
      ; optimization_opts: O.optimization_opts
      ; backend_opts: B.backend_opts }

    let default_compiler_opts =
      { frontend_opts= F.default_frontend_opts
      ; optimization_opts= O.default_optimization_opts
      ; backend_opts= B.default_backend_opts }

    [@@@ocaml.warning "-37"]

    type compiler_opts_error =
      | Frontend_opts_error of string
      | Optimize_opts_error of string
      | Backend_opts_error of string

    let compiler_opts_of_string str =
      Error [Frontend_opts_error ("todo " ^ str)]

    let compile_from_file ~opts ~file =
      F.mir_of_file ~opts:opts.frontend_opts ~file
      |> Result.map ~f:(O.optimize ~opts:opts.optimization_opts)
      |> Result.map ~f:(B.mir_to_string ~opts:opts.backend_opts)
  end
end
