include Compiler_intf
include Core_kernel

module Make
    (F : Frontend_intf.S)
    (O : Optimization_intf.S)
    (B : Backend_intf.S) :
  S
  with type semantic_error := F.semantic_error
   and type syntax_error := F.syntax_error
   and type frontend_error := F.frontend_error = struct
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

  let compiler_opts_of_string str = Error [Frontend_opts_error ("todo " ^ str)]

  let compile_from_file ~opts ~file =
    F.mir_of_file ~opts:opts.frontend_opts ~file
    |> Result.map ~f:(O.optimize ~opts:opts.optimization_opts)
    |> Result.map ~f:(B.mir_to_string ~opts:opts.backend_opts)
end
