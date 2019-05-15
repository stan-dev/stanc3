open Core_kernel

module type S = sig
  type semantic_error
  type syntax_error
  type frontend_error
  type compiler_opts_error
  type compiler_opts

  val default_compiler_opts : compiler_opts

  val compiler_opts_of_string :
    string -> (compiler_opts, compiler_opts_error list) result

  val compile_from_file :
    opts:compiler_opts -> file:string -> (string, frontend_error list) result
end

module type Compiler = sig
  module type S = S

  module Make
      (F : Frontend_intf.S)
      (O : Optimization_intf.S)
      (B : Backend_intf.S) :
    S
    with type semantic_error := F.semantic_error
     and type syntax_error := F.syntax_error
     and type frontend_error := F.frontend_error
end
