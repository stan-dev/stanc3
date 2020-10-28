open Core_kernel
open Ast

val deprecated_userdefined : string String.Table.t
val distribution_suffix : string -> bool
val without_suffix : string sexp_list -> string -> string
val is_distribution : string -> bool
val rename_distribution : string -> string
val rename_function : string -> string
val userdef_distributions : untyped_statement list option -> string list
val replace_suffix : typed_statement -> unit

val emit_warnings :
     ( (typed_expr_meta, fun_kind) expr_with
     , stmt_typed_located_meta
     , ((typed_expr_meta, fun_kind) expr_with, typed_expr_meta) lval_with
     , fun_kind )
     statement_with
     program
  -> typed_program
