open Core_kernel
open Ast

val deprecated_userdefined : string String.Table.t
val distribution_suffix : string -> bool
val without_suffix : string sexp_list -> string -> string
val is_distribution : string -> bool
val rename_distribution : string -> string
val rename_function : string -> string

val userdef_distributions :
     ( (located_meta, unit) expr_with
     , located_meta
     , ((located_meta, unit) expr_with, located_meta) lval_with
     , unit )
     statement_with
     sexp_list
     sexp_option
  -> string sexp_list

val emit_warnings :
     ( (typed_expr_meta, fun_kind) expr_with
     , stmt_typed_located_meta
     , ((typed_expr_meta, fun_kind) expr_with, typed_expr_meta) lval_with
     , fun_kind )
     statement_with
     program
  -> typed_program