(** Translate from the AST to the MIR *)
open Mir

val trans_prog :
  string -> Ast.typed_program -> (mtype_loc_ad with_expr, stmt_loc) prog
