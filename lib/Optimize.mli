(* Code for optimization passes on the MIR *)

(** Inline all functions except for ones with forward declarations
    (e.g. recursive functions, mutually recursive functions, and
    functions without a definition *)
val function_inlining : Mir.stmt_loc Mir.prog -> Mir.stmt_loc Mir.prog

(** Unroll all for-loops with constant bounds, as long as they do
    not contain break or continue statements in their body at the
    top level *)
val loop_unrolling : Mir.stmt_loc Mir.prog -> Mir.stmt_loc Mir.prog
