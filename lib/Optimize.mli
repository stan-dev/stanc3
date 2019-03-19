(* Code for optimization passes on the MIR

val function_inlining : Mir.stmt_loc Mir.prog -> Mir.stmt_loc Mir.prog
(** Inline all functions except for ones with forward declarations
    (e.g. recursive functions, mutually recursive functions, and
    functions without a definition *)

val loop_unrolling : Mir.stmt_loc Mir.prog -> Mir.stmt_loc Mir.prog
(** Unroll all for-loops with constant bounds, as long as they do
    not contain break or continue statements in their body at the
    top level *)

val list_collapsing : Mir.stmt_loc Mir.prog -> Mir.stmt_loc Mir.prog
(** Remove redundant SList constructors from the Mir that might have
    been introduced by other optimizations *)
*)
