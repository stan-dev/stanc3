(* Code for optimization passes on the MIR *)

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
val constant_propagation : Mir.stmt_loc Mir.prog -> Mir.stmt_loc Mir.prog
(** Propagate constant values through variable assignments *)

val expression_propagation : Mir.stmt_loc Mir.prog -> Mir.stmt_loc Mir.prog
(** Propagate arbitrary expressions through variable assignments.
    This can be useful for opening up new possibilities for partial evaluation.
    It should be followed by some CSE or lazy code motion pass, however. *)

val copy_propagation : Mir.stmt_loc Mir.prog -> Mir.stmt_loc Mir.prog
(** Propagate copies of variables through assignments. *)

val dead_code_elimination  : Mir.stmt_loc Mir.prog -> Mir.stmt_loc Mir.prog
(** Eliminate semantically redundant code branches.
    This includes removing redundant assignments (because they will be overwritten)
    and removing redundant code in program branches that will never be reached. *)