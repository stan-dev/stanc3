(* Code for optimization passes on the MIR *)

val function_inlining : Mir.typed_prog -> Mir.typed_prog
(** Inline all functions except for ones with forward declarations
    (e.g. recursive functions, mutually recursive functions, and
    functions without a definition *)

val loop_unrolling : Mir.typed_prog -> Mir.typed_prog
(** Unroll all for-loops with constant bounds, as long as they do
    not contain break or continue statements in their body at the
    top level *)

val list_collapsing : Mir.typed_prog -> Mir.typed_prog
(** Remove redundant SList constructors from the Mir that might have
    been introduced by other optimizations *)

val constant_propagation : Mir.typed_prog -> Mir.typed_prog
(** Propagate constant values through variable assignments *)

val expression_propagation : Mir.typed_prog -> Mir.typed_prog
(** Propagate arbitrary expressions through variable assignments.
    This can be useful for opening up new possibilities for partial evaluation.
    It should be followed by some CSE or lazy code motion pass, however. *)

val copy_propagation : Mir.typed_prog -> Mir.typed_prog
(** Propagate copies of variables through assignments. *)

val dead_code_elimination : Mir.typed_prog -> Mir.typed_prog
(** Eliminate semantically redundant code branches.
    This includes removing redundant assignments (because they will be overwritten)
    and removing redundant code in program branches that will never be reached. *)

val partial_evaluation : Mir.typed_prog -> Mir.typed_prog
(** Partially evaluate expressions in the program. This includes simplification using
    algebraic identities of logical and arithmetic operators as well as Stan math functions. *)

val lazy_code_motion : Mir.typed_prog -> Mir.typed_prog
(** Perform partial redundancy elmination using the lazy code motion algorithm. This
    subsumes common subexpression elimination and loop-invariant code motion. *)
