(* Code for optimization passes on the MIR *)

val function_inlining : Middle.Program.Typed.t -> Middle.Program.Typed.t
(** Inline all functions except for ones with forward declarations
    (e.g. recursive functions, mutually recursive functions, and
    functions without a definition *)

val static_loop_unrolling : Middle.Program.Typed.t -> Middle.Program.Typed.t
(** Unroll all for-loops with constant bounds, as long as they do
    not contain break or continue statements in their body at the
    top level *)

val one_step_loop_unrolling : Middle.Program.Typed.t -> Middle.Program.Typed.t
(** Unroll all loops for one iteration, as long as they do
    not contain break or continue statements in their body at the
    top level *)

val list_collapsing : Middle.Program.Typed.t -> Middle.Program.Typed.t
(** Remove redundant SList constructors from the Mir that might have
    been introduced by other optimizations *)

val block_fixing : Middle.Program.Typed.t -> Middle.Program.Typed.t
(** Make sure that SList constructors directly under if, for, while or fundef
    constructors are replaced with Block constructors.
    This should probably be run before we generate code. *)

val constant_propagation : Middle.Program.Typed.t -> Middle.Program.Typed.t
(** Propagate constant values through variable assignments *)

val expression_propagation : Middle.Program.Typed.t -> Middle.Program.Typed.t
(** Propagate arbitrary expressions through variable assignments.
    This can be useful for opening up new possibilities for partial evaluation.
    It should be followed by some CSE or lazy code motion pass, however. *)

val copy_propagation : Middle.Program.Typed.t -> Middle.Program.Typed.t
(** Propagate copies of variables through assignments. *)

val dead_code_elimination : Middle.Program.Typed.t -> Middle.Program.Typed.t
(** Eliminate semantically redundant code branches.
    This includes removing redundant assignments (because they will be overwritten)
    and removing redundant code in program branches that will never be reached. *)

val partial_evaluation : Middle.Program.Typed.t -> Middle.Program.Typed.t
(** Partially evaluate expressions in the program. This includes simplification using
    algebraic identities of logical and arithmetic operators as well as Stan math functions. *)

val lazy_code_motion : Middle.Program.Typed.t -> Middle.Program.Typed.t
(** Perform partial redundancy elmination using the lazy code motion algorithm. This
    subsumes common subexpression elimination and loop-invariant code motion. *)

val optimize_ad_levels : Middle.Program.Typed.t -> Middle.Program.Typed.t
(** Assign the optimal ad-levels to local variables. That means, make sure that
    variables only ever get treated as autodiff variables if they have some
    dependency on a parameter *)
