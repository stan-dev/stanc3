(* Code for optimization passes on the MIR *)
open Middle

val function_inlining : Program.Typed.t -> Program.Typed.t
(** Inline all functions except for ones with forward declarations
    (e.g. recursive functions, mutually recursive functions, and
    functions without a definition *)

val static_loop_unrolling : Program.Typed.t -> Program.Typed.t
(** Unroll all for-loops with constant bounds, as long as they do
    not contain break or continue statements in their body at the
    top level *)

val one_step_loop_unrolling : Program.Typed.t -> Program.Typed.t
(** Unroll all loops for one iteration, as long as they do
    not contain break or continue statements in their body at the
    top level *)

val list_collapsing : Program.Typed.t -> Program.Typed.t
(** Remove redundant SList constructors from the Mir that might have
    been introduced by other optimizations *)

val block_fixing : Program.Typed.t -> Program.Typed.t
(** Make sure that SList constructors directly under if, for, while or fundef
    constructors are replaced with Block constructors.
    This should probably be run before we generate code. *)

val constant_propagation :
  ?preserve_stability:bool -> Program.Typed.t -> Program.Typed.t
(** Propagate constant values through variable assignments *)

val expression_propagation :
  ?preserve_stability:bool -> Program.Typed.t -> Program.Typed.t
(** Propagate arbitrary expressions through variable assignments.
    This can be useful for opening up new possibilities for partial evaluation.
    It should be followed by some CSE or lazy code motion pass, however. *)

val copy_propagation : Program.Typed.t -> Program.Typed.t
(** Propagate copies of variables through assignments. *)

val dead_code_elimination : Program.Typed.t -> Program.Typed.t
(** Eliminate semantically redundant code branches.
    This includes removing redundant assignments (because they will be overwritten)
    and removing redundant code in program branches that will never be reached. *)

val partial_evaluation : Program.Typed.t -> Program.Typed.t
(** Partially evaluate expressions in the program. This includes simplification using
    algebraic identities of logical and arithmetic operators as well as Stan math functions. *)

val lazy_code_motion :
  ?preserve_stability:bool -> Program.Typed.t -> Program.Typed.t
(** Perform partial redundancy elmination using the lazy code motion algorithm. This
    subsumes common subexpression elimination and loop-invariant code motion. *)

val optimize_ad_levels : Program.Typed.t -> Program.Typed.t
(** Assign the optimal ad-levels to local variables. That means, make sure that
    variables only ever get treated as autodiff variables if they have some
    dependency on a parameter *)

val allow_uninitialized_decls : Program.Typed.t -> Program.Typed.t
(** Marks Decl types such that, if the first assignment after the decl
    assigns to the full object, allow the object to be constructed but 
    not uninitialized.  *)

(** Interface for turning individual optimizations on/off. Useful for testing
    and for top-level interface flags. *)
type optimization_settings =
  { function_inlining: bool
  ; static_loop_unrolling: bool
  ; one_step_loop_unrolling: bool
  ; list_collapsing: bool
  ; block_fixing: bool
  ; allow_uninitialized_decls: bool
  ; constant_propagation: bool
  ; expression_propagation: bool
  ; copy_propagation: bool
  ; dead_code_elimination: bool
  ; partial_evaluation: bool
  ; lazy_code_motion: bool
  ; optimize_ad_levels: bool
  ; preserve_stability: bool
  ; optimize_soa: bool }

val all_optimizations : optimization_settings
val no_optimizations : optimization_settings

type optimization_level = O0 | O1 | Oexperimental

val level_optimizations : optimization_level -> optimization_settings

val optimization_suite :
  ?settings:optimization_settings -> Program.Typed.t -> Program.Typed.t
(** Perform all optimizations in this module on the MIR in an appropriate order. *)
