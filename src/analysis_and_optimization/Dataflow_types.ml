open Std
open Std.Compare
open Std.Sexp_conv

(***********************************)
(* Basic datatypes                 *)
(***********************************)

(** A label is a unique identifier for a node in the dataflow/dependency graph,
    and often corresponds to one node in the Mir. *)
type label = int [@@deriving sexp_of, compare]

(** Representation of an expression that can be assigned to. This should also be
    able to represent indexed variables, but we don't support that yet. *)
type vexpr = VVar of string [@@deriving sexp_of]

(** A 'reaching definition' (or reaching_defn or RD) statement (v, l) says that
    the variable v could have been affected at the label l. *)
type reaching_defn = vexpr * label [@@deriving sexp_of]

(***********************************)
(* Loop access model               *)
(***********************************)

(** A loop-invariant integer expression in normal form,
    [const + sum_i (coeff_i * term_i)]: the terms are pairwise distinct under
    [Expr.Typed.compare], sorted, and have non-zero coefficients. It is the
    single-loop analogue of a loop-invariant SCEV in LLVM's [DependenceAnalysis]
    or of a chrec base in GCC's [tree-chrec]: subtracting two linear forms
    cancels identical symbols, which is what lets [x[n + k]] and [x[n + k]] be
    recognised as the same element. *)
type linear = {const: int; terms: (int * Middle.Expr.Typed.t) list}
[@@deriving sexp_of, compare]

(** Why a subscript could not be expressed as an affine function of the loop
    variable. Mirrors the distinctions LLVM's LoopAccessAnalysis reports
    ([IndirectUnsafe] for gathers, [Unknown] otherwise) at the granularity a
    debug report needs. *)
type varying_kind =
  | Slice  (** [:], [a:], [a:b] *)
  | Multi_index  (** [v[idxs]] with an array index *)
  | Written  (** mentions a variable assigned in the loop body *)
  | Gather  (** [v[idx[n]]]: the loop variable under another index *)
  | Nonlinear  (** [n * k], [n * n], [f(n)], ... *)
[@@deriving sexp_of, compare]

(** How one index position of a reference varies with the loop being analysed.

    This is the single-loop restriction of the subscript classification in Goff,
    Kennedy and Tseng, "Practical Dependence Testing" (PLDI 1991), of LLVM's
    [DependenceAnalysis] ([Subscript] with a [SCEVAddRecExpr] per loop) and of
    GCC's chains of recurrences ([{base, +, step}_loop]).
    [Affine {coeff; offset}] is the recurrence
    [{offset + coeff * lower, +, coeff}] over the loop; only the coefficient is
    restricted to an integer literal, because without a non-zero proof a
    symbolic step supports no conclusion. *)
type subscript =
  | Invariant of linear
      (** Mentions neither the loop variable nor any variable written in the
          loop body (LLVM ZIV). Two [Invariant]s with equal symbolic terms name
          the same element in every iteration iff their constants are equal. *)
  | Affine of {coeff: int; offset: linear}
      (** [coeff * loopvar + offset] with [coeff <> 0] (LLVM SIV). The common
          case is [{coeff = 1; offset = {const = 0; terms = []}}], i.e. [v[n]].
      *)
  | Varying of varying_kind
      (** Anything else (LLVM [NonLinear] / GCC [chrec_dont_know]): a gather
          [idx[n]], an inner loop variable, a body-written scalar, a slice
          [a:b], [:], or a [MultiIndex]. *)
[@@deriving sexp_of, compare]

(** One reference to a variable inside a loop body. A whole-variable reference
    ([v], or a [Decl] of [v]) has [subs = []]. *)
type access = {var: string; subs: subscript list; is_write: bool; label: label}
[@@deriving sexp_of, compare]

(** Relation between the iteration of the first access and the iteration of the
    second: [Lt] means the first access happens in an earlier iteration. *)
type direction = Lt | Eq | Gt [@@deriving sexp_of, compare]

(** Result of testing two accesses to the same variable. *)
type dependence =
  | Independent  (** The references can never name the same element. *)
  | Dependent of {directions: direction Set.Poly.t; distance: int option}
      (** [directions] is the set of possible relations between the source
          iteration and the sink iteration; [distance] is the exact iteration
          distance when the test can compute it. [{Eq}, Some 0] is a
          loop-independent dependence; [{Lt; Eq; Gt}, None] is the conservative
          "confused" answer. *)
[@@deriving sexp_of]

(** The most recently nested control flow (block start, if/then, or loop)

    This isn't included in the traversal_state because it only flows downward
    through the tree, not across and up like everything else *)
type cf_state = label

module LabelMap = struct
  include Int.Map

  let sexp_of_t f t = sexp_of_list (sexp_of_pair sexp_of_int f) (to_list t)
end

module ExprSet = Set.Make (Middle.Expr.Typed)
module ExprMap = Map.Make (Middle.Expr.Typed)
