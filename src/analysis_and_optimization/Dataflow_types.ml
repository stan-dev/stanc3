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

(** How a reference touches its variable. An [Increment] is [s += e] with [e]
    free of [s] ([target += e] included): it reads and writes [s], and two
    increments of the same variable commute (design §7.4). *)
type access_kind = Read | Write | Increment [@@deriving sexp_of, compare]

(** One reference to a variable inside a loop body. A whole-variable reference
    ([v], a [Decl] of [v], an increment of [v]) has [subs = []]. *)
type access =
  {var: string; subs: subscript list; kind: access_kind; label: label}
[@@deriving sexp_of, compare]

let access_reads (a : access) =
  match a.kind with Read | Increment -> true | Write -> false

let access_writes (a : access) =
  match a.kind with Write | Increment -> true | Read -> false

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

(** Classification of a dependence edge by the kinds of its two accesses
    (Kennedy and Allen §2.2): [True_dep] write then read, [Anti] read then
    write, [Output] write then write, [Effects] the ordering edge between two
    effectful statements. *)
type dep_kind = True_dep | Anti | Output | Effects
[@@deriving sexp_of, compare]

(** One leaf statement of a loop body. *)
type loop_node =
  { pos: int  (** lexical position in the body *)
  ; stmt: Middle.Stmt.Located.t
  ; accesses: access list
  ; effects: bool  (** prints, rejects or calls a user-defined function *) }

(** [src] must execute before [dst]. [dep] is the dependence of the pair of
    accesses that created the edge, with directions relative to the lexical
    order of the two statements (not to [src]/[dst]). *)
type loop_edge =
  {src: int; dst: int; var: string; kind: dep_kind; dep: dependence}

type loop_dependence_graph = {nodes: loop_node array; edges: loop_edge list}

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
