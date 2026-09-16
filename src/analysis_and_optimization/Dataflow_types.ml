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
(* Access model                    *)
(***********************************)

(** [const + sum_i (coeff_i * term_i)] with distinct, sorted terms and non-zero
    coefficients; the single-loop analogue of a loop-invariant SCEV. *)
type linear = {const: int; terms: (int * Middle.Expr.Typed.t) list}
[@@deriving sexp_of, compare]

(** Why an index expression is not affine in the loop variable. *)
type varying_kind =
  | Written  (** mentions a variable from the written set *)
  | Gather  (** [v[idx[n]]]: the loop variable under another index *)
  | Nonlinear  (** [n * k], [n * n], [f(n)], ... *)
[@@deriving sexp_of, compare]

(** One integer index expression as a function of the loop being analysed (Goff,
    Kennedy and Tseng 1991, restricted to one loop and a literal step). *)
type point =
  | Invariant of linear
      (** free of the loop variable and of the written set (ZIV) *)
  | Affine of {coeff: int; offset: linear}
      (** [coeff * loopvar + offset] with [coeff <> 0] (SIV); [v[n]] is
          [{coeff = 1; offset = 0}] *)
  | Varying of varying_kind  (** anything else *)
[@@deriving sexp_of, compare]

(** An [Increment] ([target += e]) reads and writes the incremented variable,
    and two increments of the same variable commute. *)
type access_kind = Read | Write | Increment [@@deriving sexp_of, compare]

(** One reference to [var]; [subs] holds the source indices as [point]s and is
    empty for a whole-variable reference ([v], a [Decl]). *)
type access =
  {var: string; subs: point Middle.Index.t list; kind: access_kind; label: label}
[@@deriving sexp_of, compare]

let access_reads (a : access) =
  match a.kind with Read | Increment -> true | Write -> false

let access_writes (a : access) =
  match a.kind with Write | Increment -> true | Read -> false

(** Relation between the iteration of the first access and the iteration of the
    second: [Lt] means the first access happens in an earlier iteration. *)
type direction = Lt | Eq | Gt [@@deriving sexp_of, compare]

(** Result of testing whether two accesses to the same variable can name the
    same element. *)
type dependence =
  | Independent  (** The references can never name the same element. *)
  | Dependent of {directions: direction Set.Poly.t; distance: int option}
      (** possible iteration relations and the exact distance if known;
          [{Lt; Eq; Gt}, None] is the conservative "confused" answer *)
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
