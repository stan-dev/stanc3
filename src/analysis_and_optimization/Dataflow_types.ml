open Std
open Std.Compare
open Std.Sexp_conv

(***********************************)
(* Basic datatypes                 *)
(***********************************)

(** A label is a unique identifier for a node in the dataflow/dependency graph,
    and often corresponds to one node in the Mir. *)
type label = int [@@deriving sexp_of, compare]

(** A 'reaching definition' (or reaching_defn or RD) statement (v, l) says that
    the variable named v could have been affected at the label l. *)
type reaching_defn = string * label [@@deriving sexp_of]

(***********************************)
(* Access model                    *)
(***********************************)

(** An integer constant plus at most one loop-invariant symbol, an expression
    compared structurally; [k + 1] is [{const = 1; symbol = Some k}]. *)
type linear = {const: int; symbol: Middle.Expr.Typed.t option}
[@@deriving sexp_of, compare]

(** Why an index expression is not [loopvar + offset] or [offset]. *)
type varying_kind =
  | Written  (** mentions a variable from the written set *)
  | Nonlinear
      (** mentions a loop variable in a form [linear] cannot hold: a gather
          [idx[n]], a product [2 * n], two loop variables or a negated one
          ([n + m], [N - n]) *)
[@@deriving sexp_of, compare]

(** One integer index expression as a function of the enclosing loop variables
    (Goff, Kennedy and Tseng 1991, stride 1). *)
type point =
  | Invariant of linear
      (** free of every loop variable and of the written set (ZIV) *)
  | Affine of {loopvar: string; offset: linear}
      (** [loopvar + offset] for one enclosing loop (SIV); [v[n]] is
          [{loopvar = "n"; offset = {const = 0; symbol = None}}] *)
  | Varying of varying_kind
      (** not comparable by the element test; any pair is [confused] *)
[@@deriving sexp_of, compare]

(** An [Increment] ([target += e]) reads and writes the incremented variable,
    and two increments of the same variable commute. *)
type access_kind = Read | Write | Increment [@@deriving sexp_of, compare]

(** One reference to [var]; [subs] holds the source indices as [point]s and is
    empty for a whole-variable reference ([v], a [Decl]). *)
type access = {var: string; subs: point Middle.Index.t list; kind: access_kind}
[@@deriving sexp_of, compare]

(** Relation between the iteration of the first access and the iteration of the
    second at one loop level: [Lt] means the first access happens in an earlier
    iteration of that loop. *)
type direction = Lt | Eq | Gt [@@deriving sexp_of, compare]

(** The relation at one loop level: the possible directions and the exact
    distance if known; [{Lt; Eq; Gt}, None] leaves the level unconstrained. *)
type level = {directions: direction Set.Poly.t; distance: int option}
[@@deriving sexp_of]

(** Result of testing whether two accesses to the same variable can name the
    same element. *)
type dependence =
  | Independent  (** The references can never name the same element. *)
  | Dependent of level list
      (** a direction vector over the loops enclosing both accesses, outermost
          first (Allen and Kennedy 1987 §2); empty when no loop encloses both *)
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
