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
      (** mentions the loop variable in a form [linear] cannot hold: a gather
          [idx[n]], a product [2 * n], a second symbol or a negated one
          ([n - k], [N - n]) *)
[@@deriving sexp_of, compare]

(** One integer index expression as a function of the loop being analysed (Goff,
    Kennedy and Tseng 1991, restricted to one loop and stride 1). *)
type point =
  | Invariant of linear
      (** free of the loop variable and of the written set (ZIV) *)
  | Affine of linear
      (** [loopvar + offset] (SIV); [v[n]] is [{const = 0; symbol = None}] *)
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
