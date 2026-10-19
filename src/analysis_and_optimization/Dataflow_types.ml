open Std
open Std.Sexp_conv

(** A label identifies one statement in a statement map or graph. Labels from
    different maps do not match. *)
type label = int [@@deriving sexp_of]

(** A possible source of the value of a variable. [(x, Some l)] means that the
    statement with label [l] can write [x]. [(x, None)] means that [x] gets a
    value from outside the analyzed statement. *)
type reaching_defn = string * label option [@@deriving sexp_of]

(** A [for] or [while] loop around a statement. [id] is the label of the loop
    statement. [var] is the loop variable of a [for], or [None] for a [while].
*)
type loop = {id: label; var: string option}

(** A statement that controls whether, or how often, another statement runs.
    {!Dataflow_utils.build_cf_graphs} gives each statement a list of controls.
    The list holds the enclosing ifs and loops, outermost first, and then the
    jumps. *)
(** The branch of an [if] that holds a statement. *)
type branch = Then | Else

type control =
  | Enclosing_if of {id: label; branch: branch}
      (** An [if] around the statement. [id] is the label of the [if], and
          [branch] is the branch that holds the statement. *)
  | Enclosing_loop of loop  (** A [for] or [while] around the statement. *)
  | Jump of label
      (** A [break], [continue] or [return] that can skip the statement. For a
          loop statement, a [break] or [return] in its body that can end the
          loop. *)

module LabelMap = struct
  include Int.Map

  let sexp_of_t f t = sexp_of_list (sexp_of_pair sexp_of_int f) (to_list t)
end

module ExprSet = Set.Make (Middle.Expr.Typed)
module ExprMap = Map.Make (Middle.Expr.Typed)
