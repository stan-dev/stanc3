open Std
open Std.Sexp_conv

(** A label is an integer that identifies one statement in a statement map or
    graph. A {e statement map} ({!Dataflow_utils.build_statement_map}) maps each
    label to a statement. Labels from different maps do not match. *)
type label = int [@@deriving sexp_of]

(** A possible source of the value of a variable at a statement. The source is a
    write that can run before the statement with no write between them that
    overwrites the value. Such a write is a {e reaching definition}
    ({!Monotone_framework.reaching_definitions_mfp}). [(x, Some l)] means that
    the statement with label [l] can write [x]. [(x, None)] means that [x] gets
    a value from outside the analyzed statement (the statement that the analysis
    runs on), such as data. *)
type reaching_defn = string * label option [@@deriving sexp_of]

(** A [for] or [while] loop around a statement. [id] is the label of the loop
    statement. [var] is the loop variable of a [for], or [None] for a [while].
*)
type loop = {id: label; var: string option}

(** The branch of an [if] that holds a statement. *)
type branch = Then | Else

(** A {e control} of a statement is another statement that decides whether, or
    how often, the statement runs. {!Dataflow_utils.build_cf_graphs} gives each
    statement a list of controls. The list first holds the ifs and loops around
    the statement, outermost first. These are the {e enclosing} ifs and loops.
    Then the list holds the {e jumps}, which are the [break], [continue] and
    [return] statements that can skip the statement. *)
type control =
  | Enclosing_if of {id: label; branch: branch}
      (** An [if] around the statement. [id] is the label of the [if], and
          [branch] is the branch that holds the statement. *)
  | Enclosing_loop of loop  (** A [for] or [while] around the statement. *)
  | Jump of label
      (** A [break], [continue] or [return] that can skip the statement. For a
          loop statement, a [break] or [return] in the loop body that can end
          the loop. *)

module LabelMap = struct
  include Int.Map

  let sexp_of_t f t = sexp_of_list (sexp_of_pair sexp_of_int f) (to_list t)
end

module ExprSet = Set.Make (Middle.Expr.Typed)
module ExprMap = Map.Make (Middle.Expr.Typed)
