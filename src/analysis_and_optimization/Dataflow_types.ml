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

(** A [for] loop. [id] is the label of the [for] statement, and [var] is the
    loop variable. *)
type for_loop = {id: label; var: string}

(** A [for] loop, or a [while] loop given by the label of the [while] statement.
    A loop is the only owner of a loop variable name. Other types hold the loop,
    not the name. *)
type loop = For of for_loop | While of label

(** The label of the loop statement. *)
let loop_id = function For {id; _} -> id | While id -> id

(** Two loops are the same loop when they have the same label. *)
let same_loop (left : loop) (right : loop) : bool = loop_id left = loop_id right

(** The branch of an [if] that holds a statement. *)
type branch = Then | Else

(** An [if] or a loop around a statement. *)
type enclosing =
  | If of {id: label; branch: branch}
      (** [id] is the label of the [if], and [branch] is the branch that holds
          the statement. *)
  | Loop of loop

(** The {e controls} of a statement are the other statements that decide
    whether, or how often, the statement runs. {!Dataflow_utils.build_cf_graphs}
    finds the controls of each statement. *)
type controls =
  { enclosing: enclosing list
        (** The ifs and loops around the statement, outermost first. *)
  ; jumps: label Set.Poly.t
        (** The labels of the [break], [continue] and [return] statements that
            can skip the statement. For a loop statement, also each [break] or
            [return] in the loop body that can end the loop. *) }

module LabelMap = struct
  include Int.Map

  let sexp_of_t f t = sexp_of_list (sexp_of_pair sexp_of_int f) (to_list t)
end

module ExprSet = Set.Make (Middle.Expr.Typed)
module ExprMap = Map.Make (Middle.Expr.Typed)
