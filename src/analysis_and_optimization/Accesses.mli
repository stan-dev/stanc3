open Std
open Middle

(** {1 Purpose}

    This module records the accesses of each statement. An {e access} is one
    read, write or increment of a variable. Each access has a {e path}. The path
    lists the subscripts and tuple fields that select an element of the
    variable.

    {!Dependence_analysis} finds the earlier statements that each statement
    {e depends} on. A statement has a {e data dependency} on an earlier write
    when the write can supply a value that the statement reads. To find these,
    the analysis compares the paths of a write and a read of one variable with
    {!Dependence}. Two accesses whose paths never select the same element, such
    as [theta[1]] and [theta[2]], do not depend on each other.

    {1 Running example}

    {!Dependence_analysis} analyzes one statement, such as the whole log
    density, together with the statements nested inside. This documentation
    calls that statement the {e analyzed statement}.

    In this example, the analyzed statement has a loop over [n], and the loop
    holds this assignment. The analyzed statement does not write [k].
    {@stan[
      y[n] = v[n + 1] + v[k];
    ]}
    {!of_stmt} gives these accesses:
    {@text[
      R n, R v[n+1], R n, R v[k], R k, W y[n]
    ]}
    Each access prints as the kind of access, then the variable and the path.
    [R] is a read, [W] is a write and [+=] is an increment. Consecutive
    subscripts share one pair of brackets, as in [[i, j]]. A slice prints as
    [[1:k]], a multi-index as [[{idx}]], and a tuple field as [.2]. Each
    subscript prints in the notation of {!Index_form.pp}, so [n+1] is the loop
    variable [n] plus the constant [1]. The two reads of [n] come from the
    subscripts of [y[n]] and [v[n + 1]]. *)

(** {1 API reference} *)

(** One position in a path. A step is a subscript or a tuple field. The path of
    [x[i].2[j]] has the three steps [i], [.2] and [j]. *)
type step =
  | Subscript of Index_form.index Index.t
      (** One subscript, such as [n + 1], with the form of each index. *)
  | Field of int  (** One tuple field, such as [.2] in [t.2]. *)

(** Whether an access reads, writes or increments a variable.

    An [Increment] adds to [target], as [target += ...], [jacobian += ...] and
    [_lp] or [_jacobian] calls do. Two increments do not depend on each other,
    because the additions can happen in any order. *)
type kind = Read | Write | Increment [@@deriving equal]

(** One access by a statement. [var] is the variable, [kind] tells how the
    statement uses the variable, and [path] selects the element. An empty path
    means the whole variable, as in [sum(v)] or the declaration of [v]. *)
type access = {var: string; path: step list; kind: kind}

val of_stmt :
     classify:(Expr.Typed.t -> Index_form.index)
  -> (Expr.Typed.t, 'substatement) Stmt.Pattern.t
  -> access list
(** Collects the accesses in the expressions of a statement, and gives each
    subscript the form that [classify] returns. Nested statements are collected
    separately. The accesses come in this order, which is not the run-time
    order.
    - An assignment gives the reads in the left-hand indices, then the accesses
      in the right-hand side, then a write to the left-hand side.
    - A declaration gives the reads in the sizes and the initial value, then a
      write to the whole variable. [vector[N] y;] gives [R N, W y].
    - A [for] loop gives the accesses in the loop bounds. An [if] or a [while]
      gives the accesses in the condition.
    - An addition to the log density gives an increment of [target]. [target()]
      gives a read of [target]. *)

val of_var : string -> access list -> access list
(** Keeps the accesses to the named variable, in their original order. In the
    running example, ["v"] gives [R v[n+1], R v[k]]. *)

val read_vars : access list -> string Set.Poly.t
(** Returns the names of the variables that the accesses read or increment. In
    the running example, the result is [{"n"; "v"; "k"}]. *)

val written_vars : access list -> string Set.Poly.t
(** Returns the names of the variables that the accesses write or increment. In
    the running example, the result is [{"y"}]. *)

val pp_access : loop_name:(Dataflow_types.label -> string) -> access Fmt.t
(** Prints the kind, the variable and the path in the notation of the running
    example, as in [W v[n+1]], [R k], [+= target] or [W t.2]. [loop_name] gives
    the loop variable of a loop label. *)

val pp : loop_name:(Dataflow_types.label -> string) -> access list Fmt.t
(** Prints the reads, then the increments, then the writes, each group in the
    original order, as in [R v[n+1], R k, += target, W y[n]]. *)
