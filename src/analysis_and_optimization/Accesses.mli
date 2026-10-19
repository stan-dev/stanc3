open Std
open Middle

(** {1 Purpose}

    This module records the accesses of each statement. An access is one read,
    write or increment of a variable. Each access has a path. The path lists the
    subscripts and tuple fields that select an element of the variable.

    {!Dependence_analysis} uses the paths in two ways.
    - Reaching definitions find the writes that can reach a read. They use
      {!must_overwrite} to remove a write when a later write replaces the same
      element.
    - {!Dependence} compares each such write with the read. It uses the paths to
      reject a write and a read that never select the same element, such as
      [theta[1]] and [theta[2]].

    {1 Running example}

    {!Dependence_analysis} analyzes one statement, such as the whole log
    density, with all statements nested in it. This documentation calls that
    statement the analyzed block.

    In this example, the analyzed block has a loop over [n], and the loop holds
    this assignment. The analyzed block does not write [k].
    {@stan[
      y[n] = v[n + 1] + v[k];
    ]}
    The output below prints each access as its kind, then the variable and the
    path. [R] is a read, [W] is a write and [+=] is an increment.

    The example has 3 steps.
    {ul
     {- Step 1 of 3: {!of_stmt} collects the accesses. The two reads of [n] come
        from the index of [y[n]] and the index of [v[n + 1]].
     }
     {- Step 2 of 3: {!classify} removes the reads of the loop variable [n]. }
     {- Step 3 of 3: {!classify} splits each index into a loop variable, a
        symbol and a constant. A symbol is an expression that has the same value
        in every iteration.
        {@text[
          Step 1 of 3, of_stmt:            R n, R v[n+1], R n, R v[k], R k, W y[n]
          Step 2 of 3, classify removes n: R v[n+1], R v[k], R k, W y[n]
          Step 3 of 3, classify splits each index:
            v[n+1]: loop variable n, no symbol, constant 1
            v[k]:   no loop variable, symbol k, constant 0
            k:      the whole variable (empty path)
            y[n]:   loop variable n, no symbol, constant 0
        ]}
        Step 2 of 3 loses no dependency. The statement has a control dependency
        on its loop, because the loop decides whether the statement runs. So a
        query for the dependencies of the statement reaches the loop and its
        bounds. No statement in the analyzed block writes [n], so a separate
        search for writes to [n] finds nothing. The read of [k] stays, because
        [k] decides which element [v[k]] selects.
     }
    }

    An index that splits into the three parts is [Sequential]. With a loop
    variable, consecutive iterations select consecutive elements, up or down.
    Without a loop variable, as in [v[k]] or [v[3]], the index selects the same
    element in every iteration. {!Dependence} compares two [Sequential] indices
    and computes the distance between them in iterations. Every other index is
    [Varying], such as [idx[n]] or [2 * n]. *)

(** {1 API reference} *)

(** The way an index moves when its loop variable increases. [n + 1] is
    [Ascending], and [N - n] is [Descending]. *)
type direction = Ascending | Descending [@@deriving equal]

(** The loop-variable part of an index. It stands for [loopvar] when [direction]
    is [Ascending], and for [-loopvar] when [direction] is [Descending]. *)
type loop_term = {loopvar: string; direction: direction} [@@deriving equal]

(** An index of the form [loop_term + symbol + const]. A missing part counts as
    zero. [symbol] reads no loop variable and no variable that the analyzed
    block writes. So [symbol] has the same value in every iteration.

    These examples are in a loop over [n]. [N] and [k] are not written.

    - [n + k - 1] has [loop_term = Some {loopvar = "n"; direction = Ascending}],
      [symbol = Some k] and [const = -1].
    - [N - n + 1] has
      [loop_term = Some {loopvar = "n"; direction = Descending}],
      [symbol = Some N] and [const = 1].

    The index stores one symbol, and the symbol is always added. So [n - k] is
    not [Sequential]. In [k + j], the whole sum becomes the symbol. Symbols are
    compared as expressions, so [k + j] and [j + k] are different symbols. *)
type sequential =
  {const: int; symbol: Expr.Typed.t option; loop_term: loop_term option}

(** The reason that an index is [Varying]. The reason is only for printing. *)
type varying_kind =
  | Written
      (** The index reads a variable that the analyzed block writes, so its
          value can change between iterations. For example, [v[m]] is [Written]
          when the block contains [m = n]. *)
  | Unhandled
      (** The index uses a loop variable in a form that the tests in
          {!Dependence} do not handle, such as [idx[n]], [2 * n] or [n + n].
          Sums are read left to right, so [n + k + k] is [(n + k) + k]. An index
          stores only one symbol. So after [n + k], the second [k] cannot be
          added, and the sum is [Unhandled]. In [k + k + n], [k + k] has no loop
          variable, so it becomes one symbol. So [k + k + n] is [Sequential]
          with the symbol [k + k]. *)

(** One classified subscript. *)
type index =
  | Sequential of sequential
      (** The index has the form that {!sequential} describes. *)
  | Varying of varying_kind
      (** The index has any other form. The tests in {!Dependence} cannot use
          it. *)

(** One position in a path. A step is a subscript or a tuple field. The path of
    [x[i].2[j]] has the three steps [i], [.2] and [j]. *)
type 'index step =
  | Subscript of 'index Index.t  (** One subscript, such as [n + 1]. *)
  | Field of int  (** One tuple field, such as [.2] in [t.2]. *)

(** Whether an access reads, writes or increments a variable.

    An [Increment] adds to [target], as [target += ...], [jacobian += ...] and
    [_lp] or [_jacobian] calls do. Two increments do not depend on each other,
    because the additions can happen in any order. *)
type kind = Read | Write | Increment [@@deriving equal]

(** One access by a statement. [var] is the variable, [kind] tells how the
    statement uses it, and [path] selects the element. An empty path means the
    whole variable, as in [sum(v)] or the declaration of [v]. *)
type 'index access = {var: string; path: 'index step list; kind: kind}

(** All the accesses of one statement. For an assignment, the list holds the
    reads in the left-hand indices, then the accesses in the right-hand side,
    then the write. *)
type 'index t = 'index access list

(** {2 Collecting and classifying} *)

val of_stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t -> Expr.Typed.t t
(** Collects the accesses in the own expressions of a statement. Nested
    statements are collected separately. The list order is the order that {!t}
    gives, not the run-time order.

    - An assignment gives the accesses in the left-hand indices and the
      right-hand side, then a write to the left-hand side.
    - A declaration gives the reads in the sizes and the initial value, then a
      write to the whole variable. [vector[N] y;] gives [R N, W y].
    - A loop, an [if] or a [while] gives the accesses in its bounds or its
      condition.
    - An addition to the log density gives an increment of [target]. [target()]
      gives a read of [target]. *)

val classify :
     loopvars:string Set.Poly.t
  -> written_vars:string Set.Poly.t
  -> Expr.Typed.t t
  -> index t
(** Prepares the accesses of a statement for the tests in {!Dependence}.
    [loopvars] holds the variables of the loops around the statement.
    [written_vars] holds each variable that the analyzed block writes,
    increments or declares. The function removes the reads of [loopvars]. Then
    it splits each index into the form that {!sequential} describes. If an index
    does not split, the variables that it reads decide the result.

    - If it reads a variable in [written_vars], it is [Varying Written].
    - Else, if it reads a variable in [loopvars], it is [Varying Unhandled].
    - Else, the whole expression becomes the symbol of a [Sequential] index.

    For example, with [loopvars = {n}] and [written_vars = {m}], [v[m]] gives
    [Varying Written], and [v[2 * n]] gives [Varying Unhandled]. *)

(** {2 Comparing paths}

    These functions compare two paths of one variable, one step at a time.

    Reaching definitions use {!must_overwrite}. A later write kills an earlier
    write when it overwrites each element that the earlier write wrote. Reaching
    definitions call the later write the killer and the earlier write the killed
    write. The killed write no longer reaches later reads.

    No code calls {!may_overlap} and {!fixed_path} yet. They are for planned
    element-aware warnings for uninitialized variables, and for planned value
    propagation. *)

(** The result of a comparison of two classified steps. *)
type step_comparison =
  | Same_every_iteration
      (** The steps always select the same element. Examples are [[1]] and
          [[1]], [[k + 1]] and [[k + 1]], and two equal tuple fields. *)
  | Same_in_one_iteration
      (** The steps select the same element in one loop iteration, as [[n]] and
          [[n]] do. In different iterations, [n] has different values. *)
  | Different
      (** The steps never select the same element, as [[1]] and [[2]], or [.1]
          and [.2]. *)
  | May_differ
      (** Any other pair, as [[n]] and [[n - 1]], or [[idx[n]]] and [[1]]. *)

val compare_steps : index step -> index step -> step_comparison
(** Compares two steps at the same position of two paths. *)

val must_overwrite :
     same_iteration:bool
  -> killer:index step list
  -> killed:index step list
  -> bool
(** Whether a write to [killer] overwrites each element that a write to [killed]
    wrote. [killer] can be shorter than [killed], so [m[1]] covers [m[1, 2]].
    Each step of [killer] must compare as [Same_every_iteration] with the step
    of [killed] at the same position. If [same_iteration] is true,
    [Same_in_one_iteration] also passes. So [m[n, 1]] overwrites [m[n, 1]] only
    when the caller knows that both writes run in the same iteration. *)

val may_overlap : index step list -> index step list -> bool
(** Whether a write to the first path can change an element that the second path
    selects. The result is false only when one position that both paths have
    compares as [Different]. For example, [m[2, 2]] does not overlap [m[1, 2]].
    [m[n, 1]] does not overlap [m[1, 2]], because the second subscripts differ.
    Column 1 and column 2 are always different elements. [m[n, 1]] can overlap
    [m[n - 1, 1]]. *)

(** One step of a path that has only constant indices and tuple fields. *)
type fixed_step = Fixed_index of int | Fixed_field of int
[@@deriving compare, sexp_of]

val fixed_path : index step list -> fixed_step list option
(** Converts a path to {!fixed_step} values. The result is [None] if a step is
    not a constant index or a tuple field. [t.2[3]] gives
    [Some [Fixed_field 2; Fixed_index 3]], and [m[k, 1]] gives [None]. *)

(** {2 Queries} *)

val of_var : string -> 'index t -> 'index t
(** Keeps the accesses to the named variable, in their original order. In the
    running example, ["v"] gives [R v[n+1], R v[k]]. *)

val read_vars : 'index t -> string Set.Poly.t
(** Returns the names of the variables that the accesses read or increment. In
    the running example, the result is [{"v"; "k"}]. *)

val written_vars : 'index t -> string Set.Poly.t
(** Returns the names of the variables that the accesses write or increment. In
    the running example, the result is [{"y"}]. *)

(** {2 Printers} *)

val pp_index : index Fmt.t
(** Prints a [Sequential] index as [n], [n+1], [n+k-1], [-n+N+1], [3] or [k+1].
    A [Descending] loop variable prints first, as [-n]. A [Varying] index prints
    as [?written] or [?unhandled]. *)

val pp_access : 'index Fmt.t -> 'index access Fmt.t
(** Prints the kind, the variable and the path, as in [W v[n+1]], [R k],
    [+= target] or [W t.2]. Consecutive subscripts share one pair of brackets,
    as in [[i, j]]. A slice prints as [[1:k]], a multi-index as [[{idx}]], and a
    tuple field as [.2]. *)

val pp : 'index Fmt.t -> 'index t Fmt.t
(** Prints the accesses grouped by kind, with reads first, then increments, then
    writes. Each group keeps the original order. For example,
    [R v[n+1], R k, += target, W y[n]]. *)
