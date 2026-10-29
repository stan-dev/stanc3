open Std
open Middle

(** {1 Purpose}

    This module records the accesses of each statement. An {e access} is one
    read, write or increment of a variable. Each access has a {e path}. The path
    lists the subscripts and tuple fields that select an element of the
    variable.

    {!Dependence_analysis} finds the earlier statements that each statement
    {e depends} on. A statement has a {e data dependency} on an earlier write
    when the write can supply a value that the statement reads. A statement has
    a {e control dependency} on an earlier statement that decides whether the
    statement runs, such as an enclosing loop.

    A write {e reaches} a read when the program can go from the write to the
    read, and no write between them replaces the same element. The standard
    analysis that finds these writes is called {e reaching definitions}.
    {!Dependence_analysis} uses the paths in two ways.
    - Reaching definitions use {!must_overwrite} to remove a write when a later
      write replaces the same element.
    - {!Dependence} compares each write that reaches a read with the read.
      {!Dependence} uses the paths to reject a write and a read that never
      select the same element, such as [theta[1]] and [theta[2]].

    {1 Running example}

    {!Dependence_analysis} analyzes one statement, such as the whole log
    density, together with the statements nested inside. This documentation
    calls that statement the {e analyzed block}.

    In this example, the analyzed block has a loop over [n], and the loop holds
    this assignment. The analyzed block does not write [k].
    {@stan[
      y[n] = v[n + 1] + v[k];
    ]}
    The output below prints each access as the kind of access, then the variable
    and the path. [R] is a read, [W] is a write and [+=] is an increment.
    Consecutive subscripts share one pair of brackets, as in [[i, j]]. A slice
    prints as [[1:k]], a multi-index as [[{idx}]], and a tuple field as [.2].

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
        Step 2 of 3 does not lose a dependency. The statement has a control
        dependency on the loop over [n], because the loop decides whether the
        statement runs. So a query for the dependencies of the statement reaches
        the loop and the loop bounds. No statement in the analyzed block writes
        [n], so a separate search for writes to [n] finds nothing. The read of
        [k] stays, because [k] decides which element [v[k]] selects.
     }
    }

    An index that splits into the three parts is [Sequential]. With a loop
    variable, consecutive iterations select consecutive elements, up or down.
    Without a loop variable, as in [v[k]] or [v[3]], the index selects the same
    element in every iteration. {!Dependence} compares two [Sequential] indices
    and computes the {e distance} between them. The distance is the number of
    iterations from one access of an element to the other access of the same
    element. Every other index is [Varying], such as [idx[n]] or [2 * n]. *)

(** {1 API reference} *)

(** The way an index moves when the loop variable increases. [n + 1] is
    [Ascending], and [N - n] is [Descending]. *)
type direction = Ascending | Descending [@@deriving equal]

(** The loop-variable part of an index. The part stands for [loopvar] when
    [direction] is [Ascending], and for [-loopvar] when [direction] is
    [Descending]. {!pp_index} prints this part first, as [-n] in [-n+N+1]. *)
type loop_term = {loopvar: string; direction: direction} [@@deriving equal]

(** An index of the form [loop_term + symbol + const]. A missing part counts as
    zero. [symbol] does not read a loop variable or a variable that the analyzed
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
      (** The index reads a variable that the analyzed block writes, so the
          index value can change between iterations. For example, [v[m]] is
          [Written] when the block contains [m = n]. *)
  | Unhandled
      (** The index uses a loop variable in a form that the tests in
          {!Dependence} do not handle, such as [idx[n]], [2 * n] or [n + n].
          Sums are read left to right, so [n + k + k] is [(n + k) + k]. An index
          stores only one symbol. So after [n + k], the second [k] cannot be
          added, and the sum is [Unhandled]. In [k + k + n], [k + k] has no loop
          variable, so [k + k] becomes one symbol. So [k + k + n] is
          [Sequential] with the symbol [k + k]. *)

(** One classified subscript. {!classify} first tries to split the subscript
    into the form that {!sequential} describes. If the subscript does not split,
    the variables that the subscript reads decide the result.

    - If the subscript reads a variable that the analyzed block writes, the
      result is [Varying Written].
    - Else, if the subscript reads a loop variable, the result is
      [Varying Unhandled].
    - Else, the whole expression becomes the symbol of a [Sequential] index. *)
type index =
  | Sequential of sequential
      (** The index has the form that {!sequential} describes. *)
  | Varying of varying_kind
      (** The index has any other form. The tests in {!Dependence} cannot use a
          [Varying] index. *)

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
    statement uses the variable, and [path] selects the element. An empty path
    means the whole variable, as in [sum(v)] or the declaration of [v]. *)
type 'index access = {var: string; path: 'index step list; kind: kind}

(** All the accesses of one statement, in the order below. This order is not the
    run-time order. The accesses of nested statements are not included.

    - An assignment gives the reads in the left-hand indices, then the accesses
      in the right-hand side, then a write to the left-hand side.
    - A declaration gives the reads in the sizes and the initial value, then a
      write to the whole variable. [vector[N] y;] gives [R N, W y].
    - A [for] loop gives the accesses in the loop bounds. An [if] or a [while]
      gives the accesses in the condition.
    - An addition to the log density gives an increment of [target]. [target()]
      gives a read of [target]. *)
type 'index t = 'index access list

(** {2 Collecting and classifying}

    {!classify} takes two sets of variable names. [loopvars] holds the variables
    of the loops around the statement. [written_vars] holds each variable that
    the analyzed block writes, increments or declares. *)

val of_stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t -> Expr.Typed.t t
(** Collects the accesses in the expressions of a statement, in the order that
    {!t} describes. Nested statements are collected separately. *)

val classify :
     loopvars:string Set.Poly.t
  -> written_vars:string Set.Poly.t
  -> Expr.Typed.t t
  -> index t
(** Removes the reads of [loopvars] and classifies each index as {!index}
    describes, using [written_vars], for the tests in {!Dependence}. *)

(** {2 Comparing paths}

    These functions compare two paths of one variable, one step at a time.

    Reaching definitions use {!must_overwrite}. A later write {e kills} an
    earlier write when the later write overwrites each element that the earlier
    write wrote. Reaching definitions call the later write the killer and the
    earlier write the killed write. The killed write no longer reaches later
    reads. The path of the killer can be shorter than the path of the killed
    write, so [m[1]] covers [m[1, 2]]. At each position of the killer path, the
    two steps must select the same element in every iteration, as [[1]] and
    [[1]] do. Steps that select the same element only within one iteration, as
    [[n]] and [[n]] do, pass only when both writes run in the same iteration. So
    [m[n, 1]] overwrites [m[n, 1]] only when the caller knows that both writes
    run in the same iteration.

    Two paths {e may overlap} unless some position that both paths have holds
    two steps that never select the same element. For example, [m[2, 2]] does
    not overlap [m[1, 2]]. [m[n, 1]] does not overlap [m[1, 2]], because the
    second subscripts differ. Column 1 and column 2 are always different
    elements. [m[n, 1]] can overlap [m[n - 1, 1]].

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
(** Whether a write to [killer] kills a write to [killed]. [same_iteration]
    tells whether both writes run in the same iteration. *)

val may_overlap : index step list -> index step list -> bool
(** Whether a write to the first path can change an element that the second path
    selects. Only a shared position that is [Different] gives false. *)

(** One step of a path that has only constant indices and tuple fields.
    {!fixed_path} turns [t.2[3]] into [Some [Fixed_field 2; Fixed_index 3]], and
    [m[k, 1]] into [None]. *)
type fixed_step = Fixed_index of int | Fixed_field of int
[@@deriving compare, sexp_of]

val fixed_path : index step list -> fixed_step list option
(** Converts a path to {!fixed_step} values. The result is [None] if a step is
    not a constant index or a tuple field. *)

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
(** Prints a [Sequential] index as [n], [n+1], [n+k-1], [-n+N+1], [3] or [k+1],
    and a [Varying] index as [?written] or [?unhandled]. *)

val pp_access : 'index Fmt.t -> 'index access Fmt.t
(** Prints the kind, the variable and the path in the notation of the running
    example, as in [W v[n+1]], [R k], [+= target] or [W t.2]. *)

val pp : 'index Fmt.t -> 'index t Fmt.t
(** Prints the reads, then the increments, then the writes, each group in the
    original order, as in [R v[n+1], R k, += target, W y[n]]. *)
