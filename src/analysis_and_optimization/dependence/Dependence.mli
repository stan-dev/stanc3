open Std
open Middle
open Dataflow_types

(** {1 Purpose}

    This module describes the accesses of one statement and compares two
    accesses of one variable. The three parts build on each other, in the order
    below.
    - {!Affine_subscript} describes one subscript, such as the [n + 1] in
      [v[n + 1]].
    - {!Accesses} collects the reads, writes and read-writes of one statement,
      each with a path of subscripts.
    - The functions after the two submodules compare two paths of one variable
      across the loops around both and give a distance per loop.

    {!Dependence_analysis} collects these accesses for each statement of the
    analyzed code and answers the dependence queries. *)

module Affine_subscript : sig
  (** {1 Purpose}

      This module exists to describe one subscript, such as the [n + 1] in
      [v[n + 1]], in a form that shows how the selected element moves from one
      loop iteration to the next. The dependence analysis compares two
      subscripts in this form to learn whether the two can ever select the same
      element, and if so, how many iterations apart. The module knows nothing
      beyond a single subscript position: tuple fields, whole access paths and
      nested loops are the business of the comparison functions of
      {!Dependence}.

      A subscript, such as the [n + 1] in [v[n + 1]], selects one element of a
      variable. An affine subscript is a sum of three parts, and a missing part
      counts as zero.
      - {e constant}: every integer literal in the sum, wherever the literal
        appears, added up, such as the [1] in [n + 1].
      - {e symbol}: the terms of the sum that do not change inside the loops,
        such as [N], [k + k] or [-k], kept as one expression. These terms can
        only be based on fixed values such as data [N].
      - {e loop term}: the variable of an enclosing [for] loop, added or
        subtracted, such as the [n] in [n + 1] or the [-n] in [N - n].

      With a {e loop term}, consecutive iterations select consecutive elements,
      up or down. Without a loop term, as in [v[k]] or [v[3]], the subscript
      selects the same element in every iteration. A subscript that is not a sum
      of these parts, such as [idx[n]] or [2 * n], is not affine and so we do
      not analyze it.

      {1 Comparing two subscripts}

      {!compare_subscripts} uses the two standard subscript tests of Goff,
      Kennedy and Tseng (1991).

      The ZIV test (zero index variables) compares two affine subscripts without
      a loop term. Equal subscripts always select the same element, and
      subscripts with different constants never do. The loop below has two pairs
      to compare.
      {@stan[
       1 for (n in 1:N) {
       2   v[1] = a;
       3   v[2] = b;
       4   w[n] = v[1];
       5 }
      ]}
      - The write [v[1]] on line 2 and the read [v[1]] on line 4 select the same
        element in every iteration. The result is {!Always_same}.
      - The write [v[2]] on line 3 and the read [v[1]] on line 4 never select
        the same element, and neither subscript has a loop term. The result is
        {!Never_same}.

      Each result allows a transformation of the loop. The write on line 3 does
      not touch the element that lines 2 and 4 use and so their comparisons will
      be {!Never_same}. A pass can therefore move that write before the loop.
      {@stan[
       1 v[2] = b;
       2 for (n in 1:N) {
       3   v[1] = a;
       4   w[n] = v[1];
       5 }
      ]}
      {!Always_same} shows that the write of [v[1]] fills the same element in
      every iteration and that the read of [v[1]] takes the same element back.
      Since [a] does not change in the loop, every iteration stores the same
      value. A pass can therefore move the write of [v[1]] before the loop as
      well and replace the loop with one fill of [w].
      {@stan[
       1 v[2] = b;
       2 v[1] = a;
       3 w = rep_vector(v[1], N);
      ]}

      The strong SIV test (single index variable) compares two affine subscripts
      with the same loop term and the same symbol, such as [n + 1] and [n - 1].
      These select the same element only when the two accesses run a fixed
      number of iterations apart. That number is the {e distance}: the iteration
      of the second subscript minus the iteration of the first. For [n + 1]
      against [n - 1], the distance is [2].
      {@stan[
        for (n in 2:N) {
          b[n] = a;
          c[n] = b[n - 1];
        }
      ]}
      Iteration [n] reads the element that iteration [n - 1] wrote. The write
      [b[n]] and the read [b[n - 1]] select the same element when the read runs
      one iteration after the write, so the distance is [1], which is
      {!Same_at_distance}.

      What the comparison functions of {!Dependence} do with a distance:
      - A positive distance keeps the pair as a {e loop-carried} dependence: the
        read in one iteration uses the value that the write left [distance]
        iterations earlier. In the loop above, [c[n]] depends on [a] through
        [b[n - 1]]. The distance also tells a later loop transformation which
        iterations must keep their order.
      - A distance of [0] means the same iteration only. The pair is a
        {e loop-independent} dependence, kept when the write comes before the
        read in the loop body. For kills in the monotone framework, a write to
        [b[n]] overwrites an earlier write to [b[n]] only within one iteration,
        so the earlier write still reaches the reads of other iterations.
      - A negative distance proves that the write runs after the read, so the
        pair is dropped ({!Dependence.is_lexicographically_positive}).

      Every other pair gives {!Cannot_tell}. The analysis then assumes that the
      two accesses may select the same element: the pair is kept as a possible
      dependence with an unknown distance, and the write kills nothing. *)

  (** {1 API reference} *)

  (** A loop variable in a subscript. [loop_id] is the label of the [for] loop,
      and [is_negated] is true for [-n], as in [N - n]. *)
  type loop_term = {loop_id: label; is_negated: bool}

  (** An affine subscript [loop_term + symbol + const]. These examples are in a
      loop over [n], and [N] and [k] are not defined in the analyzed statement.
      - [n + k - 1] has the loop term [n], the symbol [k] and the constant [-1].
      - [N - n + 1] has the loop term [-n], the symbol [N] and the constant [1].
      - [(N - 1) - n] has the loop term [-n], the symbol [N] and the constant
        [-1].

      The invariant terms of a sum join into one symbol, so [n + k + j] has the
      symbol [k + j] and [n - k] has the symbol [-k]. Symbols are compared as
      expressions, so [k + j] and [j + k] are different symbols. *)
  type affine =
    {const: int; symbol: Expr.Typed.t option; loop_term: loop_term option}

  (** A classified subscript. [None] means that the subscript is not affine. *)
  type t = affine option

  val of_expr :
       for_loops:(string * label) list
    -> defined_vars:string Set.Poly.t
    -> Expr.Typed.t
    -> t
  (** Returns the affine subscript of an expression, or [None]. [for_loops]
      pairs the loop variable and the label of each [for] loop around the
      statement, and [defined_vars] holds each variable that the analyzed
      statement writes. The grouping of a sum does not matter: [n + k + k],
      [k + k + n] and [n + (k + k)] all have the loop term [n] and the symbol
      [k + k]. *)

  (** The result of {!compare_subscripts}. *)
  type comparison =
    | Always_same
        (** The subscripts select the same element whichever iterations the two
            accesses run in, because neither has a loop term, as [[1]] and
            [[1]], or [[N + 1]] and [[N + 1]], do. *)
    | Never_same
        (** The subscripts never select the same element, as [[1]] and [[2]] do.
        *)
    | Same_at_distance of {loop_id: label; distance: int}
        (** The subscript selects the same element within an interation. Ex:
            [[n]] and [[n]] give [0], so they select the same element only in
            the same iteration. [[n + 1]] and [[n]] give [1]. *)
    | Cannot_tell  (** Any other pair, as [[idx[n]]] and [[1]]. *)

  val compare_subscripts : t Index.t -> t Index.t -> comparison
  (** Compares the first subscript with the second, at the same position of two
      paths. A slice or a multi-index gives {!Cannot_tell}. *)

  val pp : loop_name:(label -> string) -> t Fmt.t
  (** Prints an affine subscript as [n], [n+1], [n+k-1], [-n+N+1], [3] or [k+1],
      and a subscript that is not affine as [?]. [loop_name] gives the loop
      variable of a loop label. *)
end

module Accesses : sig
  (** {1 Purpose}

      This module records the accesses of each statement. An {e access} is one
      read, write or read-write of a variable. Each access has a {e path}. The
      path lists the subscripts and tuple fields that select an element of the
      variable.

      {!Dependence_analysis} finds the earlier statements that each statement
      {e depends} on. A statement has a {e data dependence} on an earlier write
      when the write can supply a value that the statement reads. To find these,
      the analysis compares the paths of a write and a read of one variable with
      the comparison functions of {!Dependence}. Two accesses whose paths never
      select the same element, such as [theta[1]] and [theta[2]], do not depend
      on each other.

      {1 Example}

      {!Dependence_analysis} analyzes one statement, such as the whole log
      density, together with the statements nested inside. This documentation
      calls that statement the {e analyzed statement}.

      In this example, the analyzed statement has a loop over [n], and the loop
      holds this assignment. The analyzed statement does not write [k].
      {@stan[
        y[n] = v[n + 1] + v[k];
      ]}
      {!of_statement} gives these accesses:
      {@text[
        R n, R v[n+1], R n, R v[k], R k, W y[n]
      ]}
      Each access prints as the kind of access, then the variable and the path.
      [R] is a read, [W] is a write and [RW] is a read-write. Consecutive
      subscripts share one pair of brackets, as in [[i, j]]. A slice prints as
      [[1:k]], a multi-index as [[{idx}]], and a tuple field as [.2]. Each
      subscript prints in the notation of {!Affine_subscript.pp}, so [n+1] is
      the loop variable [n] plus the constant [1]. The two reads of [n] come
      from the subscripts of [y[n]] and [v[n + 1]]. *)

  (** {1 API reference} *)

  (** One position in a path. A step is a subscript or a tuple field. The path
      of [x[i].2[j]] has the three steps [i], [.2] and [j]. ['e] is the type of
      a subscript: an expression while the path is collected, and an
      {!Affine_subscript.t} once classified. *)
  type 'e step =
    | Subscript of 'e Index.t
        (** One subscript, such as [n + 1], with the affine subscript of each
            index, or [None]. *)
    | Field of int  (** One tuple field, such as [.2] in [t.2]. *)

  (** A statement reads a variable, writes a variable, or reads and writes the
      same element of a variable, as [x += 1] and [target += 1] do. A read-write
      is one definition and one use. *)
  type kind = Read | Write | ReadWrite [@@deriving equal]

  (** One access by a statement. [var] is the variable, [kind] tells how the
      statement uses the variable, and [path] selects the element. An empty path
      means the whole variable, as in [sum(v)] or the declaration of [v]. *)
  type access = {var: string; path: Affine_subscript.t step list; kind: kind}

  (** The accesses of one statement. *)
  type t = access list

  val of_statement :
       classify:(Expr.Typed.t -> Affine_subscript.t)
    -> (Expr.Typed.t, 'substatement) Stmt.Pattern.t
    -> t
  (** Collects the accesses in the expressions of a statement, and gives each
      subscript the result of [classify]. Nested statements are collected
      separately. The accesses come in this order, which is not the run-time
      order.
      - An assignment gives the reads in the left-hand indices, then the
        accesses in the right-hand side, then a write to the left-hand side.
        When the right-hand side reads the written element, as [x = x + 1] does,
        that read and the write are one read-write, [RW x].
      - A declaration gives the reads in the sizes and the initial value, then a
        write to the whole variable. [vector[N] y;] gives [R N, W y].
      - A [for] loop gives the accesses in the loop bounds. An [if] or a [while]
        gives the accesses in the condition.
      - [target += e], [jacobian += e], a [~] statement and a call of an [_lp]
        or [_jacobian] function give a read-write of [target]. [target()] gives
        a read of [target]. *)

  val used_vars : t -> string Set.Poly.t
  (** Returns the names of the variables that the accesses use, which means read
      or read-write. In the example above, the result is [{"n"; "v"; "k"}]. *)

  val defined_vars : t -> string Set.Poly.t
  (** Returns the names of the variables that the accesses define, which means
      write or read-write. In the example above, the result is [{"y"}]. *)

  val pp_path :
    loop_name:(label -> string) -> Affine_subscript.t step list Fmt.t
  (** Prints a path, as in [[n+1]], [[n, 1]] or [.2]. An empty path prints
      nothing. *)

  val pp_access : loop_name:(label -> string) -> access Fmt.t
  (** Prints the kind, the variable and the path in the notation of the example
      above, as in [W v[n+1]], [R k], [RW target] or [W t.2]. [loop_name] gives
      the loop variable of a loop label. *)

  val pp : loop_name:(label -> string) -> t Fmt.t
  (** Prints the reads, then the read-writes, then the writes, each group in the
      original order, as in [R v[n+1], R k, RW target, W y[n]]. *)
end

(** {1 Comparing two accesses}

    The functions below are a layer on top of {!Affine_subscript} that exists to
    compare two whole accesses of one variable, such as a write to [m[n, 1]] and
    a read of [m[n - 1, 1]], across every loop around both. The per-position
    answers of {!Affine_subscript} combine into one distance per common loop,
    which tells the dependence analysis:
    - whether a write to one element of a variable can supply the value that a
      later read of that variable uses, and whether a later write replaces the
      earlier one
    - how many loop iterations apart the write and the read can run, and so
      whether the write can run first.

    Knowing these dependences per element lets the compiler warn about a
    parameter only when the parameter really reaches a statement, and tells a
    loop transformation which statements must keep their order.

    {1 Example}
    {@stan[
      for (i in 1:I) {
        for (j in 2:J) {
          a[i, j] = b;
          c[i, j] = a[i, j - 1];
        }
      }
    ]}
    {!Affine_subscript} answers for one position at a time. At the first
    position, [[i]] against [[i]] selects the same element only in the same
    iteration of [i], a distance of [0]. At the second position, [[j]] against
    [[j - 1]] selects the same element when the read runs one iteration of [j]
    after the write, a distance of [1]. Neither answer alone says whether the
    write reaches the read.

    {!test_dependence} combines the two answers into one distance per loop,
    [distances [i:0][j:1]], and reads the vector from the outer loop inward. The
    first distance that is not zero is positive, so the write runs before the
    read, and the pair is a loop-carried dependence at [j]. The combination can
    also reject a pair that every position allows. For [a[i, i]] against
    [a[i - 1, i]], the first position needs a distance of [1] at [i] and the
    second needs [0], so the two never select the same element and the result is
    [independent].

    These tests are a small subset of the tests in Goff, Kennedy and Tseng,
    {{:https://doi.org/10.1145/113445.113448} "Practical Dependence Testing"}
    (PLDI 1991). Allen and Kennedy,
    {{:https://shop.elsevier.com/books/optimizing-compilers-for-modern-architectures/allen/978-0-08-051324-9}
     "Optimizing Compilers for Modern Architectures"} (2001, chapters 2 and 3),
    describe the same tests, and LLVM's
    {{:https://github.com/llvm/llvm-project/blob/main/llvm/lib/Analysis/DependenceAnalysis.cpp}
     [DependenceAnalysis]} implements them. LLVM calls the writer [Src] and the
    reader [Dst]. A vector with no known distance is what LLVM calls a confused
    dependence, and GCC [chrec_dont_know]. The [is_writer_first] flag of
    {!is_lexicographically_positive} has the role of the
    [PossiblyLoopIndependent] flag of LLVM's
    {{:https://github.com/llvm/llvm-project/blob/main/llvm/include/llvm/Analysis/DependenceAnalysis.h}
     [FullDependence]}. *)

(** {1 API reference} *)

val is_overwritten :
     is_same_iteration:bool
  -> Affine_subscript.t Accesses.step list
  -> Affine_subscript.t Accesses.step list
  -> bool
(** Whether a later write to the first path (the killer) overwrites each element
    that an earlier write to the second path (the killed write) wrote.
    [is_same_iteration] tells whether both writes run in the same loop
    iteration. *)

(** The result of {!test_dependence}. *)
type t =
  | Independent  (** The accesses never select the same element. *)
  | Distance_vector of int option list
      (** The accesses may select the same element. The list holds one distance
          per common loop, outermost first. [None] at a loop means that the
          distance at that loop is unknown, as at [i] for [a[n]] inside
          [for (i) for (n)]: [distances [i:*][n:0]]. *)

val test_dependence :
     label list
  -> Affine_subscript.t Accesses.step list
  -> Affine_subscript.t Accesses.step list
  -> t
(** Whether the first path (the write) and the second path (the read) of one
    variable can select the same element, and if so, at which distance in each
    loop around both. The label list holds the labels of those loops, outermost
    first. *)

val is_lexicographically_positive :
     label list
  -> bool
  -> Affine_subscript.t Accesses.step list
  -> Affine_subscript.t Accesses.step list
  -> bool
(** Whether the write to the first path can run before the read of the second
    path and select the same element. The arguments are those of
    {!test_dependence}, with the flag in between that says whether the writer
    comes before the reader in the loop body.

    For [b[n] = a; c[n] = b[n - 1];] the distance is [1]: the read runs one
    iteration after the write, so the result is true. For a read of [b[n + 1]]
    the distance is [-1]: the element is written one iteration later, so the
    result is false. When every distance is [0], the two run in the same
    iteration and the flag decides. The first distance that is not zero counts,
    outermost loop first, and an unknown distance counts as positive.
    [Independent] gives false. *)

val pp : loop_name:(label -> string) -> common_loop_ids:label list -> t Fmt.t
(** Prints [independent], [unknown] when no distance is known, or [distances]
    with one [[loop:distance]] per common loop in [common_loop_ids], as in
    [distances [n:1][m:*]]. [loop_name] gives the loop variable of a loop label,
    or [while] for a [while] loop. *)
