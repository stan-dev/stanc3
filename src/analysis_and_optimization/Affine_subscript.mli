open Std
open Middle
open Dataflow_types

(** {1 Purpose}

    This module exists to describe one subscript, such as the [n + 1] in
    [v[n + 1]], in a form that shows how the selected element moves from one
    loop iteration to the next. The dependence analysis compares two subscripts
    in this form to learn whether the two can ever select the same element, and
    if so, how many iterations apart. The module knows nothing beyond a single
    subscript position: tuple fields, whole access paths and nested loops are
    the business of {!Dependence}.

    A subscript, such as the [n + 1] in [v[n + 1]], selects one element of a
    variable. An affine subscript is a sum of three parts, and a missing part
    counts as zero.
    - {e constant}: an integer such as the [1] in [n + 1].
    - {e symbol}: an expression that has the same value in every iteration of
      the loops. The expression can only be based on fixed values such as data
      [N].
    - {e loop term}: the variable of an enclosing [for] loop, added or
      subtracted, such as the [n] in [n + 1] or the [-n] in [N - n].

    With a {e loop term}, consecutive iterations select consecutive elements, up
    or down. Without a loop term, as in [v[k]] or [v[3]], the subscript selects
    the same element in every iteration. A subscript that is not a sum of these
    parts, such as [idx[n]] or [2 * n], is not affine and so we do not analyze
    it.

    {1 Comparing two subscripts}

    {!compare_subscripts} uses the two standard subscript tests of Goff, Kennedy
    and Tseng (1991).

    The ZIV test (zero index variables) compares two affine subscripts without a
    loop term. Equal subscripts always select the same element, and subscripts
    with different constants never do. The loop below has two pairs to compare.
    {@stan[
     1 for (n in 1:N) {
     2   v[1] = a;
     3   v[2] = b;
     4   w[n] = v[1];
     5 }
    ]}
    - The write [v[1]] on line 2 and the read [v[1]] on line 4 select the same
      element in every iteration. The result is {!Always_same}.
    - The write [v[2]] on line 3 and the read [v[1]] on line 4 never select the
      same element, and neither subscript has a loop term. The result is
      {!Never_same}.

    Each result allows a transformation of the loop. {!Never_same} shows that
    the write on line 3 does not touch the element that lines 2 and 4 use. A
    pass can therefore move that write before the loop.
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
    value. A pass can therefore move the write of [v[1]] before the loop as well
    and replace the loop with one fill of [w].
    {@stan[
     1 v[2] = b;
     2 v[1] = a;
     3 w = rep_vector(v[1], N);
    ]}

    The strong SIV test (single index variable) compares two affine subscripts
    with the same loop term and the same symbol, such as [n + 1] and [n - 1].
    These select the same element only when the two accesses run a fixed number
    of iterations apart. That number is the {e distance}: the iteration of the
    second subscript minus the iteration of the first. For [n + 1] against
    [n - 1], the distance is [2].
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

    What {!Dependence} does with a distance:
    - A positive distance keeps the pair as a {e loop-carried} dependence: the
      read in one iteration uses the value that the write left [distance]
      iterations earlier. In the loop above, [c[n]] depends on [a] through
      [b[n - 1]]. The distance also tells a later loop transformation which
      iterations must keep their order.
    - A distance of [0] means the same iteration only. The pair is a
      {e loop-independent} dependence, kept when the write comes before the read
      in the loop body. For kills in the monotone framework, a write to [b[n]]
      overwrites an earlier write to [b[n]] only within one iteration, so the
      earlier write still reaches the reads of other iterations.
    - A negative distance proves that the write runs after the read, so the pair
      is dropped ({!Dependence.is_lexicographically_positive}).

    Every other pair gives {!Cannot_tell}. The analysis then assumes that the
    two accesses may select the same element: the pair is kept as a possible
    dependence with an unknown distance, and the write kills nothing. *)

(** {1 API reference} *)

(** A loop variable in a subscript. [loop_id] is the label of the [for] loop,
    and [is_negated] is true for [-n], as in [N - n]. {!pp} prints a loop term
    first, as [-n] in [-n+N+1]. *)
type loop_term = {loop_id: label; is_negated: bool}

(** An affine subscript [loop_term + symbol + const]. These examples are in a
    loop over [n], and [N] and [k] are not defined in the analyzed statement.
    - [n + k - 1] has the loop term [n], the symbol [k] and the constant [-1].
    - [N - n + 1] has the loop term [-n], the symbol [N] and the constant [1].

    An affine subscript stores one symbol, and the symbol is always added. So
    [n - k] is not affine. In [k + j], the whole sum becomes the symbol. Symbols
    are compared as expressions, so [k + j] and [j + k] are different symbols.
*)
type affine =
  {const: int; symbol: Expr.Typed.t option; loop_term: loop_term option}

(** A classified subscript. [None] means that the subscript is not affine. *)
type t = affine option

val of_expr :
     for_loops:(string * label) list
  -> defined_vars:string Set.Poly.t
  -> Expr.Typed.t
  -> t
(** Returns the affine subscript of an expression, or [None]. [for_loops] pairs
    the loop variable and the label of each [for] loop around the statement, and
    [defined_vars] holds each variable that the analyzed statement writes or
    increments. Sums are read left to right, so [n + k + k] is not affine, but
    [k + k + n] has the symbol [k + k]. *)

(** The result of {!compare_subscripts}. *)
type comparison =
  | Always_same
      (** The subscripts select the same element whichever iterations the two
          accesses run in, because neither has a loop term, as [[1]] and [[1]],
          or [[N + 1]] and [[N + 1]], do. *)
  | Never_same
      (** The subscripts never select the same element, as [[1]] and [[2]] do.
      *)
  | Same_at_distance of {loop_id: label; distance: int}
      (** The subscript selects the same element within an interation. Ex: [[n]]
          and [[n]] give [0], so they select the same element only in the same
          iteration. [[n + 1]] and [[n]] give [1]. *)
  | Cannot_tell  (** Any other pair, as [[idx[n]]] and [[1]]. *)

val compare_subscripts : t Index.t -> t Index.t -> comparison
(** Compares the first subscript with the second, at the same position of two
    paths. A slice or a multi-index gives {!Cannot_tell}. *)

val pp : loop_name:(label -> string) -> t Fmt.t
(** Prints an affine subscript as [n], [n+1], [n+k-1], [-n+N+1], [3] or [k+1],
    and a subscript that is not affine as [?]. [loop_name] gives the loop
    variable of a loop label. *)
