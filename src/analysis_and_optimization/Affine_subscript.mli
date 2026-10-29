open Std
open Middle
open Dataflow_types

(** {1 Purpose}

    A subscript, such as the [n + 1] in [v[n + 1]], selects one element of a
    variable. To decide whether two accesses can select the same element,
    {!Dependence} compares their subscripts. The comparison works only on
    subscripts with a simple shape, called {e affine} subscripts, as in LLVM and
    in Allen and Kennedy.

    An affine subscript is a sum of three parts, and a missing part counts as
    zero.
    - A {e constant}, an integer such as the [1] in [n + 1].
    - A {e symbol}, an expression that has the same value in every iteration of
      the loops, such as [k] or [N]. A symbol reads no loop variable and no
      variable that the analyzed statement writes.
    - A {e loop term}, the variable of an enclosing [for] loop, added or
      subtracted, such as the [n] in [n + 1] or the [-n] in [N - n].

    With a loop term, consecutive iterations select consecutive elements, up or
    down. Without one, as in [v[k]] or [v[3]], the subscript selects the same
    element in every iteration. A subscript that is not a sum of these parts,
    such as [idx[n]] or [2 * n], is not affine in this module.

    {1 Comparing two subscripts}

    {!compare_subscripts} uses the two standard subscript tests of Goff, Kennedy
    and Tseng (1991).
    - The ZIV test (zero index variables) compares two affine subscripts without
      a loop term. Equal subscripts always select the same element, and
      subscripts with different constants never do.
    - The strong SIV test (single index variable) compares two affine subscripts
      with the same loop term and the same symbol, such as [n + 1] and [n - 1].
      These select the same element only when the two accesses run a fixed
      number of iterations apart. That number is the {e distance}: the iteration
      of the second subscript minus the iteration of the first. For [n + 1]
      against [n - 1], the distance is [2].

    Every other pair gives {!Cannot_tell}. *)

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
      (** The subscripts select the same element in every iteration, as [[1]]
          and [[1]], or [[k + 1]] and [[k + 1]], do. *)
  | Never_same
      (** The subscripts never select the same element, as [[1]] and [[2]] do.
      *)
  | Same_at_distance of {loop_id: label; distance: int}
      (** The subscripts select the same element only when the second runs
          [distance] iterations of the loop labeled [loop_id] after the first.
          [[n]] and [[n]] give [0], and [[n + 1]] and [[n]] give [1]. *)
  | Cannot_tell  (** Any other pair, as [[idx[n]]] and [[1]]. *)

val compare_subscripts : t Index.t -> t Index.t -> comparison
(** Compares the first subscript with the second, at the same position of two
    paths. A slice or a multi-index gives {!Cannot_tell}. *)

val pp : loop_name:(label -> string) -> t Fmt.t
(** Prints an affine subscript as [n], [n+1], [n+k-1], [-n+N+1], [3] or [k+1],
    and a subscript that is not affine as [?]. [loop_name] gives the loop
    variable of a loop label. *)
