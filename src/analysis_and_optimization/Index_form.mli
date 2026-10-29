open Std
open Middle
open Dataflow_types

(** {1 Purpose}

    A subscript, such as the [n + 1] in [v[n + 1]], selects one element of a
    variable. To decide whether two accesses can select the same element,
    {!Dependence} compares their subscripts. The comparison works only on
    subscripts with a simple shape, which this module calls a {e form}.

    A form is a sum of three parts, and a missing part counts as zero.
    - A {e constant}, an integer such as the [1] in [n + 1].
    - A {e symbol}, an expression that has the same value in every iteration of
      the loops, such as [k] or [N]. A symbol reads no loop variable and no
      variable that the analyzed statement writes.
    - A {e loop term}, the variable of an enclosing [for] loop, added or
      subtracted, such as the [n] in [n + 1] or the [-n] in [N - n].

    With a loop term, consecutive iterations select consecutive elements, up or
    down. Without one, as in [v[k]] or [v[3]], the subscript selects the same
    element in every iteration. A subscript that is not a sum of these parts,
    such as [idx[n]] or [2 * n], has no form.

    {1 Comparing two subscripts}

    {!compare_subscripts} uses the two standard subscript tests of Goff, Kennedy
    and Tseng (1991).
    - The ZIV test (zero index variables) compares two forms without a loop
      term. Equal forms always select the same element, and forms with different
      constants never do.
    - The strong SIV test (single index variable) compares two forms with the
      same loop term and the same symbol, such as [n + 1] and [n - 1]. These
      select the same element only when the two accesses run a fixed number of
      iterations apart. That number is the {e distance}: the iteration of the
      second subscript minus the iteration of the first. For [n + 1] against
      [n - 1], the distance is [2].

    Every other pair gives {!Cannot_tell}. *)

(** {1 API reference} *)

(** A loop variable in a subscript. [loop] is the label of the [for] loop, and
    [negated] is true for [-n], as in [N - n]. {!pp} prints a loop term first,
    as [-n] in [-n+N+1]. *)
type loop_term = {loop: label; negated: bool}

(** A subscript of the form [loop_term + symbol + const]. These examples are in
    a loop over [n], and [N] and [k] are not written.
    - [n + k - 1] has the loop term [n], the symbol [k] and the constant [-1].
    - [N - n + 1] has the loop term [-n], the symbol [N] and the constant [1].

    A form stores one symbol, and the symbol is always added. So [n - k] has no
    form. In [k + j], the whole sum becomes the symbol. Symbols are compared as
    expressions, so [k + j] and [j + k] are different symbols. *)
type form =
  {const: int; symbol: Expr.Typed.t option; loop_term: loop_term option}

(** A classified subscript. [None] means that the subscript has no form. *)
type index = form option

val classify :
     for_loops:(string * label) list
  -> written_vars:string Set.Poly.t
  -> Expr.Typed.t
  -> index
(** Returns the form of a subscript. [for_loops] pairs the loop variable and the
    label of each [for] loop around the statement, and [written_vars] holds each
    variable that the analyzed statement writes. Sums are read left to right, so
    [n + k + k] has no form, but [k + k + n] has the symbol [k + k]. *)

(** The result of {!compare_subscripts}. *)
type comparison =
  | Always_same
      (** The subscripts select the same element in every iteration, as [[1]]
          and [[1]], or [[k + 1]] and [[k + 1]], do. *)
  | Never_same
      (** The subscripts never select the same element, as [[1]] and [[2]] do.
      *)
  | Same_at_distance of {loop: label; distance: int}
      (** The subscripts select the same element only when the second runs
          [distance] iterations of the loop labeled [loop] after the first.
          [[n]] and [[n]] give [0], and [[n + 1]] and [[n]] give [1]. *)
  | Cannot_tell  (** Any other pair, as [[idx[n]]] and [[1]]. *)

val compare_subscripts :
  first:index Index.t -> second:index Index.t -> comparison
(** Compares two subscripts at the same position of two paths. A slice or a
    multi-index gives {!Cannot_tell}. *)

val pp : loop_name:(label -> string) -> index Fmt.t
(** Prints a form as [n], [n+1], [n+k-1], [-n+N+1], [3] or [k+1], and a
    subscript without a form as [?]. [loop_name] gives the loop variable of a
    loop label. *)
