open Dataflow_types

(** {1 Purpose}

    This module is a layer on top of {!Affine_subscript} that exists to compare
    two whole accesses of one variable, such as a write to [m[n, 1]] and a read
    of [m[n - 1, 1]], across every loop around both. The per-position answers of
    {!Affine_subscript} combine into one distance per common loop, which tells
    the dependence analysis:
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

    This module combines the two answers into one distance per loop,
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
      (** The accesses may select the same element. The list holds the distance
          at each common loop, outermost first. [None] is an unknown distance.
      *)

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
    {!test_dependence}, after the flag that says whether the writer comes before
    the reader in the program. The first distance that is not zero decides,
    outermost loop first: positive or unknown gives true, and negative gives
    false. When every distance is zero, the result is the flag. [Independent]
    gives false. *)

val pp : loop_name:(label -> string) -> common_loop_ids:label list -> t Fmt.t
(** Prints [independent], [unknown] when no distance is known, or [distances]
    with one [[loop:distance]] per common loop in [common_loop_ids], as in
    [distances [n:1][m:*]]. [loop_name] gives the loop variable of a loop label,
    or [while] for a [while] loop. *)
