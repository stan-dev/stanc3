open Dataflow_types

(** {1 Purpose}

    This module compares the paths of two accesses to one variable. One read,
    write or increment of a variable is an {e access}, and the {e path} of an
    access lists the subscripts and tuple fields that select an element
    ({!Accesses}). {!Dependence_analysis} asks two kinds of question about two
    paths.

    The first kind comes from {e reaching definitions}, the analysis that finds,
    for each read, the writes that can supply the value the read uses. A write
    {e reaches} a read when the program can go from the write to the read with
    no write between them that replaces the same element. So reaching
    definitions must know when a later write replaces an earlier one, which
    {!is_overwritten} answers.

    The second kind decides whether a write can supply the value that a read
    uses. Each write that reaches the read is a {e candidate source}, and the
    read is the {e sink}. Reaching definitions do not check whether the source
    selects the element that the sink selects, or whether the source runs before
    the sink. So a candidate source can still fail to supply the value, and this
    module rejects such pairs with two questions.

    + Question 1 of 2: Can the source and the sink select the same element?
      ({!test_dependence})
    + Question 2 of 2: If so, can the source run before the sink?
      ({!is_lexicographically_positive})

    The module rejects a pair only when one answer is provably no. So a kept
    pair is only a possible dependence.

    {1 Running example}

    {@stan[
      for (n in 2:N) {
        b[n] = a;
        c[n] = b[n - 1];
      }
    ]}
    The source is the write to [b[n]], and the sink is the read of [b[n - 1]].
    In iteration [i], the source writes [b[i]]. In iteration [j], the sink reads
    [b[j - 1]]. The answer to Question 1 is yes, because they select the same
    element when [j = i + 1]. The {e distance} at the loop over [n] is the
    iteration of the sink minus the iteration of the source. Here the distance
    is [j - i = 1]. The answer to Question 2 is yes, because a positive distance
    means that the source runs first. So the module keeps the pair. {!pp} prints
    this result as [distances [n:1]]. The bracket holds the loop variable and
    the distance at that loop. A [while] loop has no loop variable and prints as
    [[while:*]], where [*] is an unknown distance.

    {1 Answering the questions}

    A {e common loop} is a loop around both the source and the sink
    ({!Dependence_analysis.common_loop_ids}). {!test_dependence} compares the
    two paths position by position with the subscript tests of
    {!Affine_subscript}. Two tuple fields compare like two constant subscripts,
    as in GCC. A position whose subscripts never select the same element makes
    the pair [Independent]. Another position can fix the distance at one common
    loop. If two positions fix different distances at one loop, the pair is also
    [Independent]. For example, [m[n, n]] against [m[n - 1, n]] needs the
    distances [1] and [0] at the same time.

    A {e distance vector} has one distance per common loop, outermost first. A
    distance vector is {e lexicographically positive} when the first nonzero
    entry is positive. {!is_lexicographically_positive} applies the standard
    rule that a dependence needs a lexicographically positive distance vector:
    the first distance that is not zero decides, from the outermost loop inward.
    A positive distance or an unknown one gives true, because the source can run
    in an earlier iteration. A negative distance gives false. If every distance
    is zero, the source and the sink run in the same iteration, which makes the
    dependence {e loop-independent}. A loop-independent dependence needs the
    source statement to come before the sink statement. It cannot exist when
    both accesses are in one statement, because an assignment reads the
    right-hand side before the assignment writes. In the notation of {!pp}:
    - [[m:0][n:-1]] gives false. In the same iteration of [m], the source runs
      one iteration of [n] after the sink.
    - [[m:*][n:-1]] gives true, because the source can run in an earlier
      iteration of [m].

    These tests are a small subset of the tests in Goff, Kennedy and Tseng,
    "Practical Dependence Testing" (PLDI 1991). Allen and Kennedy, "Optimizing
    Compilers for Modern Architectures" (2001, chapters 2 and 3), describe the
    same tests, and LLVM's [DependenceAnalysis] implements them. LLVM calls the
    source [Src] and the sink [Dst]. A vector with no known distance is what
    LLVM calls a confused dependence, and GCC [chrec_dont_know]. The
    [is_loop_independent_possible] flag has the role of LLVM's
    [PossiblyLoopIndependent].

    These cases leave every distance unknown, so the module keeps the pair. Each
    name in parentheses is a standard test that this module does not implement.
    - A loop variable in only one subscript, as in [b[n]] against [b[1]] (the
      weak-zero SIV test).
    - One loop variable with opposite signs, as in [b[n]] against [b[N - n]]
      (the weak-crossing SIV test).
    - Other multiples of a loop variable, as in [b[2 * n]].
    - Subscripts with more than one loop variable (the MIV tests, for multiple
      index variables).
    - Paths of different lengths.

    The tests ignore loop bounds. A distance larger than the number of
    iterations still keeps the pair. *)

(** {1 API reference} *)

val is_overwritten :
  is_same_iteration:bool -> Accesses.step list -> Accesses.step list -> bool
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
  common_loop_ids:label list -> Accesses.access -> Accesses.access -> t
(** Answers Question 1 for the first access (the source) and the second access
    (the sink) of one variable. [common_loop_ids] holds the labels of the common
    loops. *)

val is_lexicographically_positive :
  is_loop_independent_possible:bool -> t -> bool
(** Answers Question 2 for a result of {!test_dependence}.
    [is_loop_independent_possible] is the result when every distance is zero,
    and is true when the source statement comes before the sink statement. *)

val pp : loop_name:(label -> string) -> common_loop_ids:label list -> t Fmt.t
(** Prints [independent], [unknown] when no distance is known, or [distances]
    with one [[loop:distance]] per common loop in [common_loop_ids], as in
    [distances [n:1][m:*]]. [loop_name] gives the loop variable of a loop label,
    or [while] for a [while] loop. *)
