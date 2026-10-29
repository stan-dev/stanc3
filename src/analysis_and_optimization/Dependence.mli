open Dataflow_types

(** {1 Purpose}

    This module decides whether a write can supply the value that a read uses.
    If so, the read {e depends} on the write. One read, write or increment of a
    variable is an {e access}, and {!Accesses} records the accesses of each
    statement.

    {!Dependence_analysis} first finds the writes that {e reach} each read. A
    write reaches a read when the program can go from the write to the read with
    no write between them that replaces the same element. The standard analysis
    that finds these writes is called {e reaching definitions}. Each write that
    reaches the read is a {e candidate source}, and the read is the {e sink}.

    Reaching definitions remove only the writes that a later write overwrites.
    They do not check whether the write selects the element that the read
    selects. They also do not check whether the write runs before the read. So a
    candidate source can still fail to supply the value. This module rejects
    such source and sink pairs. The module asks two questions.

    + Question 1 of 2: Can the source and the sink select the same element?
      ({!compare_paths})
    + Question 2 of 2: If so, can the source run before the sink?
      ({!is_plausible})

    The module rejects a pair only when one answer is provably no. So a kept
    pair is only a possible dependency.

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
    [[while:*]].

    {1 Relation to standard dependence testing}

    A {e common loop} is a loop around both the source and the sink. Loops match
    by the label of the loop statement, the integer that identifies the
    statement ({!Dataflow_types.label}). So two loops over [n], one after the
    other, do not match. A {e path} is the list of subscripts and tuple fields
    that select an element (see {!Accesses}). The module answers Question 1 with
    two standard tests. Each test compares the two paths at one position, which
    {!Accesses} calls a {e step} ({!Accesses.compare_steps}).

    Both tests use [Sequential] indices ({!Accesses.sequential}). A [Sequential]
    index has the form loop variable + symbol + constant. A {e symbol} is an
    expression that has the same value in every iteration, such as [N]. The
    index adds the loop variable ([Ascending]) or subtracts the loop variable
    ([Descending]). {!Accesses} calls this sign the {e direction} of the index.

    - The ZIV test (zero index variables) compares two indices without a loop
      variable, as in [theta[1]] against [theta[2]]. Different constants never
      select the same element, so the pair is independent. Equal constants leave
      the distance unknown. Tuple fields compare like constant subscripts, as in
      GCC.
    - The strong SIV test (single index variable) compares two indices with the
      same loop variable, the same direction and the same symbol. An example is
      [b[N - n]] against [b[N - n + 1]]. The loop variable must belong to a
      common loop. Call the constant of the source index [c1] and the constant
      of the sink index [c2]. In source iteration [i], the source selects
      [i + c1]. In sink iteration [j], the sink selects [j + c2]. These match
      when the distance [j - i] is [c1 - c2]. The sign flips when both indices
      are [Descending], as in [N - n]. The distance at each other common loop is
      unknown.

    {!compare_paths} combines the results of all positions. If any position
    shows that the accesses never select the same element, the result is
    [Independent]. If two positions give different distances at one loop, the
    result is also [Independent]. For example, [m[n, n]] against [m[n - 1, n]]
    needs the distances [1] and [0] at the same time. Otherwise, a known
    distance at a loop replaces an unknown one.

    These tests are a small subset of the tests in Goff, Kennedy and Tseng,
    "Practical Dependence Testing" (PLDI 1991). Allen and Kennedy, "Optimizing
    Compilers for Modern Architectures" (2001, chapters 2 and 3), describe the
    same tests. LLVM's [DependenceAnalysis] implements them. The names in this
    module map to the standard terms as follows.

    - LLVM calls the source [Src] and the sink [Dst].
    - A common loop has the same meaning as in LLVM. A {!level} holds the
      distance at one common loop.
    - The distance is LLVM's strong SIV formula [d = i' - i = (c1 - c2) / a].
      [i] and [i'] are the source and sink iterations, and [c1] and [c2] are the
      two constants. [a] is the factor of the loop variable, which is [1] or
      [-1] here.
    - LLVM also records a direction at each loop, which is the sign of the
      distance. This direction is unrelated to {!Accesses.direction}. LLVM gives
      an unknown distance the direction [*], which allows any direction. This
      module also prints an unknown distance as [*].
    - A {e distance vector} has one distance per common loop. [Distances] holds
      a distance vector.
    - [Unknown] means that the pair may depend, but no distance is known. LLVM
      calls this a confused dependence, and GCC calls this case
      [chrec_dont_know].
    - A distance vector is {e lexicographically positive} when the first nonzero
      entry of the vector is positive. {!is_plausible} applies the standard rule
      that a distance vector must be lexicographically positive.
    - [source_first_in_text] is true when the source statement comes before the
      sink statement in the program. The flag is false when both accesses are in
      one statement, because an assignment reads the right-hand side before the
      assignment writes. The flag has the role of LLVM's
      [PossiblyLoopIndependent], which allows a dependence inside one iteration.

    {!is_plausible} answers Question 2 with this rule. [Independent] gives
    false, and [Unknown] gives true. For [Distances], the first level that is
    not zero decides, from the outermost loop inward. A positive distance or a
    [*] gives true, because the source can run in an earlier iteration. A
    negative distance gives false, because the source runs in a later iteration.
    If every distance is zero, the result is [source_first_in_text]. The
    examples that follow use the notation of {!pp}.

    - [[m:0][n:-1]] gives false. In the same iteration of [m], the source runs
      one iteration of [n] after the sink.
    - [[m:*][n:-1]] gives true, because the source can run in an earlier
      iteration of [m].

    The cases that follow give [Unknown]. [Unknown] makes {!is_plausible} return
    true, so the module keeps the pair. Each name in parentheses is a standard
    test that this module does not implement.

    - A loop variable in only one index, as in [b[n]] against [b[1]] (the
      weak-zero SIV test).
    - One loop variable in opposite directions, as in [b[n]] against [b[N - n]]
      (the weak-crossing SIV test).
    - Other multiples of a loop variable, as in [b[2 * n]].
    - Indices with more than one loop variable (the MIV tests, for multiple
      index variables).
    - Paths of different lengths.

    The tests ignore loop bounds. A distance larger than the number of
    iterations still keeps the pair. *)

(** {1 API reference}

    The functions of this module work together as follows. [source] and [sink]
    are two accesses to the same variable. [source_loops] and [sink_loops] are
    the loops around each access, outermost first.
    {[
    let loops = common_loops source_loops sink_loops in
    is_plausible ~source_first_in_text (compare_paths ~loops source sink)
    ]} *)

(** A loop around an access. [id] is the label of the loop statement. [var] is
    the loop variable of a [for], or [None] for a [while]. The loop statement
    itself counts as inside the loop. The loop checks a [while] condition or a
    [for] upper bound again after each iteration. So writes in the body can
    reach these expressions. *)
type loop = Dataflow_types.loop = {id: label; var: string option}

val common_loops : loop list -> loop list -> loop list
(** Returns the common loops of two outermost-first loop lists, which is the
    longest start that both lists share. Loops match by [id]. *)

(** The distance at one common loop. [None] means that the distance is unknown,
    printed [*]. *)
type level = {loop: loop; distance: int option}

(** The result of a comparison of the elements that two accesses select. *)
type t =
  | Independent  (** The accesses never select the same element. *)
  | Unknown
      (** The accesses may select the same element, but no distance is known. *)
  | Distances of level list
      (** The accesses may select the same element, and at least one distance is
          known. The list has one level per common loop, outermost first. *)

val compare_paths :
     loops:loop list
  -> Accesses.index Accesses.access
  -> Accesses.index Accesses.access
  -> t
(** Answers Question 1 for the first access (the source) and the second access
    (the sink) of one variable. [loops] holds the common loops. *)

val is_plausible : source_first_in_text:bool -> t -> bool
(** Answers Question 2 for a result of {!compare_paths}. [source_first_in_text]
    tells whether the source statement comes before the sink statement. *)

val pp : t Fmt.t
(** Prints [independent], [unknown], or [distances] with one [[loop:distance]]
    per level, as in [distances [n:1][m:*]]. *)
