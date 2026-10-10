open Dataflow_types

(** {1 Purpose of element and execution-order checks}

    A dependency query has found a candidate write by variable name. The
    candidate write is the {e source}; the read is the {e sink}. Element and
    execution-order checks determine whether the source can supply the value
    selected by the sink.

    After names and access kinds permit a value flow, analysis identifies the
    loops enclosing both accesses. With the shared loops identified, analysis
    compares the selected elements and checks whether the source can execute
    before the sink.

    The sections below use the following loop to show the results of the loop,
    element and ordering checks. The write to [b[n]] is the source, and the read
    of [b[n - 1]] is the sink:
    {@stan[
      for (n in 2:N) {
        b[n] = a;
        c[n] = b[n - 1];
      }
    ]}
    A candidate remains when the available information cannot rule out a value
    flow.

    {1 Identify shared loops}

    The same element can be written and read in different loop iterations.
    Analysis identifies shared enclosing loops so index comparisons can relate
    the source iteration to the sink iteration.

    In the example, both accesses are enclosed by the same loop statement. The
    shared-loop check produces:
    {@text[
      source's enclosing loops: [for n in 2:N]
      sink's enclosing loops:   [for n in 2:N]
      shared loops:             [for n in 2:N]
    ]}

    The shared-loop list identifies the loops for which element comparisons can
    record constraints on matching iterations.

    {1 Compare all selected parts}

    Knowing the shared loops allows analysis to compare subscripts and tuple
    fields. Every position must permit the same element to be selected.
    Constraints from different positions must also agree on the matching
    iterations.

    Each comparison can prove that elements differ, establish a distance or
    leave the overlap unresolved. The {e distance} measures the separation
    between matching iterations as sink iteration minus source iteration. Nested
    loops can contribute several distances, one for each shared loop.

    In the example, the write in iteration [i] selects [b[i]], and the read in
    iteration [j] selects [b[j - 1]]. Matching elements require [i = j - 1], so
    the read occurs one iteration after the matching write. The comparison
    therefore establishes distance [1].

    The following result records the constraint from the example's single
    subscript position:
    {@text[
      source element:    b[n]      (loop n, constant offset 0)
      sink element:      b[n - 1]  (loop n, constant offset -1)
      required distance: 0 - (-1) = 1 at loop n
      comparison result: overlap constrained to distance 1 at loop n
    ]}
    The comparison has established which iterations can select the same element.
    The ordering check must still determine whether the write can execute first.

    {1 Check whether the write can execute first}

    Matching elements are necessary, but a read can receive only a value written
    before the read executes. The ordering check combines iteration distances
    with statement order.

    A positive distance places the sink in a later iteration. A negative
    distance places the sink in an earlier iteration. When both accesses occur
    in the same iteration, the write must precede the read in the program text.

    The example's distance [1] allows the write to supply the read in the
    following iteration. Because an earlier iteration executes before a later
    iteration, a write can supply a read even when the writing statement appears
    later in the loop body.

    For the example pair, the ordering check receives the distance constraint
    and the statement order, then returns the following decision:
    {@text[
      required distance:          1 at loop n
      write precedes read in text: yes
      write may execute first:    yes
      candidate pair:             retain
    ]}

    With nested loops, an earlier outer iteration can supply values even when
    the source appears later within an inner loop. Ordering must account for the
    outer loops as well as the innermost loop.

    {1 Retain candidates that cannot be ruled out}

    A retained pair is a possible dependency, not proof that the value flow
    occurs. Unresolved indices and unchecked loop bounds can preserve candidates
    that never supply a read.

    Conservative results prevent the query from dropping a possible source. The
    dependency query rejects a pair only when the element or ordering check
    rules out the required flow.

    Setting [N = 2] in the example shows the effect of unchecked bounds. Only
    iteration [2] executes, so the write selects [b[2]] and the read selects
    [b[1]]. The checks still produce:
    {@text[
      comparison result: overlap constrained to distance 1 at loop n
      ordering decision: write may execute first
      candidate pair:    retain
    ]}
    No iteration in the loop writes [b[1]]. The pair remains a possible
    dependency because the checks do not use the bounds to rule out the required
    earlier iteration. *)

(** {1 API reference}

    The declarations below specify loop identity, comparison results and the
    inputs needed for path comparison and execution-order checks.

    {2 Loop and result representation}

    Shared loops associate accesses with matching iterations. Comparison results
    record whether overlap is impossible, constrained or unresolved. *)

(** An enclosing loop, identified by the loop statement's label. [var] is the
    loop variable for a [for] and [None] for a [while]. *)
type loop = {id: label; var: string option}

val common_loops : loop list -> loop list -> loop list
(** Returns the longest common prefix of two enclosing-loop lists. Both inputs
    must be ordered outermost first. Loops are matched by label, so two sibling
    loops over [n] share no loop despite using the same variable name.

    The following calls show how the shared loops connect path comparison to the
    ordering check. [source] and [sink] are classified accesses whose names and
    kinds have already passed {!Dependence_analysis.flows_into}. [source_loops]
    and [sink_loops] contain their enclosing loops. [source_first_in_text] is
    true when the source statement precedes the sink:
    {[
    let loops = common_loops source_loops sink_loops in
    let comparison = compare_paths ~loops source sink in
    may_exist ~source_first_in_text comparison
    ]}
    The boolean result tells the caller whether to retain the access pair. For
    the running example, [loops] contains the loop over [n], [comparison] is
    [Constrained] with distance [1] at that loop, and the boolean result is
    true. *)

(** A distance at one enclosing loop, measured as sink iteration minus source
    iteration. Positive distances put the sink later; negative distances put the
    sink earlier. [None] leaves the distance unconstrained.

    A constrained result has one component per common enclosing loop, ordered
    outermost first. All-zero distances describe a loop-independent dependence.
    A nonzero distance describes a loop-carried dependence, carried by the
    outermost loop with a nonzero distance. *)
type component = {loop: loop; distance: int option}

(** The combined constraints from access-path comparisons. {!compare_paths} does
    not check access kinds or statement order; {!may_exist} inspects the result
    to decide whether the source may execute first. *)
type t =
  | Independent  (** The access paths cannot overlap. *)
  | Unknown
      (** Overlap has not been ruled out and no loop distance is constrained.
          Identical invariant indices and empty access paths return [Unknown].
      *)
  | Constrained of component list
      (** Matching elements must satisfy the recorded distance constraints. The
          nonempty list has one component per common enclosing loop, outermost
          first. At least one component has a known distance; the remaining
          distances can be unknown. *)

(** {2 Access-path comparison}

    Path comparison combines element constraints and iteration distances. The
    result is passed to the execution-order check. *)

val compare_paths :
     loops:loop list
  -> Accesses.index Accesses.access
  -> Accesses.index Accesses.access
  -> t
(** Compares the source access's path (first argument) with the sink access's
    path (second argument). The caller must establish that the accesses refer to
    the same variable. [loops] must come from {!common_loops}. Access kinds and
    statement order are not checked.

    For equal-length paths, corresponding subscripts and fields are compared.
    Different tuple fields give [Independent]; equal fields give [Unknown]
    because the field adds no loop-distance constraint.

    Two single indices can be compared when both are {!Accesses.sequential} with
    matching loop variables and symbolic expressions. The tests follow Goff,
    Kennedy and Tseng (1991). The names count loop variables, called "index
    variables" in the dependence literature:

    - The ZIV (zero index variables) test compares indices without loop
      variables. Different constants give [Independent], as for [theta[1]] and
      [theta[2]]. Equal constants give [Unknown].
    - The strong SIV (single index variable) test requires the same shared loop
      variable on both sides, with coefficient one. Let [i] and [j] be the
      source and sink iterations. Matching indices [i + source_offset] and
      [j + sink_offset] require [i + source_offset = j + sink_offset]. The
      iteration distance is therefore [j - i = source_offset - sink_offset].
      Other shared-loop distances remain unknown.

    An [Independent] position rules out the pair. Otherwise, position results
    are combined loop by loop, with all components describing the same shared
    loops in the same order. Conflicting known distances give [Independent]. If
    no known distances conflict, a known distance replaces an unknown distance;
    two unknown distances remain unknown.

    The following example shows why constraints from two positions must agree:
    {@stan[
      for (n in 2:N) {
        m[n, n] = a;
        g[n] = m[n - 1, n - 1];
      }
    ]}
    Both positions require distance [1], so the result is [Constrained].
    Changing the read to [m[n - 1, n]] requires both [i = j - 1] and [i = j]. No
    pair of iterations satisfies both equations, so the result is [Independent].

    Empty paths and paths of different lengths give [Unknown]. Unsupported pairs
    include different symbols, varying indices, slices, multi-indices and a loop
    variable belonging to only one access. An unsupported position adds no
    constraint; another position can still prove independence.

    Distances also remain unknown for shared [while] loops because a [while]
    loop has no index variable.

    Loop bounds are not checked. The following example shows why a constrained
    result does not prove that a matching write executes:
    {@stan[
      for (n in 2:2) {
        b[n] = a;
        c[n] = b[n - 1];
      }
    ]}
    Comparing [b[n]] with [b[n - 1]] gives [Constrained] with distance [1]. Only
    one iteration executes, so no earlier iteration writes the element read.
    {!may_exist} still retains the pair because the distance is positive.
    Additional comparison examples are in [test/unit/Dependence.ml]. *)

(** {2 Execution-order check}

    Ordering combines path constraints with the statements' textual order. A
    false result rejects the pair; a true result retains a possible flow. *)

val may_exist : source_first_in_text:bool -> t -> bool
(** Tests whether the source may execute before the sink. [source_first_in_text]
    is true when the source statement precedes the sink statement in the program
    text. [source_first_in_text] must also be false when both accesses belong to
    the same statement because an assignment reads the right-hand expression
    before writing the destination.

    Returns false for [Independent] and true for [Unknown]. For [Constrained],
    components are checked outermost first, skipping zero distances. The first
    remaining component decides: positive or unknown gives true; negative gives
    false. If all distances are zero, the result is [source_first_in_text]. The
    ordering rule follows Allen and Kennedy (2001, section 2.2).

    A positive distance permits a source in an earlier iteration even if its
    statement appears later in the text. An unknown distance also permits an
    earlier source iteration, but does not establish an iteration order.

    The following results show why an unknown outer-loop distance can change the
    ordering decision. Using the notation from {!pp}, [[m:0][n:-1]] gives false
    because the sink executes first in the same iteration of [m]. With
    [[m:*][n:-1]], an earlier iteration of [m] remains possible, so the result
    is true.

    The following loop shows reads that match a written element but execute
    before the matching write:
    {@stan[
      for (n in 1:(N - 1)) {
        c[n] = b[n + 1];
        d[n] = b[n];
        b[n] = a;
      }
    ]}
    The write and read of [b[n]] have distance [0]. The writing statement comes
    later in the text, so the write cannot supply [d[n] = b[n]]. Comparing the
    write with [b[n + 1]] gives distance [-1]; the write would execute in a
    later iteration than the read. Both pairs give false.

    Rejection requires enough information to rule out an earlier matching write.
    When the available information cannot rule out a flow, a true result retains
    the possible dependency. Unchecked bounds and unresolved comparisons can
    therefore add false dependencies. *)

(** {2 Printers}

    Printers display comparison results and iteration constraints. Loop names
    and distances explain why a pair may or may not supply a value. *)

val pp_component : component Fmt.t
(** Prints a component as [[n:1]] for distance [1] at the loop over [n], [[m:*]]
    for an unknown distance, or [[while:*]] for a [while] loop. *)

val pp : t Fmt.t
(** Prints [independent], [unknown], or [constrained] followed by components in
    outermost-first order. For example, [constrained [n:1][m:*]] records known
    distance [1] at [n] and an unconstrained distance at [m]. *)
