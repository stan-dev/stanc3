open Dataflow_types

(** {1 Purpose and caller responsibilities}

    [Dependence] compares two accesses to determine whether a candidate write
    can supply a read. The write is the {e source}; the read is the {e sink}.
    {!Dependence_analysis} uses the comparisons to remove candidate writes that
    cannot supply the read.

    A value can flow only when the accesses touch the same element or tuple
    field and the source executes before the sink. The analysis checks both
    requirements:

    + {!depends} compares access paths and computes possible distances between
      loop iterations.
    + {!may_exist} uses the comparison result and statement order to check
      whether the source can execute first.

    The caller must check variable names and access kinds before comparing
    paths. {!Dependence_analysis.flows_into} performs the name and kind checks.
    The caller must also identify loops shared by the source and sink.
    Unresolved comparisons retain possible flows for later dependency queries.

    {1 Shared loops and loop distances}

    Shared loops identify iterations in which both accesses can execute. Loop
    distances describe the separation between iterations that access the same
    element. The distances connect subscript comparisons to execution-order
    checks.

    {!common_loops} keeps loops shared by both accesses, starting from the
    outermost loop. The lists are ordered outermost first, and loops are matched
    by statement label. Two sibling loops over [n] are different loops despite
    sharing a variable name.

    The {e distance} at a shared loop is the sink iteration minus the source
    iteration. The following example derives a distance from matching the write
    [b[n]] with the read [b[n - 1]]:
    {@stan[
      for (n in 2:N) {
        b[n] = a;
        c[n] = b[n - 1];
      }
    ]}

    The write in iteration [i] and the read in iteration [j] access the same
    element when [i = j - 1]. The distance [j - i] is therefore [1]. The read
    accesses the element written one iteration earlier.

    Nested loops have one distance per shared loop, ordered outermost first. An
    unknown distance permits any separation in iterations. When every distance
    is zero, the dependence is {e loop-independent}. When a distance is nonzero,
    the dependence is {e loop-carried}. The outermost loop with a nonzero
    distance carries the dependence.

    {1 Comparison outcomes}

    A path comparison reports whether overlap is impossible or remains possible.
    For possible overlap, the result also reports known loop distances. The
    result determines whether a candidate needs an execution-order check.

    {!depends} returns one of three outcomes:

    - [Independent]: the access paths cannot overlap.
    - [Dependent]: the paths may overlap, and at least one loop distance is
      known. All-zero distances describe a loop-independent dependence.
    - [Unknown]: overlap has not been ruled out, and no loop distance is known.

    A [Dependent] result can contain unknown distances at other loops. Printers
    mark unknown distances with [*]. {!may_exist} rejects [Independent] and
    accepts [Unknown]. For [Dependent], {!may_exist} checks the reported
    distances and statement order.

    {1 Comparing access paths}

    Path comparisons check tuple fields and subscripts, then combine the
    constraints from each position. One position can prove that the source and
    sink never touch the same element. Combining positions also detects
    conflicting loop distances.

    An {!Accesses.access} path contains subscript positions and tuple fields.
    For equal-length paths, {!depends} compares corresponding positions.
    Different tuple-field numbers give [Independent]. Equal field numbers give
    [Unknown] because matching a field constrains no loop distance.

    Two single indices can be compared when both are {!Accesses.sequential} and
    have matching loop variables and symbolic expressions. The tests follow
    Goff, Kennedy and Tseng (1991). Test names count loop variables, called
    "index variables" in the dependence literature:

    - ZIV (zero index variables): neither index uses a loop variable. Different
      constants give [Independent], as for [theta[1]] and [theta[2]]. Equal
      constants give [Unknown]: the indices match but constrain no loop
      distance.
    - Strong SIV (single index variable): both indices use the same loop
      variable with coefficient one, as for [b[n]] and [b[n - 1]]. The loop over
      [n] must enclose both accesses. The distance is the source constant minus
      the sink constant. For [b[n]] and [b[n - 1]], the distance is
      [0 - (-1) = 1]. Other enclosing loop distances remain unknown.

    To combine results, every position must allow overlap. Known distances at
    each loop must agree, and an unknown position adds no constraint.

    The following example shows how subscript positions can agree on a distance
    or conflict and prove independence:
    {@stan[
      for (n in 2:N) {
        m[n, n] = a;
        g[n] = m[n - 1, n - 1];
      }
    ]}
    Both subscript positions require distance [1] at [n]. {!depends} returns
    [Dependent], printed [dependent [n:1]]. Replacing the read with
    [m[n - 1, n]] introduces conflicting distances. For a write in iteration [i]
    and a read in iteration [j], the positions require [i = j - 1] and [i = j].
    No pair of iterations satisfies both equations, so the result is
    [Independent].

    {1 Checking execution order}

    Overlapping paths can carry a value only when the source executes first.
    {!may_exist} checks loop distances before statement order because an earlier
    iteration can supply a read in a later iteration. A source can therefore
    come after the sink in the program text.

    A positive distance places the sink in a later iteration than the source.
    Zero places both accesses in the same iteration. A negative distance places
    the sink in an earlier iteration.

    For nested loops, {!may_exist} checks distances outermost first and skips
    zero distances. The check stops at the first positive, negative or unknown
    distance. A positive distance permits flow. A negative distance rules out
    flow. An unknown distance permits flow because the source can execute in an
    earlier iteration. The ordering rule follows Allen and Kennedy (2001,
    section 2.2).

    The following distance vectors show why an unknown outer distance permits
    flow despite a negative inner distance. For loops over [m] and [n],
    [[m:0][n:-1]] rules out flow: the sink executes first in the same iteration
    of [m]. For [[m:*][n:-1]], the source can execute in an earlier iteration of
    [m], so flow remains possible.

    When all distances are zero, statement order decides which access executes
    first. For example, [b[n] = a; c[n] = b[n];] has distance [0], and the write
    precedes the read. Reversing the statement order prevents the write from
    supplying the read.

    {1 Limits of the analysis}

    Unsupported comparisons and unchecked loop bounds can leave impossible flows
    among the retained candidates. The limits explain why a positive
    execution-order result represents a possible flow rather than a confirmed
    flow.

    A whole-variable access has no positions to compare and gives [Unknown].
    Paths of different lengths also give [Unknown].

    Unsupported index pairs include different symbolic expressions, varying
    indices, slices and multi-indices. An unsupported pair leaves its position
    unconstrained. Other positions can still prove independence.

    A loop variable belonging to only one access cannot constrain a shared-loop
    distance. A [while] loop has no loop variable, so its distance remains
    unknown.

    The subscript tests do not check loop bounds. A distance larger than the
    number of iterations can still be reported as [Dependent]. The unknown
    result and unchecked bounds preserve candidate writes when the tests cannot
    prove independence.

    For examples of comparisons and execution-order checks, see
    [test/unit/Dependence.ml]. *)

(** {1 API reference}

    The contracts below specify loop inputs, comparison results and
    execution-order checks. Callers use the result of {!depends} as the input to
    {!may_exist}.

    {2 Loop and result representation}

    Loop records identify enclosing loops by statement label. Components and
    results store the constraints used by the execution-order check. *)

(** An enclosing loop, identified by the loop statement's label. [var] is the
    loop variable for a [for] and [None] for a [while]. *)
type loop = {id: label; var: string option}

val common_loops : loop list -> loop list -> loop list
(** Returns the longest common prefix of two enclosing-loop lists. Both inputs
    must be ordered outermost first. Loops are matched by label, so two sibling
    loops over [n] share no loop despite using the same variable name. *)

(** A distance at one enclosing loop, measured as sink iteration minus source
    iteration. [None] leaves the distance unconstrained. *)
type component = {loop: loop; distance: int option}

type t =
  | Independent  (** The access paths cannot overlap. *)
  | Unknown
      (** Overlap has not been ruled out and no loop distance is constrained.
          Identical invariant indices and empty access paths return [Unknown].
      *)
  | Dependent of component list
      (** The paths may overlap at the given distances. The nonempty list has
          one component per common enclosing loop, outermost first. A distance
          can be unknown even when another component has a known distance. *)

(** {2 Access-path comparison}

    {!depends} compares classified paths for a source and sink. The caller
    supplies their shared loops so distances are computed for loops enclosing
    both accesses. *)

val depends :
     loops:loop list
  -> Accesses.index Accesses.access
  -> Accesses.index Accesses.access
  -> t
(** Compares the source access's path (first argument) with the sink access's
    path (second argument). The caller must establish that the accesses refer to
    the same variable. [loops] must come from {!common_loops}. Access kinds and
    statement order are not checked. Returns the combined path constraints in
    {!t}. *)

(** {2 Execution-order check}

    {!may_exist} uses path constraints and statement order to decide whether
    flow remains possible. A false result lets the caller reject the access
    pair. *)

val may_exist : source_first_in_text:bool -> t -> bool
(** Tests whether the source may execute before the sink. [source_first_in_text]
    is true when the source statement precedes the sink statement in the program
    text.

    {!may_exist} returns false for [Independent] and true for [Unknown]. For
    [Dependent], {!may_exist} checks components outermost first, skipping zero
    distances. At the first remaining component, {!may_exist} returns true for a
    positive or unknown distance and false for a negative distance. An unknown
    distance allows the source to run in an earlier iteration. If all distances
    are zero, {!may_exist} returns [source_first_in_text]. *)

(** {2 Printers}

    The printers display comparison outcomes and loop distances for inspection.
    Unknown distances use [*] so unresolved constraints remain visible. *)

val pp_component : component Fmt.t
(** Prints a component as [[n:1]] for distance [1] at the loop over [n], [[m:*]]
    for an unknown distance, or [[while:*]] for a [while] loop. *)

val pp : t Fmt.t
(** Prints [independent], [unknown], or [dependent] followed by components in
    outermost-first order, as [dependent [n:1][m:*]]. *)
