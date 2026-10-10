open Std
open Middle

(** {1 Purpose of access analysis}

    Access analysis identifies the values a statement reads and updates.
    Dependency checks need the selected elements as well as variable names: a
    write to [theta[1]] cannot supply a read of [theta[2]].

    Analysis first collects accesses from each statement, then prepares index
    expressions for comparison across loop iterations. The resulting access
    information lets a dependency query compare candidate writes with reads.

    The stages below use one assignment to show the collected and prepared
    access information. Assume the analyzed block contains a loop over [n] and
    the following assignment, with [k] an invariant input defined outside the
    block:
    {@stan[
      y[n] = v[n + 1] + v[k];
    ]}

    {1 Collect statement inputs and updates}

    Collection preserves the selected elements and the expressions used to
    select them. Reading [v[k]] also reads [k], so dependencies must account for
    both the vector element and the input that selects the element.

    For the example assignment, collection produces the following access list.
    Indices still contain the original program expressions:
    {@text[
      position   kind    selected value
      1          read    n
      2          read    v[n + 1]
      3          read    n
      4          read    v[k]
      5          read    k
      6          write   y[n]
    ]}
    The first read of [n] comes from the destination index; the second comes
    from selecting [v[n + 1]]. The read of [k] comes from selecting [v[k]].

    Each statement's own expressions are collected separately from nested
    statements. Separate access lists identify which statement reads or updates
    a value. For a loop or conditional statement, the list contains reads from
    the bounds or condition. Queries reach those reads by following control
    dependencies from the body statements.

    Collection also distinguishes adding to the log density from reading the
    accumulated log density. Distinguishing additions from reads prevents
    separate additions from creating dependencies on each other merely because
    both update the same accumulator. For example, [target += 1] contributes an
    increment of [target], whereas [target()] contributes a read of [target].

    {1 Prepare indices for comparison}

    Collected indices are still program expressions. Comparing an index across
    iterations requires distinguishing the changing loop variable from invariant
    inputs and constant offsets.

    Analysis uses the enclosing loops and writes across the analyzed statements
    to identify those parts. A written input can change within the analyzed
    region, so comparisons cannot assume that input remains invariant.

    When an index's parts can be identified, the parts constrain element
    comparisons. An unresolved index supplies no constraint. Other subscripts or
    tuple fields can still prove that two accesses select different elements.

    Reads of enclosing loop variables are accounted for through dependencies on
    the loops. Removing separate loop-variable reads lets queries follow the
    loop bounds instead of searching for assignments to loop variables.

    For the example assignment, preparation removes both explicit reads of [n]
    and stores the following access list:
    {@text[
      reads:      v[n + 1], v[k], k
      writes:     y[n]
      increments: none
    ]}
    The subscripts now hold the parts used for comparisons rather than the
    original expressions. The following table shows the prepared index at each
    selected element:
    {@text[
      selected value   loop variable   invariant expression   constant offset
      v[n + 1]         n               none                   1
      v[k]             none            k                      0
      y[n]             n               none                   0
    ]}
    The access to [k] has no subscript because the read selects the whole
    variable. Keeping the read of [k] lets a query follow the input that selects
    [v[k]].

    {1 Use prepared accesses in dependency checks}

    Prepared accesses supply variable names for candidate selection and element
    paths for comparisons. The example assignment yields the following name
    sets:
    {@text[
      names read:    {v, k}
      names written: {y}
    ]}
    A dependency query uses [{v, k}] to find candidate value sources. For a
    candidate write to [v], the query compares the write with both [v[n + 1]]
    and [v[k]]. For a candidate write to [k], the query compares the write with
    the whole-variable read of [k]. The separate control dependency on the loop
    accounts for [n] and the loop bounds. *)

(** {1 API reference}

    The declarations below specify access representations, collection and
    classification inputs, and operations for querying or printing accesses.

    {2 Representation types}

    Access paths hold program expressions before classification and classified
    indices afterwards. The type parameter permits both representations. *)

(** An index represented as [loopvar + symbol + const]. Absent terms contribute
    zero. When present, [loopvar] has coefficient one and [symbol] is an
    expression that reads neither an enclosing loop variable nor a variable
    written in the analyzed block.

    The examples below show how loop variables, symbolic expressions and
    constants populate the record fields. Assume a loop over [n], with [k] not
    written in the analyzed block:

    - [n + k - 1] has [loopvar = Some "n"], [symbol = Some k] and [const = -1].
    - [k] has [loopvar = None], [symbol = Some k] and [const = 0].
    - [3] has [loopvar = None], [symbol = None] and [const = 3].

    At most one symbolic expression is stored. For invariant inputs [k] and [j],
    the whole sum [k + j] becomes the symbol. Dependence tests require identical
    stored expressions for matching symbols. The comparison does not rearrange
    terms, so [k + j] and [j + k] are different symbols even though the sums are
    equal.

    Indices with matching [loopvar] and [symbol] fields differ only by their
    [const] fields. *)
type sequential =
  {const: int; symbol: Expr.Typed.t option; loopvar: string option}

(** The reason classification produced a [Varying] index. Only the printer uses
    the reason. *)
type varying_kind =
  | Written
      (** The index reads a variable written in the analyzed block, other than a
          recognized loop variable. For example, [v[m]] has a written index when
          the block contains [m = n]. A declaration also counts as a write, even
          if the variable is assigned only once. *)
  | Nonsequential
      (** The index uses a loop variable in a form the classifier cannot
          represent, such as [idx[n]], [2 * n] or [N - n]. The classifier cannot
          combine two symbolic terms with a loop variable, so [n + k + k] is
          also rejected. *)

(** A classified expression at one subscript position. *)
type index =
  | Sequential of sequential
      (** The index has the form described by {!sequential}. *)
  | Varying of varying_kind
      (** The index cannot constrain a subscript comparison. *)

(** One subscript position or tuple field in an access path. The path
    [x[i].2[j]] has the steps [i], [.2] and [j]. *)
type 'index step =
  | Subscript of 'index Index.t  (** One index position, such as [n + 1]. *)
  | Field of int  (** One tuple field, such as the [.2] in [t.2]. *)

(** Whether an access reads, writes or increments a variable. [Increment] models
    a read and write of [target]. The represented operations are
    [target += ...], [jacobian += ...] and calls to [_lp] or [_jacobian]
    functions.

    Dependency queries treat increments as a reduction: [target += b] does not
    depend on an earlier [target += a] merely because both update [target]. A
    read through [target()] can still depend on preceding increments.
    {!Dependence_analysis.flows_into} specifies the access-kind rules. *)
type kind = Read | Write | Increment [@@deriving equal]

(** One access to [var] by a statement. [kind] distinguishes reads, writes and
    increments. [path] identifies the selected element or tuple field, with one
    step per position. An empty path accesses the whole variable, as in [sum(v)]
    or a declaration of [v].

    The following example separates the variable name from the selected element
    in the read [v[n + 1]]. Before classification, the subscript holds the
    program expression [n + 1]:
    {@text[
      var:   v
      kind:  Read
      path:  one subscript, n + 1
    ]}
    {!pp_access} prints [R] for reads, [W] for writes and [+=] for increments.
    In that notation, [W theta[1]] and [R theta[2]] share a variable name but
    have different paths. *)
type 'index access = {var: string; path: 'index step list; kind: kind}

(** The accesses collected from one statement. For an assignment, the list
    contains reads in the left-hand indices, accesses in the right-hand
    expression, then the write. An indexed read precedes reads of variables in
    the index expressions: [v[k]] gives [R v[k], R k]. *)
type 'index t = 'index access list

(** {2 Collection and classification}

    Collection records a statement's accesses. Classification prepares their
    paths using the enclosing loop variables and the analyzed block's writes. *)

val of_stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t -> Expr.Typed.t t
(** Collects accesses with indices represented as program expressions. Processes
    only the statement's own expressions; nested statements are collected
    separately.

    Accesses are collected in the order specified by {!t}. The collected order
    does not establish a runtime evaluation order.

    Assignments contribute accesses from destination indices and the right-hand
    expression, then a write to the destination. Indexed reads include accesses
    from their index expressions. For example, reading [v[k]] gives
    [R v[k], R k] because selecting the element requires reading [k].

    Declarations read size expressions and the optional initializer, then write
    the declared variable. For example, [vector[N] y;] gives [R N, W y].

    For control statements, collection records the expressions that control
    execution. A [for] contributes accesses from its bounds; an [if] or [while]
    contributes accesses from its condition.

    Log-density updates record increments as described by {!kind}. [target()]
    records a read of [target], so a query can distinguish reading the
    accumulator from adding to it. *)

val classify :
     loopvars:string Set.Poly.t
  -> written_vars:string Set.Poly.t
  -> Expr.Typed.t t
  -> index t
(** Removes reads of [loopvars], then classifies the remaining access paths.
    [loopvars] contains the enclosing loop variables. [written_vars] contains
    all variables written or incremented in the analyzed block, including
    declarations. Access kinds and remaining access order are preserved.

    Reads of enclosing loop variables are removed because control dependencies
    lead to the loops and their bound expressions. The remaining indices are
    decomposed into the form described by {!sequential} where possible.

    When decomposition fails, the whole expression's reads determine the
    fallback. Reading a written variable gives [Varying Written]. Otherwise,
    reading an enclosing loop variable gives [Varying Nonsequential]. If neither
    check applies, the whole expression becomes an invariant symbol.

    The following diagram shows collection and both classification operations
    for [y[n] = v[n + 1] + v[k]]. Assume the analyzed block contains only the
    enclosing loop and assignment, with [k] defined outside the block:
    {@text[
      of_stmt: collect accesses
        R n, R v[n+1], R n, R v[k], R k, W y[n]
                             |
                             v
      classify: remove reads of loopvars = {n}
        R v[n+1], R v[k], R k, W y[n]
                             |
                             v
      classify: prepare paths using loopvars = {n}, written_vars = {y}
        v[n+1]: loopvar n, no symbol, constant 1
        v[k]:   no loopvar, symbol k, constant 0
        k:      empty path (the whole variable)
        y[n]:   loopvar n, no symbol, constant 0
    ]}
    The two reads of [n] come from the destination index and [v[n+1]]. Both are
    removed; the read of [k] remains because selecting [v[k]] depends on the
    invariant input.

    For varying examples, take [loopvars = {n}] and [written_vars = {m}]. The
    index in [v[m]] becomes [Varying Written], and the index in [v[2 * n]]
    becomes [Varying Nonsequential]. The stored reasons are used by printers. *)

(** {2 Access queries}

    Queries select accesses by variable name or collect the names read and
    written. The results support candidate selection and index classification.
*)

val of_var : string -> 'index t -> 'index t
(** Keeps only accesses to the named variable, preserving access order. For the
    prepared assignment example, selecting ["v"] gives [R v[n+1], R v[k]]. *)

val read_vars : 'index t -> string Set.Poly.t
(** Returns the names of variables read or incremented. Dependency queries use
    the names to look up candidate reaching definitions. The prepared assignment
    example gives [{"v"; "k"}]. *)

val written_vars : 'index t -> string Set.Poly.t
(** Returns the names of variables written or incremented. A map builder
    combines the sets from all analyzed statements to prepare [written_vars] for
    {!classify}. The assignment example gives [{"y"}]. *)

(** {2 Printers}

    Printers display access paths and reasons for unresolved indices. Grouping
    by access kind makes statement inputs and updates easy to find. *)

val pp_index : index Fmt.t
(** Prints sequential indices as [n], [n+1], [n+k-1], [3] or [k+1]. Varying
    indices print as [?written] or [?nonsequential]. *)

val pp_path : 'index Fmt.t -> 'index step list Fmt.t
(** Prints consecutive subscripts in one pair of brackets, as [[i, j]]. Slices
    print as [[1:k]], multi-indices as [[{idx}]] and tuple fields as [.2]. *)

val pp_access : 'index Fmt.t -> 'index access Fmt.t
(** Prints the kind, variable and path, as [W v[n+1]], [R k], [+= target] or
    [W t.2]. *)

val pp : 'index Fmt.t -> 'index t Fmt.t
(** Prints accesses grouped by kind: reads, increments, then writes. Order
    within each group is preserved. For example,
    [R v[n+1], R k, += target, W y[n]]. *)
