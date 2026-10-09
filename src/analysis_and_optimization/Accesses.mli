open Std
open Middle

(** {1 Purpose of access records}

    [Accesses] converts a statement into records of reads, writes and
    increments. Dependency analysis uses the records to identify possible inputs
    and compare accesses to individual elements or tuple fields.

    Access records are built in two stages. {!of_stmt} collects accesses with
    index expressions from the statement. {!classify} then converts the index
    expressions into forms that {!Dependence} can inspect.

    {1 Contents of an access record}

    An access record identifies the variable, operation and part of the variable
    accessed. Dependency analysis compares the records to find writes that can
    supply values to reads.

    Each {!access} contains a variable name, an access kind and a path. The
    kinds are [Read], [Write] and [Increment]. In the examples below, [R]
    denotes a read, [W] a write and [+=] an increment.

    A path contains one step per subscript position or tuple field. For example,
    [x[i].2[j]] has the steps [i], [.2] and [j]. An empty path accesses the
    whole variable, as in [sum(v)] or a declaration of [v]. Paths distinguish
    elements: [W theta[1]] and [R theta[2]] refer to different elements.

    An increment represents an update to [target]. Dependency analysis treats
    increments as a reduction. A [target += b] term does not acquire a data
    dependency on an earlier [target += a] term merely because both increment
    [target].

    {1 Collecting accesses from statements}

    Collection records the inputs and outputs of each statement before indices
    are classified. The collected accesses include variables used to compute
    indices and declared sizes. Index and size values also affect the statement.

    {!of_stmt} collects accesses from the statement's own expressions.
    Assignments read the expressions used in indices and on the right side, then
    write the destination. Declarations read size expressions and an optional
    initializer, then write the declared variable. For example, [vector[N] y;]
    produces [R N, W y].

    A [for] contributes reads from the loop bounds. An [if] or [while]
    contributes reads from the condition. Collection does not enter nested
    statements. Each nested statement needs a separate access record so
    dependency queries can identify the statement responsible for an access.

    Updates to the log density produce increments of [target]. Calls to
    [target()] produce reads of [target]. The {!kind} contract lists the
    operations represented as increments.

    {1 Classifying index expressions}

    Classification identifies index forms that dependence tests can compare
    across loop iterations. Unsupported forms leave the index comparison
    unconstrained. The classification needs enclosing loop variables and
    variables written anywhere in the analyzed block.

    A [Sequential] index has the form [loopvar + symbol + const]. The loop
    variable has coefficient one. The symbol is an expression that reads neither
    an enclosing loop variable nor a variable written in the block. Any of the
    three terms can be absent. Without a loop variable, the index stays constant
    between iterations.

    The following indices illustrate how the form represents moving and fixed
    indices. In a loop over [n], assume [k] is defined outside the analyzed
    block and is not written in the block. The indices [n + k - 1], [k] and [3]
    are sequential. The {!sequential} contract shows how the terms populate the
    record fields.

    Expressions outside the supported form are [Varying]. Written variables and
    unsupported uses of loop variables prevent classification as [Sequential].
    Declarations count as writes, even when a variable is assigned only once.
    The reason for a varying index is stored for printing.

    Classification also removes reads of enclosing loop variables. The enclosing
    loops supply the loop-variable values. {!Dependence_analysis} records the
    loops as control dependencies, so separate data dependencies for
    loop-variable reads are unnecessary.

    {1 Example: collecting and classifying an indexed assignment}

    The following example shows the changes between collection and
    classification. Index expressions contribute separate reads during
    collection. Classification removes loop-variable reads and preserves reads
    of other index inputs needed by dependency queries.

    Assume [k] is defined outside the analyzed block and is not written in the
    block. Inside [for (n in 1:N)], consider the assignment:
    {@stan[
      y[n] = v[n + 1] + v[k];
    ]}
    Collection produces six accesses: [R n, R v[n+1], R n, R v[k], R k, W y[n]].
    The first [R n] comes from the destination index. The second [R n] comes
    from the index in [v[n+1]]. Evaluating [v[k]] also contributes a separate
    read of [k].

    Classification with [loopvars = {n}] removes both reads of [n]. The
    remaining accesses print as [R v[n+1], R v[k], R k, W y[n]]. The indices
    [n+1], [k] and [n] are all sequential. The read of [k] remains because the
    enclosing loop supplies [n], while [k] comes from an external definition.

    For examples of other statement forms, see [test/unit/Accesses.ml]. *)

(** {1 API reference}

    The contracts below specify access representations, collection,
    classification, queries and printed output. The index type changes between
    collection and classification, while variable names and access kinds remain
    available for queries.

    {2 Representation types}

    The types describe index terms and paths, then combine the paths with
    variable names and access kinds. The type parameter permits paths containing
    either Stan expressions or classified indices. *)

(** An index represented as [loopvar + symbol + const]. Absent terms contribute
    zero. [symbol] is an expression that reads neither an enclosing loop
    variable nor a variable written in the analyzed block.

    The examples below show how loop variables, symbolic expressions and
    constants populate the record fields. Assume a loop over [n] in which [k] is
    not written in the analyzed block:

    - [n + k - 1] has [loopvar = Some "n"], [symbol = Some k] and [const = -1].
    - [k] has [loopvar = None], [symbol = Some k] and [const = 0].
    - [3] has [loopvar = None], [symbol = None] and [const = 3].

    At most one symbolic expression is stored. For [k + j], the whole sum
    becomes the symbol. Dependence tests compare the stored expressions
    structurally, so [k + j] and [j + k] are different symbols even though the
    sums are equal.

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
          the block contains [m = n]. A declaration counts as a write, even if
          the variable is assigned only once. *)
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

(** One index position or tuple field of an access path. *)
type 'index step =
  | Subscript of 'index Index.t  (** One index position, such as [n + 1]. *)
  | Field of int  (** One tuple field, such as the [.2] in [t.2]. *)

(** Whether an access reads, writes or increments a variable. [Increment] models
    a read and write of [target]. The represented operations are
    [target += ...], [jacobian += ...] and calls to [_lp] or [_jacobian]
    functions. See {!Dependence_analysis.flows_into} for dependency rules. *)
type kind = Read | Write | Increment [@@deriving equal]

(** One access to [var] by a statement. [path] identifies the element or field
    used, with one step per subscript position or tuple field. An empty path
    accesses the whole variable. *)
type 'index access = {var: string; path: 'index step list; kind: kind}

(** The accesses collected from one statement. For an assignment, the list
    contains reads in the left-hand indices, accesses in the right-hand
    expression, then the write. An indexed read precedes reads of variables in
    the index expressions: [v[k]] gives [R v[k], R k]. *)
type 'index t = 'index access list

(** {2 Collection and classification}

    {!of_stmt} produces records containing Stan index expressions. {!classify}
    uses block and loop information to prepare the records for dependence tests.
*)

val of_stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t -> Expr.Typed.t t
(** Collects accesses with indices represented as Stan expressions. A [for]
    contributes accesses from the loop bounds. An [if] or [while] contributes
    accesses from the condition. Accesses in nested statements are collected
    separately.

    Declarations read size expressions and the optional initializer, then write
    the declared variable. Log-density updates record increments as described by
    {!kind}. [target()] records a read of [target]. *)

val classify :
     loopvars:string Set.Poly.t
  -> written_vars:string Set.Poly.t
  -> Expr.Typed.t t
  -> index t
(** Classifies each index expression and removes reads of [loopvars]. [loopvars]
    contains the enclosing loop variables. [written_vars] contains all variables
    written or incremented in the analyzed block, including declarations.

    The following accesses illustrate the three classification outcomes. With
    [loopvars = {n}] and [written_vars = {m}], the index in [v[n + 1]] is
    [Sequential], the index in [v[m]] is [Varying Written], and the index in
    [v[2 * n]] is [Varying Nonsequential]. *)

(** {2 Access queries}

    Access queries either select the accesses to a named variable or return the
    names of variables read and written. Selecting a variable's accesses
    provides the paths needed for element comparisons. The name sets identify
    possible inputs to a statement and variables whose writes can affect index
    classification. *)

val of_var : string -> 'index t -> 'index t
(** Keeps only accesses to the named variable, preserving access order. *)

val read_vars : 'index t -> string Set.Poly.t
(** Returns the names of variables read or incremented. *)

val written_vars : 'index t -> string Set.Poly.t
(** Returns the names of variables written or incremented. *)

(** {2 Printers}

    The printers display indices, paths and access records for inspection. A
    statement's access list is grouped by access kind to make reads and updates
    easy to find. *)

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
