open Std
open Middle
open Dataflow_types

(** {1 Purpose}

    This module describes the accesses of one statement and compares two
    accesses of one variable. The parts build on each other, in the order below,
    which is also the order to read them in.
    - {!Loop_context} holds the loops of the analyzed statement and the
      variables that the analyzed statement defines.
    - {!Accesses} collects the reads, writes and read-writes of one statement,
      each with a path of subscripts and tuple fields. Collection only looks at
      syntax.
    - {!Access_function} explains one subscript expression of a path in a loop
      context. An access function keeps the expression and adds what the
      analysis can prove about the expression.
    - The functions after the submodules compare two paths of one variable. Two
      questions are kept apart: whether two accesses {e may} select the same
      element ({!test_dependence}), and whether a write {e must} replace the
      element of an earlier write ({!is_overwritten}).

    {!Dependence_analysis} collects the accesses of each statement of the
    analyzed code and answers the dependence queries.

    {1 Three different questions about two subscripts}

    The code keeps these three questions apart, because an answer to one is not
    an answer to another.
    - {e Same syntax}: the two subscripts are written as the same expression
      ([Accesses] uses this to join [x = x + 1] into one read-write). The same
      syntax does not prove the same value: [v[categorical_rng(theta)]] selects
      a new element at each evaluation.
    - {e Proven equal value}: the two subscripts have the same value under a
      stated condition, such as "in every iteration" or "in the same iteration".
      Only this proof lets a write overwrite an earlier write.
    - {e Possible overlap}: no test has proven that the two subscripts differ.
      An overlap that is only possible proves nothing about an overwrite.

    {1 Assumptions about arithmetic}

    A subscript is a Stan [int] expression. The generated C++ evaluates [+], [-]
    and [*] on [int] with the built-in operators, and an overflow there is
    undefined behavior. The tests below reason over the mathematical integers,
    which agrees with every execution that has defined behavior. This justifies
    a dependence verdict. This alone does not justify emitting a rewritten
    expression whose intermediate values differ from the original expression. *)

module Loop_context : sig
  (** {1 Purpose}

      A loop context holds what the analysis needs to know about the code around
      an access: the loops of the analyzed statement, and the variables that the
      analyzed statement defines. A loop is identified by the label of the loop
      statement. Two loops with the same variable name and the same bounds are
      still two loops.

      A context is built for one analyzed statement and is valid only for that
      statement. After a transformation, the context and every access function
      made with the context must be rebuilt. *)

  (** The context keeps the [For] or [While] statement of each loop label. The
      [for] statement owns the loop variable and the bound expressions. The
      bounds alone do not prove which iterations run: a bound can change between
      runs of the loop, and a [break] can end the loop early. *)
  type t

  val create :
       ((Expr.Typed.t, label) Stmt.Pattern.t * 'meta) LabelMap.t
    -> defined_vars:string Set.Poly.t
    -> t
  (** Builds the context of the statement map of the analyzed statement.
      [defined_vars] holds each variable that the analyzed statement writes. *)

  val for_loop_id : t -> loop_ids:label list -> string -> label option
  (** Returns the label of the [for] loop in [loop_ids] whose loop variable has
      the given name, or [None]. [loop_ids] holds the loops around one
      statement, so the same name in a sibling loop is a different loop. *)
end

module Accesses : sig
  (** {1 Purpose}

      This module records the accesses of each statement. An {e access} is one
      read, write or read-write of a variable. Each access has a {e path}. The
      path lists the subscripts and tuple fields that select an element of the
      variable.

      Collection looks only at the syntax of the statement. The path of a
      collected access holds the subscript expressions as written, including
      slices, multi-indices and expressions that no test can analyze, such as
      [idx[n]] or [n * n]. {!Access_function} later explains each expression,
      and nothing is thrown away when an expression has no explanation.

      {1 Example}

      {!Dependence_analysis} analyzes one statement, such as the whole log
      density, together with the statements nested inside. This documentation
      calls that statement the {e analyzed statement}.

      In this example, the analyzed statement has a loop over [n], and the loop
      holds this assignment. The analyzed statement does not write [k].
      {@stan[
        y[n] = v[n + 1] + v[k];
      ]}
      {!of_statement} gives these accesses:
      {@text[
        R n, R v[n+1], R n, R v[k], R k, W y[n]
      ]}
      Each access prints as the kind of access, then the variable and the path.
      [R] is a read, [W] is a write and [RW] is a read-write. Consecutive
      subscripts share one pair of brackets, as in [[i, j]]. A slice prints as
      [[1:k]], a multi-index as [[{idx}]], and a tuple field as [.2]. The two
      reads of [n] come from the subscripts of [y[n]] and [v[n + 1]]. *)

  (** {1 API reference} *)

  (** One position in a path. A step is a subscript or a tuple field. The path
      of [x[i].2[j]] has the three steps [i], [.2] and [j]. ['scalar] is the
      type of a subscript: an expression in a collected path, and an
      {!Access_function.t} in an analyzed path. The path has the same shape
      before and after the analysis. *)
  type 'scalar step =
    | Subscript of 'scalar Index.t
        (** One subscript: a single index such as [n + 1], a slice or a
            multi-index. *)
    | Field of int  (** One tuple field, such as [.2] in [t.2]. *)

  (** A statement reads a variable, writes a variable, or reads and writes the
      same element of a variable, as [x += 1] and [target += 1] do. A read-write
      is one definition and one use. *)
  type kind = Read | Write | ReadWrite [@@deriving equal]

  (** One access by a statement. [var] is the variable, [path] selects the
      element and [kind] tells how the statement uses the variable. An empty
      path means the whole variable, as in [sum(v)] or the declaration of [v].
  *)
  type 'scalar access = {var: string; path: 'scalar step list; kind: kind}

  (** The accesses of one statement. *)
  type 'scalar t = 'scalar access list

  val of_statement :
    (Expr.Typed.t, 'substatement) Stmt.Pattern.t -> Expr.Typed.t t
  (** Collects the accesses in the expressions of a statement. Nested statements
      are collected separately. The accesses come in this order, which is not
      the run-time order.
      - An assignment gives the reads in the left-hand indices, then the
        accesses in the right-hand side, then a write to the left-hand side.
        When the right-hand side reads the written element with the same syntax,
        as [x = x + 1] does, that read and the write are one read-write, [RW x].
      - A declaration gives the reads in the sizes and the initial value, then a
        write to the whole variable. [vector[N] y;] gives [R N, W y].
      - A [for] loop gives the accesses in the loop bounds. An [if] or a [while]
        gives the accesses in the condition.
      - [target += e], [jacobian += e], a [~] statement and a call of an [_lp]
        or [_jacobian] function give a read-write of [target]. [target()] gives
        a read of [target].
      - A subscript after a slice, as the [[2]] in [v[1:k][2]], selects from the
        result of the slice. The path stops at the slice, and the later
        subscripts only give their reads. *)

  val map_scalars : f:('scalar -> 'mapped) -> 'scalar access -> 'mapped access
  (** Applies [f] to each subscript expression of the path and keeps the shape
      of the path. *)

  val used_vars : 'scalar t -> string Set.Poly.t
  (** Returns the names of the variables that the accesses use, which means read
      or read-write. In the example above, the result is [{"n"; "v"; "k"}]. *)

  val defined_vars : 'scalar t -> string Set.Poly.t
  (** Returns the names of the variables that the accesses define, which means
      write or read-write. In the example above, the result is [{"y"}]. *)

  val pp_path : 'scalar Fmt.t -> 'scalar step list Fmt.t
  (** Prints a path, as in [[n+1]], [[n, 1]] or [.2], with the given printer for
      each subscript expression. An empty path prints nothing. *)

  val pp_access : 'scalar Fmt.t -> 'scalar access Fmt.t
  (** Prints the kind, the variable and the path in the notation of the example
      above, as in [W v[n+1]], [R k], [RW target] or [W t.2]. *)

  val pp : 'scalar Fmt.t -> 'scalar t Fmt.t
  (** Prints the reads, then the read-writes, then the writes, each group in the
      original order, as in [R v[n+1], R k, RW target, W y[n]]. *)
end

module Access_function : sig
  (** {1 Purpose}

      An access function explains one subscript expression, such as the [n + 1]
      in [v[n + 1]], in the loop context of the access. The access function
      keeps the expression and adds a {e view}: a restricted form that the
      subscript tests can calculate with. An expression without a view is still
      an access function. A missing view means that no present test can use the
      expression, and a later analysis can look at the same expression.

      {1 The unit-stride view}

      The one view today has three parts, and a missing part counts as zero.
      - {e constant}: every integer literal in the sum, added up, such as the
        [1] in [n + 1].
      - {e invariant part}: the terms of the sum that are stable, such as [N],
        [k + k] or [-k], kept as one expression.
      - {e loop term}: the variable of one enclosing [for] loop, added or
        subtracted once, such as the [n] in [n + 1] or the [-n] in [N - n].

      This view is narrower than an affine expression. [2 * n] is affine and has
      no view. The invariant parts of two views are compared as expressions, so
      [k + j] and [j + k] are different invariant parts.

      {1 Stability}

      A term is {e stable} when the term has the same value at every evaluation
      in the analyzed statement. A stable term names no variable that the
      analyzed statement defines and no loop variable, and does not draw a
      random number, read the target or have a side effect. Only a stable term
      can be the invariant part of a view. For example [categorical_rng(theta)]
      names no defined variable but is not stable, so
      [v[categorical_rng(theta)]] has no view. *)

  type t

  val of_expr : Loop_context.t -> loop_ids:label list -> Expr.Typed.t -> t
  (** Returns the access function of a subscript expression. [loop_ids] holds
      the labels of the loops around the statement of the access, outermost
      first. The grouping of a sum does not matter: [n + k + k], [k + k + n] and
      [n + (k + k)] all have the loop term [n] and the invariant part [k + k].
  *)

  val pp : loop_name:(label -> string) -> t Fmt.t
  (** Prints the view as [n], [n+1], [n+k-1], [-n+N+1], [3] or [k+1], and an
      access function without a view as the source expression, such as [idx[n]]
      or [(2 * n)]. [loop_name] gives the loop variable of a loop label. *)
end

(** {1 Comparing two accesses}

    The functions below compare two whole accesses of one variable, such as a
    write to [m[n, 1]] and a read of [m[n - 1, 1]].

    {2 Possible overlap}

    {!test_dependence} asks in which pairs of iterations the write and the read
    can select the same element. Each position of the two paths is tested alone,
    and a test gives one of three answers.
    - {e Disjoint}: the two steps never select the same element, so the accesses
      are independent.
    - {e A distance at a loop}: the two steps select the same element only when
      the read runs a fixed number of iterations of that loop after the write.
    - {e No information}: the test proves nothing, and the result stays as
      before.

    The answers of all positions must hold together. Two different distances at
    one loop therefore prove independence. The result is a summary that holds
    every real overlap. The result does not prove that an overlap exists.

    Two subscript tests exist, from Goff, Kennedy and Tseng,
    {{:https://doi.org/10.1145/113445.113448} "Practical Dependence Testing"}
    (PLDI 1991).

    The ZIV test (zero index variables) compares two subscripts without a loop
    term and with the same invariant part. Different constants are disjoint, as
    [v[1]] and [v[2]], or [v[k]] and [v[k + 1]], are.

    The strong SIV test (single index variable) compares two subscripts with the
    same loop term and the same invariant part, such as [n + 1] and [n - 2].
    With the write in iteration [i] and the read in iteration [j], the equation
    is [i + 1 = j - 2], so the distance [j - i] is [3]: [v[n + 1]] in iteration
    2 and [v[n - 2]] in iteration 5 both select [v[3]]. The write iteration and
    the read iteration are two unknowns, and the test never treats them as one.
    For a negated loop variable, as in [N - n], the sign of the distance flips.

    {@stan[
      for (i in 1:I) {
        for (j in 2:J) {
          a[i, j] = b;
          c[i, j] = a[i, j - 1];
        }
      }
    ]}
    At the first position, [[i]] against [[i]] gives the distance [0] at [i]. At
    the second position, [[j]] against [[j - 1]] gives the distance [1] at [j].
    The result prints as [distances [i:0][j:1]]. For [a[i, i]] against
    [a[i - 1, i]], the first position needs a distance of [1] at [i] and the
    second needs [0], so the result is [independent].

    A path can be shorter than the other, as a write to [m] and a read of
    [m[1]]. The whole object holds the element, so the result is a possible
    overlap with no known distance.

    {2 Execution order}

    A possible overlap does not say which access runs first.
    {!is_writer_possibly_first} reads the distances from the outermost loop
    inward. The first distance that is not zero decides: a positive distance
    means that the write runs in an earlier iteration, and a negative distance
    that the write runs in a later one. An unknown distance allows an earlier
    iteration. When every distance is zero, both run in the same iteration and
    the order of the two statements in the loop body decides.

    {2 Overwrite}

    {!is_overwritten} is a separate proof with separate rules. A possible
    overlap, an unknown result or a distance of zero does not prove that a write
    replaces an element. An overwrite needs a proven equal value at each
    position of the killer path. {!Dependence_analysis} adds the conditions on
    control flow.

    {2 Related work}

    Allen and Kennedy,
    {{:https://shop.elsevier.com/books/optimizing-compilers-for-modern-architectures/allen/978-0-08-051324-9}
     "Optimizing Compilers for Modern Architectures"} (2001, chapters 2 and 3),
    describe the tests, and LLVM's
    {{:https://github.com/llvm/llvm-project/blob/main/llvm/lib/Analysis/DependenceAnalysis.cpp}
     [DependenceAnalysis]} implements them. LLVM calls the writer [Src] and the
    reader [Dst]. A result with no known distance is what LLVM calls a confused
    dependence, and GCC [chrec_dont_know]. The [is_writer_first] flag of
    {!is_writer_possibly_first} has the role of the [PossiblyLoopIndependent]
    flag of LLVM's
    {{:https://github.com/llvm/llvm-project/blob/main/llvm/include/llvm/Analysis/DependenceAnalysis.h}
     [FullDependence]}. Both compilers have far more tests than this module. *)

(** {1 API reference} *)

(** An analyzed path: each subscript is an access function. *)
type path = Access_function.t Accesses.step list

val is_overwritten : is_same_iteration:bool -> path -> path -> bool
(** Whether a later write to the first path (the killer) is proven to overwrite
    each element that an earlier write to the second path (the killed write)
    wrote. The killer path can be shorter, so [m[1]] overwrites [m[1, 2]]. At
    each position of the killer path, the two steps must have a proven equal
    value. Steps without a loop term, as [[1]] and [[1]] or [[k]] and [[k]], are
    equal in any two iterations. Steps with a loop term, as [[n]] and [[n]], are
    equal only when [is_same_iteration] says that both writes run in the same
    iteration. A slice, a multi-index and a subscript without a view prove
    nothing. *)

(** The result of {!test_dependence}: independent, or a possible overlap with
    one distance per common loop. The result holds the labels of the common
    loops, outermost first, so that a distance cannot be read against the wrong
    loop. *)
type t

val test_dependence : label list * path -> label list * path -> t
(** Tests whether the first path (the write) and the second path (the read) of
    one variable can select the same element, and if so, at which distance in
    each loop around both. Each path comes with the labels of the loops around
    the statement of the access, outermost first. *)

val is_overlap_possible : t -> bool
(** Whether the result is a possible overlap. False means that the two accesses
    are proven independent. True does not prove that an overlap exists. *)

val is_writer_possibly_first : is_writer_first:bool -> t -> bool
(** Whether the write can run before the read and select the same element.
    [is_writer_first] says whether the writer comes before the reader in the
    loop body, which decides when every distance is zero.

    For [b[n] = a; c[n] = b[n - 1];] the distance is [1]: the read runs one
    iteration after the write, so the result is true. For a read of [b[n + 1]]
    the distance is [-1]: the element is written one iteration later, so the
    result is false. An independent result gives false. *)

val known_distance : loop_id:label -> t -> int option
(** Returns the distance at a common loop: the iteration of the read minus the
    iteration of the write. [None] means that no distance is known at that loop.
    [None] does not mean independence, and [Some 0] says nothing about the other
    loops. *)

val pp : loop_name:(label -> string) -> t Fmt.t
(** Prints [independent], [unknown] when no distance is known, or [distances]
    with one [[loop:distance]] per common loop, as in [distances [n:1][m:*]].
    [loop_name] gives the loop variable of a loop label, or [while] for a
    [while] loop. *)
