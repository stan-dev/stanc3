open Std
open Middle

(** {1 Purpose of uninitialized-variable analysis}

    A read of a variable is uninitialized if the program can reach the read
    without running an assignment to the variable. This analysis finds
    uninitialized reads for pedantic mode, which warns about legal code that is
    probably a mistake ({!Pedantic_analysis}). The code moved here from
    {!Dependence_analysis}.

    The analysis has three steps. Each section below describes one step with the
    following model. Only one branch assigns [x], so the read in [print(x)] is
    uninitialized.
    {@stan[
      data { int use_x; }
      model {
        real x;
        if (use_x) x = 1;
        print(x);
      }
    ]}

    {1 Step 1 of 3: Collect reads and paths}

    The analysis records the names that each statement reads, with the source
    location of each read. A read here is a variable in an expression of the
    statement ({!Mir_utils.stmt_rhs_var_set}). Declaration expressions and the
    indices of an assignment target do not count as reads. A control-flow graph
    links each statement to the statements that can run after that statement. A
    path is a sequence of statements along these links. The analysis uses this
    graph to follow every path to each read. The example gives the following
    reads.
    {@text[
      statement       names read
      real x;         {}
      if (use_x)      {use_x}
      x = 1;          {}
      print(x);       {x}
    ]}
    Two paths go from the [if] to [print(x)]. One path runs [x = 1]. The other
    path skips the body and does not assign [x].

    {1 Step 2 of 3: Find the names assigned on every path}

    Each statement has an entry set and an exit set. The entry set holds the
    names that every path into the statement assigns. The exit set adds the
    names that the statement assigns to the entry set. Where paths join, the
    entry set is the intersection of the exit sets of the statements before the
    join. A loop can lead a path back to an earlier statement. Then one pass
    does not give the final sets, so the analysis repeats the updates until the
    sets stop changing. A read is a candidate if the name of the read is not in
    the entry set of the statement that reads the name.
    {!Monotone_framework.initialized_vars_mfp} computes the sets.

    The sets hold whole variables. An element is one entry of a variable, such
    as [v[1]] of a vector [v]. An assignment to the element [v[1]] marks all of
    [v] as assigned. The example gives the following sets.
    {@text[
      entry of if (use_x):  {}
      exit of if (use_x):   {}
      exit of x = 1:        {x}
      entry of print(x):    {x} intersect {} = {}
    ]}
    The entry of [print(x)] joins the path through [x = 1] and the path that
    skips the body. The model does not assign [use_x], so the read of [use_x] in
    the [if] is a candidate. [x] is not in the entry set of [print(x)], so this
    read is also a candidate.

    {1 Step 3 of 3: Remove names that the program supplies}

    Some names get a value from outside the analyzed statements, for example
    data variables and function arguments. The analysis removes the candidates
    with these names. Each result pairs the location of a read with the name.
    Step 2 gives two candidates in the example. [use_x] is data, so this step
    removes the read of [use_x]. The only result is the read of [x] in
    [print(x)].

    A region is one part of the program that the analysis checks separately. The
    regions are transformed data, the two MIR versions of the log density
    ([log_prob] and [reverse_mode_log_prob]), generated quantities, and each
    function body. Each region ignores function names and compiler flag
    variables. Program blocks also ignore data names. Blocks after transformed
    data also ignore parameter names. Function bodies ignore the names of the
    function arguments. The log density and generated quantities checks run
    transformed data first, so the transformed data assignments can initialize
    later reads. Pedantic mode drops results with an empty location, and reports
    each name once. *)

(** {1 API reference} *)

val mir_uninitialized_variables :
  Program.Typed.t -> (Location_span.t * string) Set.Poly.t
(** Returns (read location, variable name) pairs for the uninitialized reads in
    each region of the overview. *)
