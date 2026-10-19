open Std
open Middle

(** {1 Purpose of uninitialized-variable analysis}

    A read of a variable is uninitialized if at least one path to the read has
    no assignment to the variable. This analysis finds uninitialized reads for
    pedantic mode. The code moved here from {!Dependence_analysis}.

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

    The analysis records the names that each statement reads, with their source
    locations. It checks each name that a statement reads. A control-flow graph
    links each statement to the statements that can run after it. The analysis
    uses this graph to follow every path to each read. The example gives the
    following reads.
    {@text[
      statement       names read
      real x;         {}
      if (use_x)      {use_x}
      x = 1;          {}
      print(x);       {x}
    ]}
    Two paths go from the [if] to [print(x)]. One path runs [x = 1]. The other
    path skips the body and has no assignment to [x].

    {1 Step 2 of 3: Find the names assigned on every path}

    Each statement has an entry set and an exit set. The entry set holds the
    names that every path into the statement assigns. The exit set adds the
    names that the statement assigns to the entry set. Where paths join, the
    entry set is the intersection of the exit sets before it. A loop can lead a
    path back to an earlier statement. Then one pass does not give the final
    sets, so the analysis repeats the updates until no set changes. A read is a
    candidate if its name is not in the entry set of its statement. The example
    gives the following sets.
    {@text[
      entry of if (use_x):  {}
      exit of if (use_x):   {}
      exit of x = 1:        {x}
      entry of print(x):    {x} intersect {} = {}
    ]}
    The entry of [print(x)] joins the path through [x = 1] and the path that
    skips the body. No statement assigns [use_x], so the read of [use_x] in the
    [if] is a candidate. [x] is not in the entry set of [print(x)], so this read
    is also a candidate.

    {1 Step 3 of 3: Remove names that the program supplies}

    Some names get a value from outside the analyzed statements, for example
    data variables and function arguments. The analysis removes the candidates
    with these names. Each result pairs the location of a read with the name.
    Step 2 gives two candidates in the example. [use_x] is data, so this step
    removes it. The only result is the read of [x] in [print(x)]. *)

(** {1 API reference} *)

val mir_uninitialized_variables :
  Program.Typed.t -> (Location_span.t * string) Set.Poly.t
(** Returns [(read_location, variable_name)] pairs for the uninitialized reads.
    Runs {!Monotone_framework.initialized_vars_mfp} on transformed data, both
    versions of the log density, generated quantities and each function body.

    Each of these parts is a region, and the analysis checks each region
    separately. Each region ignores function names and compiler flag variables.
    Program blocks also ignore data names. Blocks after transformed data also
    ignore parameter names. Function bodies ignore their argument names. The log
    density and generated quantities checks run transformed data first, so its
    assignments can initialize later reads.

    The analysis tracks whole variables, not elements. An assignment to [v[1]]
    marks all of [v] as assigned. Reads come from {!Mir_utils.stmt_rhs_var_set}.
    They do not include declaration expressions or the indices of an assignment
    target.

    Pedantic mode drops results with an empty location, and reports each name
    once. *)
