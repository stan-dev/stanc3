open Std
open Middle

(** {1 Purpose of uninitialized-variable analysis}

    Uninitialized-variable analysis finds reads that lack an assignment on at
    least one path to the read. Pedantic mode uses the results to warn about
    variables that may be used before receiving a value.

    The following example shows why an assignment in only one branch does not
    initialize a variable at a later read. The sections below follow the read of
    [x] through read collection, assignment tracking and reporting:
    {@stan[
      data { int use_x; }
      model {
        real x;
        if (use_x) x = 1;
        print(x);
      }
    ]}

    {1 Collect reads and execution paths}

    Analysis records the variable names and source locations read by each
    statement. A control-flow graph connects statements along the paths that can
    reach a read. Assignment tracking uses the collected names as the set of
    variables to analyze.

    For the example, collection produces:
    {@text[
      statement       names read
      real x;         {}
      if (use_x)      {use_x}
      x = 1;          {}
      print(x);       {x}

      names to analyze: {use_x, x}
    ]}
    Both branches of the [if] lead to [print(x)]. The path through the body
    includes [x = 1]; the path that skips the body has no assignment to [x].

    {1 Find assignments present on every path}

    Each statement has an entry set of variables already assigned on every
    incoming path and an exit set that also includes assignments made by the
    statement. At a join, intersection keeps only names assigned on all paths.
    Analysis repeats the updates until the sets stop changing.

    The following sets show the result for [x] at the example's branch join:
    {@text[
      path through x = 1: assigned names {x}
      path skipping body: assigned names {}
      entry to print(x):  intersection = {}
    ]}
    The read of [x] is absent from the entry set, so the read is a candidate for
    reporting.

    {1 Report reads using the program context}

    Some values come from outside the analyzed statements. Analysis removes
    candidates already supplied by the program context, such as data variables
    and function arguments. The remaining results pair each read's source
    location with the variable name.

    In the example, assignment tracking also finds no assignment to [use_x]. The
    program context supplies [use_x] as data, so reporting produces:
    {@text[
      candidate reads: (location of use_x in the condition, use_x)
                       (location of x in print(x), x)
      supplied by context: {use_x}
      reported reads:  {(location of x in print(x), x)}
    ]}
    Pedantic mode turns the reported read into a warning about [x]. *)

(** {1 API reference}

    The query below selects program regions and returns read locations. Warning
    formatting and deduplication are handled by pedantic analysis. *)

val mir_uninitialized_variables :
  Program.Typed.t -> (Location_span.t * string) Set.Poly.t
(** Returns [(read_location, variable_name)] pairs for reads not known to have
    an assigned value. Runs {!Monotone_framework.initialized_vars_mfp} on
    transformed data, both log-density versions, generated quantities and each
    function body.

    Function names and compiler flag variables are excluded in every region.
    Data names are also excluded in program blocks; parameter names are excluded
    outside transformed data. Function bodies instead exclude their argument
    names. Transformed-data statements are prepended when checking the log
    density and generated quantities, so assignments in transformed data can
    initialize later reads.

    The analysis tracks variable names, not individual elements. An assignment
    to [v[1]] marks [v] as assigned without checking other elements. Read
    collection uses {!Mir_utils.stmt_rhs_var_set}; declaration expressions and
    assignment destination indices are not included.

    Pedantic analysis removes results with an empty location and reports each
    variable once. For the overview example, the result contains only the
    location and name of [x] in [print(x)]. *)
