open Middle

(** {1 Purpose of pedantic analysis}

    Pedantic analysis finds model code that is legal but is probably a mistake.
    It returns a warning with a source location for each case.

    Three analyses supply the evidence. {!Dependence_analysis} finds the
    statements that each statement depends on, so the checks can find control
    flow that depends on a parameter. A factor is a term that [target +=] adds
    to the log density, a rejection or an [_lp] call. {!Factor_graph} links each
    factor to the data and parameters that it uses. A path is a chain of these
    links from factor to variable to factor. A factor is a prior of a parameter
    if it links to the parameter and has no path to data without the parameter.
    An unused parameter is a parameter that no factor links to. The factor graph
    finds unused parameters and priors. {!Uninitialized_variables} finds reads
    that can occur before an assignment. Other checks inspect parameter
    constraints and statement patterns.

    {1 Example of a control-flow warning}

    {!warn_pedantic} has three steps.
    + Simplify: run constant propagation, copy propagation and partial
      evaluation on the program.
    + Check: run each check on the simplified program.
    + Join: join the warnings of all checks and sort them.

    Step 1 makes some warnings more precise. Constant propagation replaces a
    variable with its constant value. Copy propagation replaces a variable with
    the variable that it copies. Partial evaluation computes expressions whose
    operands are constants. Some checks look only at literal numbers, such as
    the checks for constraint bounds and distribution arguments. After step 1, a
    local variable with a constant value becomes a literal, so these checks also
    see it.

    The following statement shows step 2. In it, [a] is a parameter.
    {@stan[
      if (a > 0) target += 1;
    ]}
    The check collects the names that the [if] and its dependencies read. It
    keeps the parameter names among them, and gives the following evidence.
    {@text[
      read names:      {a}
      parameter names: {a}
      evidence:        (location of the if, {a})
    ]}
    Each check turns its evidence into a location and a message. The message for
    the example is [A control flow statement depends on parameter(s): a.]. Step
    3 joins this warning with the warnings of the other checks and sorts them.
*)

(** {1 API reference} *)

(** A source location and a warning message. For a parameter-dependent [if], the
    location is the [if]. For an uninitialized variable, it is the read. *)
type warning_span = Location_span.t * string

val warn_pedantic : Program.Typed.t -> warning_span list
(** Returns all pedantic warnings, sorted by location and message. Runs the
    checks for parameter use, control flow, priors, distributions, constraints
    and uninitialized reads. *)

val warn_uninitialized : Program.Typed.t -> warning_span list
(** Returns the uninitialized-read warnings, sorted by location and message.
    Uses {!Uninitialized_variables.mir_uninitialized_variables}, drops empty
    locations, and keeps one warning for each variable name. For
    [real x; print(x);], the result holds the location of the read and the
    message
    [The variable x may not have been assigned a value before its first use.].
*)
