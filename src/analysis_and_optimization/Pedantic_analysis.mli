open Middle

(** {1 Purpose of pedantic analysis}

    Pedantic analysis finds model code that is legal but is probably a mistake.
    The analysis returns a warning with a source location for each case. Each
    check first collects evidence, which is the location of the suspect code and
    the names involved. The check then turns the evidence into a warning.

    Three analyses supply the evidence. A statement depends on another statement
    if the other statement can supply a value that the statement reads, or
    decides whether the statement runs. {!Dependence_analysis} finds these
    dependencies, so the checks can find control flow that depends on a
    parameter. A factor is a term that [target +=] adds to the log density, a
    rejection, or a call to an [_lp] function. An [_lp] function is a user
    function whose name ends in [_lp]. {!Factor_graph} links each factor to the
    data and parameters that the factor uses. A path is a chain of these links
    from factor to variable to factor. A factor is a prior of a parameter if the
    factor links to the parameter and does not have a path to data without the
    parameter. An unused parameter is a parameter that no factor links to. The
    factor graph finds unused parameters and priors. A read of a variable is
    uninitialized if the program can reach the read without running an
    assignment to the variable. {!Uninitialized_variables} finds these reads.
    For [real x; print(x);], the message is
    [The variable x may not have been assigned a value before its first use.].
    Other checks inspect parameter constraints and statement patterns.

    {1 Steps of the analysis}

    Some checks look only at literal numbers, such as the checks for constraint
    bounds and distribution arguments. Three simplifications turn more values
    into literals. Constant propagation replaces a variable with the constant
    value of the variable. After [b = a], copy propagation replaces later reads
    of [b] with [a]. Partial evaluation computes expressions whose operands are
    constants. After these simplifications, a local variable with a constant
    value becomes a literal, so the checks for literals also see the value.

    {!warn_pedantic} has three steps.
    + Simplify: run constant propagation, copy propagation, and partial
      evaluation on the program.
    + Check: run each check on the simplified program.
    + Join: join the warnings of all checks and sort the warnings.

    The following statement shows step 2 for the control-flow check. In this
    statement, [a] is a parameter.
    {@stan[
      if (a > 0) target += 1;
    ]}
    The check collects the names that the [if] and the dependencies of the [if]
    read. The check keeps the parameter names among these names, and gives the
    following evidence.
    {@text[
      read names:      {a}
      parameter names: {a}
      evidence:        (location of the if, {a})
    ]}
    The message for the example is
    [A control flow statement depends on parameter(s): a.]. Step 3 joins this
    warning with the warnings of the other checks and sorts the result. *)

(** {1 API reference} *)

(** A source location and a warning message. The location is the flagged code,
    such as a parameter-dependent [if] or an uninitialized read. *)
type warning_span = Location_span.t * string

val warn_pedantic : Program.Typed.t -> warning_span list
(** Runs the three steps of the overview and returns the warnings of all checks,
    sorted by location and message. *)

val warn_uninitialized : Program.Typed.t -> warning_span list
(** Returns the warnings for uninitialized reads, sorted, with one warning for
    each variable name. Reads without a source location are dropped. *)
