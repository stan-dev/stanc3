open Middle

(** {1 Purpose of pedantic analysis}

    Pedantic analysis turns suspicious model constructs into warnings with
    source locations. Statement dependencies identify parameter-dependent
    control flow. The factor graph supports unused-parameter and prior checks.
    Assignment tracking identifies reads that may occur before initialization.

    {1 Prepare evidence for warnings}

    The full pedantic check first propagates constants and copies and partially
    evaluates expressions. The simplified program supplies distribution
    information, a factor graph and statement dependencies for the checks. Other
    checks inspect parameter constraints and statement patterns.

    The following fragment shows the evidence used for a control-flow warning.
    Assume [a] is a parameter:
    {@stan[
      if (a > 0) target += 1;
    ]}
    The condition reads [a]. Matching the condition's collected read names
    against parameter names produces:
    {@text[
      collected read names: {a}
      parameter names:      {a}
      reported evidence:    (location of the if, {a})
    ]}

    {1 Format located warning results}

    Each check converts its evidence into a source location and message. Results
    from the checks are combined and sorted before returning to the caller. For
    the control-flow example, the warning entry is:
    {@text[
      location: location of the if
      message:  A control flow statement depends on parameter(s): a.
    ]}
    The caller displays the returned messages at their associated locations.

    Uninitialized-read warnings use a separate assignment analysis. For example,
    [real x; print(x);] reports [x] at the read in [print(x)]. Empty source
    locations are discarded, and repeated reads produce one warning per variable
    name. *)

(** {1 API reference}

    Both entry points return warnings for the caller to display. The full check
    gathers all pedantic warning categories; the initialization check selects
    only uninitialized-read warnings. *)

(** A warning's source location and formatted message. For a parameter-dependent
    [if], the location points to the control-flow statement. For an
    uninitialized variable, the location points to a reported read. *)
type warning_span = Location_span.t * string

val warn_pedantic : Program.Typed.t -> warning_span list
(** Returns all pedantic warnings for the program after constant propagation,
    copy propagation and partial evaluation. The result is sorted by location
    and message. Includes parameter-use, control-flow, prior, distribution,
    constraint and uninitialized-read checks. *)

val warn_uninitialized : Program.Typed.t -> warning_span list
(** Returns uninitialized-read warnings for the supplied program, sorted by
    location and message. Uses
    {!Uninitialized_variables.mir_uninitialized_variables}, discards empty
    locations, and keeps one warning per variable name. For [real x; print(x);],
    the result contains the read location and the message
    [The variable x may not have been assigned a value before its first use.].
*)
