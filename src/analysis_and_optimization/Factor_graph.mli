open Std
open Middle
open Dataflow_types
open Dependence_analysis

(** {1 Purpose of the factor graph}

    A density term is one term that [target +=] adds to the log density. A
    factor is a part of a statement that can change the log density, such as a
    density term. Step 1 lists all kinds of factor. The factor graph links each
    factor to the data and parameters that the factor depends on. Pedantic mode
    uses the graph for two warnings.

    - An unused parameter is a parameter that no factor uses. Thus the parameter
      does not change the density.
    - A prior of a parameter is a factor that uses the parameter but no data.
      Step 4 gives the exact test. Pedantic mode warns when a used parameter has
      no prior or more than one prior, because each case can be a modeling
      error.

    The graph comes from four steps. The sections below show each step for the
    following model.
    {@stan[
      data { real y; }
      parameters { real mu; }
      model {
        real centered = y - mu;
        target += normal_lpdf(centered | 0, 1);
        target += normal_lpdf(mu | 0, 1);
      }
    ]}

    {1 Step 1 of 4: extract factors}

    A factor has one of three kinds. A density term adds to [target]. An [_lp]
    function is a user function whose name ends in [_lp], and a call to such a
    function can also add to [target]. A rejection is a [reject] or
    [fatal_error] statement. A rejection that runs stops the evaluation, so the
    sampler does not accept the sample. Thus a factor is a density term, a
    rejection, or an [_lp] call.

    {!Dependence_analysis} gives each statement of the log density a label,
    which is an integer that identifies the statement. The statement map
    ({!Dependence_analysis.dep_info_map}) maps each label to the statement with
    that label. Each factor stores the label of the statement that contains the
    factor. The label keeps equal terms in different statements apart. The
    example has two factors.
    {@text[
      factor   term
      F1       normal_lpdf(centered | 0, 1)
      F2       normal_lpdf(mu | 0, 1)
    ]}

    {1 Step 2 of 4: find the inputs of each factor}

    The inputs of a factor are the data and parameters that the value of the
    factor depends on. The data names include transformed data, and the
    parameter names are the names in the parameters block. A dependency of a
    statement is another statement that the statement needs. A data dependency
    can supply a value that the statement reads. A control dependency, such as
    an enclosing [if], decides whether the statement runs.
    {!Dependence_analysis} finds both kinds.

    To find the inputs, a traversal starts from the names that the factor reads.
    The traversal follows the dependencies of the statement and collects the
    names that these dependencies read. The data and parameter names are
    blockers, and the traversal does not follow the definitions of a blocker.
    The goal is the direct inputs of the factor. The definitions of a blocker
    lead to other names, which are not direct inputs of this factor. An element
    is one entry of a variable, such as [v[1]] of a vector [v]. A write is not a
    dependency if a later write always replaces the same element. Thus a factor
    does not get the inputs of an overwritten value. For [F1], the assignment to
    [centered] reads [y] and [mu].
    {@text[
      factor   term reads   dependency reads   combined names
      F1       {centered}   {y, mu}            {centered, y, mu}
      F2       {mu}         {}                 {mu}
    ]}
    The graph keeps only data and parameter names. The inputs are [{y, mu}] for
    [F1] and [{mu}] for [F2].

    {1 Step 3 of 4: store links in both directions}

    The graph stores the inputs of each factor and the factors of each input. A
    factor and each input of the factor are neighbors. A path goes from neighbor
    to neighbor. The example has the following links.
    {@text[
      factor -> inputs        input -> factors
      F1 -> {y, mu}           y  -> {F1}
      F2 -> {mu}              mu -> {F1, F2}
    ]}

    {1 Step 4 of 4: find priors}

    A prior of a parameter is a factor that links to the parameter and does not
    have a path to data after the removal of the parameter. The removal is
    necessary, because every factor of [mu] links to [mu]. Without the removal,
    [F2] reaches [y] through [mu] and [F1], so [mu] has no prior. After the
    removal, a path can start only at the other inputs of the factor. For [mu],
    the test gives the following results.
    {@text[
      factor   inputs without mu   reaches y   prior of mu
      F1       {y}                 yes         no
      F2       {}                  no          yes
    ]}
    The priors of [mu] are [{F2}].

    {!list_priors} first removes scalar integer data from the graph, because
    these names are often sizes. Without the removal, a factor that uses a size
    such as [N] reaches data and fails the prior test. That result is wrong,
    because [N] gives only a size and does not give observed values. *)

(** {1 API reference} *)

(** A factor without the label of the statement. {!FactorMap} keys pair each
    factor with the label. *)
type factor =
  | TargetTerm of Expr.Typed.t
      (** One additive term of a [target +=] or [jacobian +=] expression. *)
  | Reject
      (** A [reject] or [fatal_error] statement. The inputs of a rejection come
          from the statements that control whether the rejection runs. *)
  | LPFunction of (string * Expr.Typed.t list)
      (** A call to an [_lp] function, stored as the function name and the
          arguments. *)
[@@deriving sexp_of]

module FactorMap : Map.S with type key = factor * label

(** The two link maps of step 3. [factor_map] maps each factor and label to the
    input names of the factor. [var_map] maps each input name to the factors
    that use the name. In the overview, [F1] maps to [{"y"; "mu"}], and ["mu"]
    maps to [F1] and [F2]. *)
type factor_graph =
  { factor_map: string Set.Poly.t FactorMap.t
  ; var_map: (factor * label) Set.Poly.t String.Map.t }

(** {2 Factor extraction and input collection} *)

val extract_factors_statement : (Expr.Typed.t, 'a) Stmt.Pattern.t -> factor list
(** Returns the factors of one statement, without nested statements. For
    example, [target += a + b] gives [TargetTerm a] and [TargetTerm b]. *)

val extract_factors :
     ((Expr.Typed.t, label) Stmt.Pattern.t * 'a) LabelMap.t
  -> label
  -> (label * factor) list
(** Returns the factors at the given label of the statement map and in nested
    statements, each paired with the label of the statement. *)

val factor_rhs : factor -> string Set.Poly.t
(** Returns the names that the factor itself reads. A rejection gives the empty
    set. Each [LPFunction] argument must be a bare variable. *)

val factor_var_dependencies :
  dep_info_map -> string Set.Poly.t -> label * factor -> string Set.Poly.t
(** Returns the names read by the factor and by the dependencies of the factor
    (step 2). The second argument holds the blockers, which the result keeps. *)

(** {2 Graph construction} *)

val build_adjacency_maps :
  (label * factor * string Set.Poly.t) list -> factor_graph
(** Builds both link maps from (label, factor, input names) triples. Sets under
    equal keys are joined. *)

val fg_remove_fac : factor * label -> factor_graph -> factor_graph
(** Removes the factor from [factor_map] only. If a later {!fg_reaches} finds
    the factor in [var_map], the search raises [Not_found]. *)

val fg_remove_var : string -> factor_graph -> factor_graph
(** Removes the given name from the inputs of each factor and from [var_map], as
    in step 4. *)

val remove_touching : string Set.Poly.t -> factor_graph -> factor_graph
(** Removes the given names, and applies {!fg_remove_fac} to the neighbor
    factors of these names. *)

val prog_factor_graph :
  ?exclude_data_facs:bool -> Program.Typed.t -> factor_graph
(** Builds the factor graph of the log density. With [~exclude_data_facs:true]
    (default false), {!remove_touching} then removes the data names. *)

(** {2 Reachability and prior queries} *)

val fg_reaches : string Set.Poly.t -> string Set.Poly.t -> factor_graph -> bool
(** Returns true if a path goes from a name in the first set to a name in the
    second set. Each name in the first set must have a [var_map] entry. *)

val fg_factor_reaches :
  factor * label -> string Set.Poly.t -> factor_graph -> bool
(** Like {!fg_reaches}, but starts from the inputs of the given factor. *)

val fg_factor_is_prior :
  string -> factor * label -> string Set.Poly.t -> factor_graph -> bool
(** Returns true if the factor passes the prior test of step 4 for the parameter
    (first argument), given the data names (third argument). *)

val fg_var_priors :
     string
  -> string Set.Poly.t
  -> factor_graph
  -> (factor * label) Set.Poly.t option
(** Returns [Some] set of the priors of the parameter, given the data names, or
    [None] if the parameter does not have a [var_map] entry. *)

val list_priors :
     ?factor_graph:factor_graph option
  -> Program.Typed.t
  -> ((factor * label) Set.Poly.t option * Location_span.t) String.Map.t
(** Maps each parameter name to the priors and the declaration location of the
    parameter. Builds the graph unless [~factor_graph] is given. *)

(** {2 Graph display} *)

val string_of_factor : factor -> string
(** Returns the term in quotes, ["reject"] for a rejection, or the function name
    for an [LPFunction]. The name does not contain the label. *)

val factor_graph_to_dot : factor_graph -> string
(** Returns [factor_map] as Graphviz DOT text, with a box for each factor. Equal
    terms at different labels share one node ({!string_of_factor}). *)
