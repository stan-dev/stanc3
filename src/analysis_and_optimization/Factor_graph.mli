open Std
open Middle
open Dataflow_types
open Dependence_analysis

(** {1 Purpose of the factor graph}

    A density term is one term that [target +=] adds to the log density. The
    factor graph links each density term to the data and parameters that it
    depends on. Pedantic mode uses the graph for two warnings. Both warnings use
    factors, which step 1 defines.

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

    A factor is a part of a statement that changes the log density. A density
    term adds to [target]. An [_lp] call can also add to [target]. A rejection
    that runs stops the evaluation, so the sampler does not accept the sample.
    Thus a factor is a density term, a rejection or an [_lp] call.

    A label is the integer that identifies a statement in the statement map of
    {!Dependence_analysis}. Each factor stores the label of its statement. The
    label keeps equal terms in different statements apart. The example has two
    factors.
    {@text[
      factor   term
      F1       normal_lpdf(centered | 0, 1)
      F2       normal_lpdf(mu | 0, 1)
    ]}

    {1 Step 2 of 4: find the inputs of each factor}

    The inputs of a factor are the data and parameters that its value depends
    on. A dependency of a statement is another statement that can supply a value
    that it reads, or that controls whether it runs. The second kind is a
    control dependency. {!Dependence_analysis} finds both kinds.

    To find the inputs, a traversal starts from the names that the term reads.
    It follows the dependencies of the statement and collects their reads. It
    does not follow data or parameter names, which are its blockers. The goal is
    the direct inputs of the factor. The definitions of a blocker lead to other
    names, which are not direct inputs of this factor. A write is not a
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
    factor and its inputs are neighbors. A path goes from neighbor to neighbor.
    The example has the following links.
    {@text[
      factor -> inputs        input -> factors
      F1 -> {y, mu}           y  -> {F1}
      F2 -> {mu}              mu -> {F1, F2}
    ]}

    {1 Step 4 of 4: find priors}

    A prior of a parameter is a factor that links to the parameter and has no
    path to data after the removal of the parameter. The removal is necessary,
    because every factor of [mu] links to [mu]. Without the removal, [F2]
    reaches [y] through [mu] and [F1], so [mu] has no prior. After the removal,
    a path can start only at the other inputs of the factor. For [mu], the test
    gives the following results.
    {@text[
      factor   inputs without mu   reaches y   prior of mu
      F1       {y}                 yes         no
      F2       {}                  no          yes
    ]}
    The priors of [mu] are [{F2}]. *)

(** {1 API reference} *)

(** A factor without its label. {!FactorMap} keys add the label. *)
type factor =
  | TargetTerm of Expr.Typed.t
      (** One additive term of a [target +=] or [jacobian +=] expression. *)
  | Reject
      (** A reject or fatal-error statement. Its inputs come from the statements
          that control whether it runs. *)
  | LPFunction of (string * Expr.Typed.t list)
      (** A call to an [_lp] function, stored as its name and arguments. *)
[@@deriving sexp_of]

module FactorMap : Map.S with type key = factor * label

(** The two link maps. [factor_map] maps each factor and label to its input
    names. [var_map] maps each input name to the factors that use it. In the
    overview, [F1] maps to [{"y"; "mu"}], and ["mu"] maps to [F1] and [F2]. *)
type factor_graph =
  { factor_map: string Set.Poly.t FactorMap.t
  ; var_map: (factor * label) Set.Poly.t String.Map.t }

(** {2 Factor extraction and input collection} *)

val extract_factors_statement : (Expr.Typed.t, 'a) Stmt.Pattern.t -> factor list
(** Returns the factors of [stmt] without visiting nested statements.
    [target += a + b] gives [TargetTerm a] and [TargetTerm b]. Other statements
    give no factors, unless they are rejections or [_lp] calls. *)

val extract_factors :
     ((Expr.Typed.t, label) Stmt.Pattern.t * 'a) LabelMap.t
  -> label
  -> (label * factor) list
(** Returns the factors of the statement at [label] and of all nested
    statements. Each factor is paired with the label of its statement. *)

val factor_rhs : factor -> string Set.Poly.t
(** Returns the names that a factor reads itself. In the overview, [F1] gives
    [{"centered"}] and [F2] gives [{"mu"}]. A rejection gives an empty set. Each
    [LPFunction] argument must be a bare variable, because
    {!Mir_utils.var_name_of_expr_exn} reads its name. *)

val factor_var_dependencies :
  dep_info_map -> string Set.Poly.t -> label * factor -> string Set.Poly.t
(** Returns the names that a factor reads, joined with the names read by its
    dependencies. The second argument holds the blockers. The result can still
    contain blocker names. In the overview, [F1] gives
    [{"centered"; "y"; "mu"}]. {!prog_factor_graph} then keeps only inputs. *)

(** {2 Graph construction} *)

val build_adjacency_maps :
  (label * factor * string Set.Poly.t) list -> factor_graph
(** Builds both link maps from [(label, factor, input_names)] triples. Equal
    keys join their sets. *)

val fg_remove_fac : factor * label -> factor_graph -> factor_graph
(** Removes [fac] from [factor_map]. The [var_map] entries keep [fac]. Thus a
    later lookup through [var_map] can still return [fac]. {!fg_reaches} then
    raises [Not_found], because [fac] has no [factor_map] entry. *)

val fg_remove_var : string -> factor_graph -> factor_graph
(** Removes [var] from the inputs of each factor and removes its [var_map]
    entry. In the overview, removing [mu] leaves [F1] with [{"y"}] and [F2] with
    no inputs. *)

val remove_touching : string Set.Poly.t -> factor_graph -> factor_graph
(** Removes the names in [vars], and removes their neighbor factors from
    [factor_map]. The [var_map] entries of other names can still hold the
    removed factors. For [vars = {"y"}], [var_map["mu"]] still holds [F1]. *)

val prog_factor_graph :
  ?exclude_data_facs:bool -> Program.Typed.t -> factor_graph
(** Builds the factor graph of [prog.log_prob]. The inputs are the data names,
    including transformed data, and the names in the parameters block. These
    inputs are the blockers. The input sets do not contain local names.

    [exclude_data_facs] is false by default. When it is true, {!remove_touching}
    removes the data names and their neighbor factors. The result then holds
    only the factors that use no data. No caller in the compiler sets this
    option. Pedantic mode needs the data factors, because the prior test in step
    4 looks for paths to data. *)

(** {2 Reachability and prior queries} *)

val fg_reaches : string Set.Poly.t -> string Set.Poly.t -> factor_graph -> bool
(** Returns true if a path goes from a name in [starts] to a name in [goals].
    The search expands each variable once. Each start name must have a [var_map]
    entry. In the overview, [{"mu"}] reaches [{"y"}] through [F1]. *)

val fg_factor_reaches :
  factor * label -> string Set.Poly.t -> factor_graph -> bool
(** Like {!fg_reaches}, but starts from the inputs of [start]. *)

val fg_factor_is_prior :
  string -> factor * label -> string Set.Poly.t -> factor_graph -> bool
(** Returns true if [fac] has no path to [data] after the removal of [var]. In
    the overview with [var = "mu"], [F1] gives false and [F2] gives true. *)

val fg_var_priors :
     string
  -> string Set.Poly.t
  -> factor_graph
  -> (factor * label) Set.Poly.t option
(** Returns [Some] set of the priors of [var]. The set is empty if no neighbor
    factor of [var] is a prior. Returns [None] if [var] has no [var_map] entry.
    In the overview, [mu] gives [Some {F2}]. *)

val list_priors :
     ?factor_graph:factor_graph option
  -> Program.Typed.t
  -> ((factor * label) Set.Poly.t option * Location_span.t) String.Map.t
(** Maps each parameter name to its priors and its declaration location. Uses
    [factor_graph] if given, and otherwise builds a graph from [mir]. The search
    removes scalar integer data from the graph, because these names are often
    sizes. Without the removal, a factor that uses a size such as [N] reaches
    data and fails the prior test in step 4. That result is wrong, because [N]
    gives only a size and no observed values. *)

(** {2 Graph display} *)

val string_of_factor : factor -> string
(** Returns the term in quotes, ["reject"] for a rejection, or the function name
    for an [LPFunction]. The name does not contain the label. *)

val factor_graph_to_dot : factor_graph -> string
(** Returns [factor_map] as Graphviz DOT text. Each factor is a box with an edge
    to each of its inputs. Nodes take names from {!string_of_factor}, so equal
    terms at different labels share one node. *)
