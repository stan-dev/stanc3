open Std
open Middle
open Dataflow_types
open Dependence_analysis

(** {1 Purpose of the factor graph}

    The factor graph connects density terms to the data and parameters that
    affect the terms. Pedantic mode uses the connections to find unused
    parameters and terms that may act as priors.

    The following example shows how a local computation connects a density term
    to program inputs. The sections below trace the terms and classify a
    possible prior:
    {@stan[
      data { real y; }
      parameters { real mu; }
      model {
        real centered = y - mu;
        target += normal_lpdf(centered | 0, 1);
        target += normal_lpdf(mu | 0, 1);
      }
    ]}

    {1 Extract density terms}

    Each density term becomes a factor associated with its statement. Separate
    factors let the graph distinguish terms with different input dependencies.
    The example produces:
    {@text[
      factor   term
      F1       normal_lpdf(centered | 0, 1)
      F2       normal_lpdf(mu | 0, 1)
    ]}

    {1 Follow dependencies to program inputs}

    For each factor, traversal starts from the term's read names and controlling
    statements. Traversal stops at data and parameters. The term's own reads are
    combined with reads from visited dependencies because the starting statement
    can be absent from the traversal result.

    For the example, following the assignment to [centered] produces:
    {@text[
      factor   term reads   dependency reads   combined names
      F1       {centered}   {y, mu}            {centered, y, mu}
      F2       {mu}         {}                 {mu}
    ]}
    Keeping only data and parameter names removes [centered], yielding inputs
    [{y, mu}] for [F1] and [{mu}] for [F2].

    {1 Store connections in both directions}

    Construction stores inputs by factor and factors by input name. The first
    direction supplies a factor's neighbors; the reverse direction supplies a
    variable's neighbors. For the example, the completed maps contain:
    {@text[
      factor -> inputs        input -> factors
      F1 -> {y, mu}           y  -> {F1}
      F2 -> {mu}              mu -> {F1, F2}
    ]}

    {1 Find possible priors}

    A factor touching a parameter is a possible prior when removing that
    parameter leaves no path from the factor to data. Removing the parameter
    prevents its other density terms from creating a path to data.

    For [mu], the example's checks produce:
    {@text[
      factor   inputs after removing mu   reaches data y   possible prior
      F1       {y}                        yes              no
      F2       {}                         no               yes
    ]}
    The result is the possible-prior set [{F2}] for [mu]. Graph connectivity
    supplies a heuristic classification rather than a proof about the density.
*)

(** {1 API reference}

    The declarations below describe factor identity, adjacency maps, graph
    construction and reachability queries. *)

(** A density contribution or statement that affects whether evaluation
    continues. Each factor is paired with a statement label in {!FactorMap}, so
    identical expressions in different statements remain distinct. *)
type factor =
  | TargetTerm of Expr.Typed.t
      (** One additive term from a [target +=] or [jacobian +=] expression. *)
  | Reject
      (** A reject or fatal-error statement. Dependencies come from the
          statements controlling whether evaluation reaches the rejection. *)
  | LPFunction of (string * Expr.Typed.t list)
      (** A call classified as [FnTarget], stored as its name and arguments. *)
[@@deriving sexp_of]

module FactorMap : Map.S with type key = factor * label

(** Adjacency maps produced by graph construction. [factor_map] maps each
    factor/label pair to input names; [var_map] maps each input name to the
    factors using that input. The example's [F1] maps to [{"y"; "mu"}], and
    ["mu"] maps to both [F1] and [F2]. *)
type factor_graph =
  { factor_map: string Set.Poly.t FactorMap.t
  ; var_map: (factor * label) Set.Poly.t String.Map.t }

(** {2 Factor extraction and input collection}

    Extraction selects density terms and rejection statements. Input collection
    follows their statement dependencies and retains the factor's own reads. *)

val extract_factors_statement : (Expr.Typed.t, 'a) Stmt.Pattern.t -> factor list
(** Extracts factors from [stmt] without visiting nested statements. An update
    [target += a + b] gives [TargetTerm a] and [TargetTerm b]. Other statements
    give no factors unless they are rejections or [FnTarget] calls. *)

val extract_factors :
     ((Expr.Typed.t, label) Stmt.Pattern.t * 'a) LabelMap.t
  -> label
  -> (label * factor) list
(** Collects factors from the statement at [label] and all nested statements.
    Results pair each factor with its containing statement's label. *)

val factor_rhs : factor -> string Set.Poly.t
(** Returns the factor's own read names before dependency traversal. The
    overview's first term gives [{"centered"}]; the second gives [{"mu"}].
    Rejection factors give an empty set. [LPFunction] arguments must be bare
    variable expressions because {!Mir_utils.var_name_of_expr_exn} extracts
    their names. *)

val factor_var_dependencies :
  dep_info_map -> string Set.Poly.t -> label * factor -> string Set.Poly.t
(** Combines a factor's own reads with reads at dependency labels. [blockers]
    stops traversal at program inputs but does not remove their names from the
    combined result. For the overview's [F1], the result is
    [{"centered"; "y"; "mu"}]; graph construction later selects only inputs. *)

(** {2 Graph construction}

    Construction converts factor/input triples into adjacency maps. Removal
    helpers update selected entries for data exclusion and prior queries. *)

val build_adjacency_maps :
  (label * factor * string Set.Poly.t) list -> factor_graph
(** Builds both adjacency directions from [(label, factor, input_names)]
    triples. Repeated keys union their sets. Two factors reading [mu] produce
    one [var_map] entry for [mu] containing both factor/label pairs. *)

val fg_remove_fac : factor * label -> factor_graph -> factor_graph
(** Removes [fac] from [factor_map]. The reverse [var_map] entries are retained.
*)

val fg_remove_var : string -> factor_graph -> factor_graph
(** Removes [var] from every factor's input set and removes the variable's
    [var_map] entry. Removing [mu] in the overview leaves [F1] with [{"y"}] and
    [F2] with an empty input set. *)

val remove_touching : string Set.Poly.t -> factor_graph -> factor_graph
(** Removes names in [vars] and removes directly touching factors from
    [factor_map]. Other variable entries retain their factor sets, including
    references to removed factors. For the overview with [vars = {"y"}],
    [factor_map] retains only [F2], while [var_map["mu"]] still lists both
    factors. *)

val prog_factor_graph :
  ?exclude_data_facs:bool -> Program.Typed.t -> factor_graph
(** Builds adjacency maps for factors in [prog.log_prob]. Keeps data names,
    including transformed data, and parameters from the parameters block.
    Dependency traversal stops at the input names; local names are removed from
    the resulting input sets.

    [exclude_data_facs] defaults to false. When true, {!remove_touching} removes
    data variables and directly touching [factor_map] entries. Remaining
    [var_map] entries can retain references to the removed factors. *)

(** {2 Reachability and prior queries}

    Reachability alternates variable-to-factor and factor-to-variable links.
    Prior queries remove a parameter's links before looking for paths to data.
*)

val fg_reaches : string Set.Poly.t -> string Set.Poly.t -> factor_graph -> bool
(** Searches from variable names in [starts] for a connection to [goals]. Visits
    each expanded variable once and returns true when an expansion reaches a
    goal. Starting names must have [var_map] entries. In the overview graph,
    starting at [{"mu"}] reaches [{"y"}] through [F1]. *)

val fg_factor_reaches :
  factor * label -> string Set.Poly.t -> factor_graph -> bool
(** Starts reachability from the inputs of [start]. In the overview, [F1]
    reaches [{"y"}]. After removing [mu], [F2] has no inputs and returns false.
*)

val fg_factor_is_prior :
  string -> factor * label -> string Set.Poly.t -> factor_graph -> bool
(** Tests whether [fac] has no path to [data] after removing [var]. For the
    overview's [mu], the result is false for [F1] and true for [F2]. *)

val fg_var_priors :
     string
  -> string Set.Poly.t
  -> factor_graph
  -> (factor * label) Set.Poly.t option
(** Returns [Some] with the touching factors classified as possible priors.
    Returns [None] when [var] has no [var_map] entry. An existing variable with
    no qualifying factor gives [Some] with an empty set. For [mu] in the
    overview, the result is [Some {F2}]. *)

val list_priors :
     ?factor_graph:factor_graph option
  -> Program.Typed.t
  -> ((factor * label) Set.Poly.t option * Location_span.t) String.Map.t
(** Maps each parameter name to its possible-prior set and declaration location.
    Uses the supplied graph or constructs one from [mir]. Scalar integer data
    names are removed from reachability checks because they often represent
    sizes. For the overview, [mu] maps to [Some {F2}] and its declaration span.
*)

(** {2 Graph display}

    Display names describe factor expressions. DOT output draws factors as boxes
    and connects each factor to the input names in [factor_map]. *)

val string_of_factor : factor -> string
(** Returns the density expression in quotes, ["reject"] for a rejection, or the
    function name for a call factor. Display names omit statement labels. *)

val factor_graph_to_dot : factor_graph -> string
(** Emits Graphviz DOT text using the forward adjacency map. For the overview,
    edges connect [F1]'s expression to [y] and [mu], and [F2]'s expression to
    [mu]. The displayed factor names come from {!string_of_factor}, so identical
    names share a DOT node even when their statement labels differ. *)
