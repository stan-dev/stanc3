open Std
open Middle
open Dataflow_types

(** Utilities for inspecting and rewriting MIR. Dependency and initialization
    analyses use the read collectors below to find variable names and read
    locations. Access analysis separately preserves selected elements and
    destination-index reads. *)

val expr_any : (ExprSet.elt -> bool) -> ExprSet.elt -> bool
(** Searches an expression for a node satisfying the predicate, including nodes
    in index expressions. An indexed expression is searched through its base and
    indices rather than tested as a whole node. A predicate matching [k]
    therefore returns true for [v[k]]. *)

val idx_any : (ExprSet.elt -> bool) -> ExprSet.elt Index.t -> bool
(** Searches the expressions contained in an index. For a slice [a:b], tests
    expressions under both [a] and [b]; [:] contains no expressions. *)

val increments_target : 'e Fun_kind.t -> bool
(** Returns true for user functions classified as [FnTarget] or [FnJacobian] and
    Stan Math functions classified as [FnJacobian]. Access collection uses the
    result to add an [Increment] record; reaching definitions use the result to
    record an update to [target].

    A user call [foo_lp(y)] returns true. The Stan Math call [target()] returns
    false because the call reads the accumulator without updating it. *)

val cannot_duplicate_expr : ?preserve_stability:bool -> ExprSet.elt -> bool
(** Tests whether duplicating evaluation can change behavior. Side effects,
    random draws and reads of [target()] prevent duplication.
    [preserve_stability] also prevents duplicating autodiffable expressions. *)

val cannot_remove_expr : ExprSet.elt -> bool
(** Tests whether evaluation can have side effects. Reading [target()] alone has
    no side effect, although {!cannot_duplicate_expr} prevents duplication. *)

val var_declarations : ('a, 'b) Stmt.t -> string Set.Poly.t
val num_expr_value : Expr.Typed.t -> (float * string) option

type bound_values =
  { lower: [`None | `Nonlit | `Lit of float]
  ; upper: [`None | `Nonlit | `Lit of float] }

val trans_bounds_values : Expr.Typed.t Transformation.t -> bound_values
val chop_dist_name : string -> string Option.t
val top_var_declarations : Stmt.Located.t -> string Set.Poly.t
val function_names : Program.Typed.t -> string Set.Poly.t

val data_set :
     ?exclude_transformed:bool
  -> ?exclude_ints:bool
  -> Program.Typed.t
  -> string Set.Poly.t

val parameter_set :
     ?include_transformed:bool
  -> Program.Typed.t
  -> (string * Expr.Typed.t Transformation.t * Location_span.t) Set.Poly.t

val parameter_names_set :
  ?include_transformed:bool -> Program.Typed.t -> string Set.Poly.t

val fold_expr :
  take_expr:('c -> Expr.Typed.t -> 'c) -> init:'c -> Expr.Typed.t -> 'c

val fold_stmts :
     take_expr:('c -> Expr.Typed.t -> 'c)
  -> take_stmt:('c -> Stmt.Located.t -> 'c)
  -> init:'c
  -> Stmt.Located.t List.t
  -> 'c

val map_rec_expr :
     (Expr.Typed.t Expr.Pattern.t -> Expr.Typed.t Expr.Pattern.t)
  -> Expr.Typed.t
  -> Expr.Typed.t

val map_rec_expr_state :
     ('s -> Expr.Typed.t Expr.Pattern.t -> Expr.Typed.t Expr.Pattern.t * 's)
  -> 's
  -> Expr.Typed.t
  -> Expr.Typed.t * 's

val map_rec_stmt_loc :
     (   (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t)
  -> Stmt.Located.t
  -> Stmt.Located.t

val top_down_map_rec_stmt_loc :
     (   (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t)
  -> Stmt.Located.t
  -> Stmt.Located.t

val map_rec_state_stmt_loc :
     (   's
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t * 's)
  -> 's
  -> Stmt.Located.t
  -> Stmt.Located.t * 's

val map_rec_stmt_loc_num :
     Stmt.Located.Non_recursive.t LabelMap.t
  -> (   int
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t)
  -> Stmt.Located.Non_recursive.t
  -> Stmt.Located.t

val stmt_loc_of_stmt_loc_num :
     Stmt.Located.Non_recursive.t LabelMap.t
  -> Stmt.Located.Non_recursive.t
  -> Stmt.Located.t

val statement_stmt_loc_of_statement_stmt_loc_num :
     Stmt.Located.Non_recursive.t LabelMap.t
  -> (Expr.Typed.t, int) Stmt.Pattern.t
  -> (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t

val unnumbered_prog_of_numbered_prog :
     Stmt.Located.Non_recursive.t LabelMap.t
  -> ('a -> 'b)
  -> (Stmt.Located.Non_recursive.t, 'a, 'c) Program.t
  -> (Stmt.Located.t, 'b, 'c) Program.t
(** Reconstructs located statements from the supplied label map and maps the
    other program components with the second argument. *)

(** {1 Statement traversal and read collection}

    Traversal can replace nested statements while carrying an accumulator. Read
    collection selects expression inputs and returns names or names with
    occurrence metadata. Statement collectors visit only the statement's own
    expressions; callers handle nested statements separately. *)

val fwd_traverse_statement :
     ('e, 'a) Stmt.Pattern.t
  -> init:'f
  -> f:('f -> 'a -> 'f * 'c)
  -> 'f * ('e, 'c) Stmt.Pattern.t
(** Applies [f] to immediate substatements in source order, carrying state of
    type ['f] and replacing each substatement of type ['a] with the returned
    value of type ['c]. Returns the final state and rebuilt statement pattern.

    An [if] visits the then branch followed by the else branch, passing the
    state from the first visit into the second. The traversal supplies a source
    order for tasks such as labeling; it does not model runtime branching. *)

val var_name_of_expr_exn : Expr.Typed.t -> string
(** Returns the name of a bare [Var] expression. [v] gives ["v"], while [v[n]]
    raises an internal compiler error because an indexed expression is not a
    bare variable. *)

val expr_var_set : Expr.Typed.t -> (string * Expr.Typed.Meta.t) Set.Poly.t
(** Collects [(name, metadata)] pairs for variable occurrences in an expression,
    including indices and tuple bases. Metadata retains each occurrence's source
    location. For [v[k]], the result contains pairs for the occurrences of [v]
    and [k]; the result does not encode the selected element. *)

val expr_var_names_set : Expr.Typed.t -> string Set.Poly.t
(** Collects variable names using {!expr_var_set}, discarding occurrence
    metadata. For [v[k] + v[j]], the result is [{"v"; "k"; "j"}]. *)

val stmt_rhs : (Expr.Typed.t, 's) Stmt.Pattern.t -> Expr.Typed.t Set.Poly.t
(** Selects the statement's input expressions. For [y[k] = v[j]], the result
    contains only [v[j]]. A [for] supplies its bounds; an [if] or [while]
    supplies its condition. Call statements supply argument expressions and
    expressions carried by the function kind.

    Nested statements, declaration expressions and assignment destination
    indices are excluded. The result contains expressions, not variable names;
    {!stmt_rhs_var_set} collects variable occurrences from those expressions. *)

val stmt_rhs_var_set :
  (Expr.Typed.t, 's) Stmt.Pattern.t -> (string * Expr.Typed.Meta.t) Set.Poly.t
(** Applies {!expr_var_set} to the expressions selected by {!stmt_rhs}. For
    [y[k] = v[j]], the result contains occurrence pairs for [v] and [j], with
    their metadata. Neither the destination [y] nor its index [k] is included.
    Uninitialized-variable analysis uses the locations to report reads. *)

val stmt_rhs_names_set : ('a Expr.t, 'b) Stmt.Pattern.t -> string Set.Poly.t
(** Returns the names in {!stmt_rhs_var_set} without metadata. For
    [y[k] = v[j]], the result is [{"v"; "j"}]. Destination-index reads require
    access collection rather than this query. *)

(** {1 Expression rewriting}

    The operations below split additive terms, substitute expressions, and
    rebuild expression metadata. Splitting terms supplies separate factors for
    factor-graph construction. *)

val summation_terms : Expr.Typed.t -> Expr.Typed.t list
(** Recursively splits binary [Plus] expressions, preserving term order. For
    [a + (b + c)], the result is [[a; b; c]]. A subtraction [a - b] remains one
    term. *)

val subst_expr : Expr.Typed.t String.Map.t -> Expr.Typed.t -> Expr.Typed.t
(** Replaces variable expressions using a name-to-expression map. With
    [x -> a + 1], expression [x * 2] becomes [(a + 1) * 2]. Replacement
    expressions are inserted without applying the map to their children. *)

val subst_stmt_base :
     Expr.Typed.t String.Map.t
  -> (Expr.Typed.t, 'a) Stmt.Pattern.t
  -> (Expr.Typed.t, 'a) Stmt.Pattern.t
(** Substitutes within the statement's own expression trees, including
    destination indices. Nested statement bodies are retained. With [k -> j] and
    [x -> b], [y[k] = x] becomes [y[j] = b]. *)

val subst_stmt : Expr.Typed.t String.Map.t -> Stmt.Located.t -> Stmt.Located.t
(** Applies {!subst_stmt_base} recursively to the statement and nested bodies.
    For [if (x > 0) y = x] and [x -> b], both the condition and assignment read
    [b]. Declaration and assignment destination names are retained. *)

val name_subst_stmt : string String.Map.t -> Stmt.Located.t -> Stmt.Located.t
(** Renames variable occurrences in expression trees throughout the statement,
    preserving occurrence metadata. With [x -> "b"], [y = x] becomes [y = b].
    Declaration names and destination base names are not renamed. *)

val expr_subst_expr : Expr.Typed.t ExprMap.t -> Expr.Typed.t -> Expr.Typed.t
(** Replaces expressions using an expression-to-expression map. Each node is
    looked up before its children, so a match for [a + b] replaces the whole sum
    before substitutions for [a] or [b] are considered. Replacement expressions
    are inserted without further substitution. *)

val expr_subst_stmt_base :
     Expr.Typed.t ExprMap.t
  -> (Expr.Typed.t, 'a) Stmt.Pattern.t
  -> (Expr.Typed.t, 'a) Stmt.Pattern.t
(** Applies {!expr_subst_expr} within the statement's own expressions, including
    destination indices. Nested statement bodies are retained. Mapping
    [a + b -> c] changes [y = a + b] to [y = c]. *)

val expr_depth : Expr.Typed.t -> int
(** Returns the maximum expression-tree depth. Variables and literals have depth
    [0]; each enclosing operation adds one. For [a + b * c], the result is [2].
*)

val update_expr_ad_levels :
  ?keep_promotions:bool -> string Set.Poly.t -> Expr.Typed.t -> Expr.Typed.t
(** Preserves variable autodiff metadata for names in the supplied set and marks
    other variable nodes as data-only. Recomputes parent metadata from the
    rewritten children. A name in the set retains its current level, so a
    data-only variable is not promoted merely because its name is present.

    For an autodiffable [x] and an empty set, the resulting variable is
    data-only. [keep_promotions] defaults to false; true preserves existing
    promotion nodes. *)

val cleanup_empty_stmts : ('e, 's) Stmt.t list -> ('e, 's) Stmt.t list
(** Removes skips, empty blocks and loops with empty bodies. Flattens blocks
    without declarations; preserves profiling blocks and declaration scope. *)

val unsafe_unsized_to_sized_type :
  Expr.Typed.t Type.t -> Expr.Typed.t SizedType.t
(** Preserves sized types and converts unsized types using zero dimensions.
    Function types are converted through their return type; void functions and
    math-library function types raise an internal error. The zero dimensions are
    placeholders for compiler analysis, not actual runtime sizes. *)
