open Std
open Middle
open Dataflow_types

(** Utilities to inspect and rewrite MIR. *)

val expr_any : (ExprSet.elt -> bool) -> ExprSet.elt -> bool
(** [expr_any p e] is true if [p] is true for a subexpression of [e], including
    subexpressions in indices. For an indexed expression, [p] tests the base and
    the indices, but not the whole indexed expression. *)

val idx_any : (ExprSet.elt -> bool) -> ExprSet.elt Index.t -> bool
(** Applies {!expr_any} to each expression in an index. *)

val increments_target : 'e Fun_kind.t -> bool
(** True if a call to the function can add to the log density [target]. This is
    true for a user function of kind [FnTarget] or [FnJacobian], and for a Stan
    Math function of kind [FnJacobian]. The Stan Math call [target()] reads
    [target] but does not change it, so the result is false. *)

val cannot_duplicate_expr : ?preserve_stability:bool -> ExprSet.elt -> bool
(** True if two evaluations of the expression can give different behavior. Side
    effects, random draws and reads of [target()] cause this. With
    [preserve_stability], autodiffable expressions also cause this. *)

val cannot_remove_expr : ExprSet.elt -> bool
(** True if evaluation of the expression can have a side effect. A read of
    [target()] has no side effect. *)

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
(** Rebuilds recursive statements from the label map. The second argument maps
    the other parts of the program. *)

(** {1 Statement traversal and read collection} *)

val fwd_traverse_statement :
     ('e, 'a) Stmt.Pattern.t
  -> init:'f
  -> f:('f -> 'a -> 'f * 'c)
  -> 'f * ('e, 'c) Stmt.Pattern.t
(** Applies [f] to each direct substatement in source order. [f] takes a state
    of type ['f] and replaces a substatement of type ['a] with a value of type
    ['c]. The result is the final state and the new statement. For an [if], the
    state from the then branch goes into the else branch. *)

val var_name_of_expr_exn : Expr.Typed.t -> string
(** Returns the name of a [Var] expression. Any other expression, for example
    [v[n]], raises an internal compiler error. *)

val expr_var_set : Expr.Typed.t -> (string * Expr.Typed.Meta.t) Set.Poly.t
(** Collects [(name, metadata)] for each variable in an expression, including
    variables in indices. For [v[k]], the result has pairs for [v] and [k]. *)

val expr_var_names_set : Expr.Typed.t -> string Set.Poly.t
(** The names from {!expr_var_set}. For [v[k] + v[j]], the result is
    [{"v"; "k"; "j"}]. *)

val stmt_rhs : (Expr.Typed.t, 's) Stmt.Pattern.t -> Expr.Typed.t Set.Poly.t
(** Returns the input expressions of the statement itself. For [y[k] = v[j]],
    the result is [v[j]]. A [for] gives its bounds, and an [if] or [while] gives
    its condition. A call gives its arguments and the expressions in its
    [Fun_kind.t]. Nested statements, declarations and destination indices are
    not in the result. *)

val stmt_rhs_var_set :
  (Expr.Typed.t, 's) Stmt.Pattern.t -> (string * Expr.Typed.Meta.t) Set.Poly.t
(** Applies {!expr_var_set} to the result of {!stmt_rhs}. For [y[k] = v[j]], the
    result has pairs for [v] and [j], but not for [y] or [k]. *)

val stmt_rhs_names_set : ('a Expr.t, 'b) Stmt.Pattern.t -> string Set.Poly.t
(** The names from {!stmt_rhs_var_set}. For [y[k] = v[j]], the result is
    [{"v"; "j"}]. *)

(** {1 Expression rewriting} *)

val summation_terms : Expr.Typed.t -> Expr.Typed.t list
(** Splits nested [+] expressions into a list of terms, in order. For
    [a + (b + c)], the result is [[a; b; c]]. [a - b] is one term. *)

val subst_expr : Expr.Typed.t String.Map.t -> Expr.Typed.t -> Expr.Typed.t
(** Replaces variables with the expressions in the map. With [x -> a + 1],
    [x * 2] becomes [(a + 1) * 2]. The map does not apply to inserted
    expressions. *)

val subst_stmt_base :
     Expr.Typed.t String.Map.t
  -> (Expr.Typed.t, 'a) Stmt.Pattern.t
  -> (Expr.Typed.t, 'a) Stmt.Pattern.t
(** Applies {!subst_expr} to the expressions of the statement itself, including
    destination indices. Nested statements do not change. *)

val subst_stmt : Expr.Typed.t String.Map.t -> Stmt.Located.t -> Stmt.Located.t
(** Applies {!subst_stmt_base} to the statement and to all nested statements.
    Declared names and destination names do not change. *)

val name_subst_stmt : string String.Map.t -> Stmt.Located.t -> Stmt.Located.t
(** Renames variables in all expressions of the statement. With [x -> "b"],
    [y = x] becomes [y = b]. Declared names and destination names do not change.
*)

val expr_subst_expr : Expr.Typed.t ExprMap.t -> Expr.Typed.t -> Expr.Typed.t
(** Replaces subexpressions with the expressions in the map. A node is looked up
    before its children, so a match for [a + b] replaces the whole sum. The map
    does not apply to inserted expressions. *)

val expr_subst_stmt_base :
     Expr.Typed.t ExprMap.t
  -> (Expr.Typed.t, 'a) Stmt.Pattern.t
  -> (Expr.Typed.t, 'a) Stmt.Pattern.t
(** Applies {!expr_subst_expr} to the expressions of the statement itself,
    including destination indices. Nested statements do not change. *)

val expr_depth : Expr.Typed.t -> int
(** The depth of the expression tree. A variable or literal has depth [0]. For
    [a + b * c], the depth is [2]. *)

val update_expr_ad_levels :
  ?keep_promotions:bool -> string Set.Poly.t -> Expr.Typed.t -> Expr.Typed.t
(** Recomputes the autodiff levels in an expression from the bottom up. A
    variable whose name is not in the set becomes data-only. A variable whose
    name is in the set keeps its level. With [keep_promotions] (default false),
    the promotion nodes do not change. *)

val cleanup_empty_stmts : ('e, 's) Stmt.t list -> ('e, 's) Stmt.t list
(** Removes skips, empty blocks and loops with empty bodies. Flattens blocks
    that have no declarations. Profile blocks stay. *)

val unsafe_unsized_to_sized_type :
  Expr.Typed.t Type.t -> Expr.Typed.t SizedType.t
(** Converts a type to a sized type. Each unsized dimension gets size zero. This
    size is a placeholder and not a runtime size. A function type converts to
    its return type. A void or math-library function raises an internal error.
*)
