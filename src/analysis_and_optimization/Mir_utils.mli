open Std
open Middle
open Dataflow_types

(** Utilities to inspect and rewrite MIR, the middle intermediate representation
    of a Stan program. The docs below use the following terms.

    - The log density of a model is the sum that [target] accumulates. A call
      increments the target if the call can add to this sum. A user function
      whose name ends in [_lp], or a user or Stan Math function whose name ends
      in [_jacobian], can do so. In [Fun_kind.t], these functions have the
      suffix [FnTarget] or [FnJacobian].
    - The destination of an assignment is the left side, such as [y[k]] in
      [y[k] = v[j]]. The destination indices are the indices in the destination,
      such as [k].
    - The input expressions of a statement are the expressions that the
      statement itself evaluates. A [for] gives the loop bounds, and an [if] or
      [while] gives the condition. A call gives the arguments and the
      expressions in the [Fun_kind.t] of the call. For [y[k] = v[j]], the input
      expression is [v[j]]. Nested statements, declarations, and destination
      indices are not input expressions.
    - The child statements of a statement are the statements directly inside the
      statement, such as the body of a [for]. A label map maps an integer label
      to a statement whose child statements are replaced by labels
      ([Stmt.Located.Non_recursive.t]).
    - The autodiff level of an expression records whether the value needs
      automatic differentiation (autodiffable) or is data-only. *)

val expr_any : (ExprSet.elt -> bool) -> ExprSet.elt -> bool
(** [expr_any p e] is true if [p] holds for a subexpression of [e], indices
    included. [p] does not test an indexed expression as a whole. *)

val idx_any : (ExprSet.elt -> bool) -> ExprSet.elt Index.t -> bool
(** Applies {!expr_any} to each expression in an index. *)

val is_target_increment : 'e Fun_kind.t -> bool
(** True if a call to a function of this kind increments the target. The Stan
    Math call [target()] only reads [target] and gives false. *)

val cannot_duplicate_expr : ?preserve_stability:bool -> ExprSet.elt -> bool
(** True if two evaluations can differ, as with side effects, random draws, or
    [target()]. [preserve_stability] adds autodiffable expressions. *)

val cannot_remove_expr : ExprSet.elt -> bool
(** True if evaluation of the expression can have a side effect. A read of
    [target()] does not have a side effect. *)

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
(** Folds [~f] over the child statements in order, mapping ['a] to ['c]. The
    then-branch state of an [if] goes into the else branch. *)

val var_name_of_expr_exn : Expr.Typed.t -> string
(** Returns the name of a [Var] expression. Any other expression, for example
    [v[n]], raises an internal compiler error. *)

val expr_var_set : Expr.Typed.t -> (string * Expr.Typed.Meta.t) Set.Poly.t
(** Collects [(name, metadata)] for each variable in an expression, including
    variables in indices. For [v[k]], the result has pairs for [v] and [k]. *)

val expr_names : Expr.Typed.t -> string Set.Poly.t
(** The names from {!expr_var_set}. For [v[k] + v[j]], the result is
    [{"v"; "k"; "j"}]. *)

val stmt_rhs : (Expr.Typed.t, 's) Stmt.Pattern.t -> Expr.Typed.t Set.Poly.t
(** Returns the input expressions of the statement, as defined in the overview.
    For [y[k] = v[j]], the result is [v[j]]. *)

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
(** Replaces variables with the expressions in the map. The map does not apply
    to inserted expressions. *)

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
(** Renames variables in all expressions of the statement. Declared names and
    destination names do not change. *)

val expr_subst_expr : Expr.Typed.t ExprMap.t -> Expr.Typed.t -> Expr.Typed.t
(** Replaces subexpressions with the expressions in the map, trying larger
    subexpressions first. The map does not apply to inserted expressions. *)

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
(** Recomputes autodiff levels bottom up. Variables outside the set become
    data-only. [~keep_promotions:true] leaves [Promotion] nodes unchanged. *)

val cleanup_empty_stmts : ('e, 's) Stmt.t list -> ('e, 's) Stmt.t list
(** Removes skips, empty blocks and loops with empty bodies. Flattens blocks
    that have no declarations. Profile blocks stay. *)

val unsafe_unsized_to_sized_type :
  Expr.Typed.t Type.t -> Expr.Typed.t SizedType.t
(** Converts a type to a sized type with placeholder size zero in each unsized
    dimension. A function type gives the return type of the function. *)
