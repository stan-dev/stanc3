open Core_kernel
open Middle
open Dataflow_types

val var_declarations : ('a, 'b) Stmt.Fixed.t -> string Set.Poly.t
val num_expr_value : Expr.Typed.t -> (float * string) option

type bound_values =
  { lower: [`None | `Nonlit | `Lit of float]
  ; upper: [`None | `Nonlit | `Lit of float] }

val trans_bounds_values : Expr.Typed.t Transformation.t -> bound_values
val chop_dist_name : string -> string Option.t
val top_var_declarations : Stmt.Located.t -> string Set.Poly.t

val data_set :
     ?exclude_transformed:bool
  -> ?exclude_ints:bool
  -> Program.Typed.t
  -> string Set.Poly.t

val parameter_set :
     ?include_transformed:bool
  -> Program.Typed.t
  -> (string * Expr.Typed.t Transformation.t) Set.Poly.t

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
     (Expr.Typed.t Expr.Fixed.Pattern.t -> Expr.Typed.t Expr.Fixed.Pattern.t)
  -> Expr.Typed.t
  -> Expr.Typed.t

val map_rec_expr_state :
     (   's
      -> Expr.Typed.t Expr.Fixed.Pattern.t
      -> Expr.Typed.t Expr.Fixed.Pattern.t * 's )
  -> 's
  -> Expr.Typed.t
  -> Expr.Typed.t * 's

val map_rec_stmt_loc :
     (   (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t )
  -> Stmt.Located.t
  -> Stmt.Located.t

val top_down_map_rec_stmt_loc :
     (   (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t )
  -> Stmt.Located.t
  -> Stmt.Located.t

val map_rec_state_stmt_loc :
     (   's
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t * 's )
  -> 's
  -> Stmt.Located.t
  -> Stmt.Located.t * 's

val map_rec_stmt_loc_num :
     (int, Stmt.Located.Non_recursive.t) Map.Poly.t
  -> (   int
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t )
  -> Stmt.Located.Non_recursive.t
  -> Stmt.Located.t

val map_rec_state_stmt_loc_num :
     (int, Stmt.Located.Non_recursive.t) Map.Poly.t
  -> (   int
      -> 's
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t * 's )
  -> 's
  -> Stmt.Located.Non_recursive.t
  -> Stmt.Located.t * 's

val stmt_loc_of_stmt_loc_num :
     (int, Stmt.Located.Non_recursive.t) Map.Poly.t
  -> Stmt.Located.Non_recursive.t
  -> Stmt.Located.t

val statement_stmt_loc_of_statement_stmt_loc_num :
     (int, Stmt.Located.Non_recursive.t) Map.Poly.t
  -> (Expr.Typed.t, int) Stmt.Fixed.Pattern.t
  -> (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t

val unnumbered_prog_of_numbered_prog :
     (int, Stmt.Located.Non_recursive.t) Map.Poly.t
  -> ('a -> 'b)
  -> (Stmt.Located.Non_recursive.t, 'a) Program.t
  -> (Stmt.Located.t, 'b) Program.t

val fwd_traverse_statement :
     ('e, 'a) Stmt.Fixed.Pattern.t
  -> init:'f
  -> f:('f -> 'a -> 'f * 'c)
  -> 'f * ('e, 'c) Stmt.Fixed.Pattern.t
(**
   A traversal that simultaneously accumulates a state (type 'f) and replaces the
   substatement values from ('a to 'c). Traversal is done in-order but ignores branching,
   e.g., and if's then block is followed by the else block rather than branching.
*)

val vexpr_of_expr_exn : Expr.Typed.t -> vexpr
(**
   Take a LHS expression from a general expression, throwing an exception if it can't be a
   LHS expression.
*)

val expr_var_set : Expr.Typed.t -> (vexpr * Expr.Typed.Meta.t) Set.Poly.t
(**
   The set of variables in an expression, including inside an index.
   For use in RHS sets, not LHS assignment sets, except in a target term.
*)

val index_var_set :
  Expr.Typed.t Index.t -> (vexpr * Expr.Typed.Meta.t) Set.Poly.t
(**
   The set of variables in an index.
   For use in RHS sets, not LHS assignment sets, except in a target term
*)

val expr_var_names_set : Expr.Typed.t -> string Core_kernel.Set.Poly.t
(** 
   Return the names of the variables in an expression.
*)

val stmt_rhs :
  (Expr.Typed.t, 's) Stmt.Fixed.Pattern.t -> Expr.Typed.t Set.Poly.t
(**
   The set of variables that can affect the value or behavior of the expression, i.e. rhs.
   Using Set.Poly instead of ExprSet so that 'e can be polymorphic, it usually doesn't
   matter if there's duplication.
*)

val union_map : 'a Set.Poly.t -> f:('a -> 'b Set.Poly.t) -> 'b Set.Poly.t
(**
   This is a helper function equivalent to List.concat_map but for Sets
*)

val stmt_rhs_var_set :
     (Expr.Typed.t, 's) Stmt.Fixed.Pattern.t
  -> (vexpr * Expr.Typed.Meta.t) Set.Poly.t
(**
   The set of variables in an expression, including inside an index.
   For use in RHS sets, not LHS assignment sets, except in a target term.
*)

val expr_assigned_var : Expr.Typed.t -> vexpr
(**
   The variable being assigned to when the expression is the LHS
*)

val summation_terms : Expr.Typed.t -> Expr.Typed.t list
(** The list of terms in expression separated by a + *)

val stmt_of_block : Stmt.Located.t list -> Stmt.Located.t
(** Represent a list of statements as a single statement *)

val subst_expr :
  (string, Expr.Typed.t) Map.Poly.t -> Expr.Typed.t -> Expr.Typed.t
(** Substitute variables in an expression according to the provided Map. *)

val subst_stmt_base :
     (string, Expr.Typed.t) Map.Poly.t
  -> (Expr.Typed.t, 'a) Stmt.Fixed.Pattern.t
  -> (Expr.Typed.t, 'a) Stmt.Fixed.Pattern.t
(** Substitute variables occurring at the top level in statements according to the provided Map. *)

val subst_stmt :
  (string, Expr.Typed.t) Map.Poly.t -> Stmt.Located.t -> Stmt.Located.t
(** Substitute variables occurring anywhere in a statement according to the provided Map. *)

val name_subst_stmt :
  (string, string) Map.Poly.t -> Stmt.Located.t -> Stmt.Located.t
(** Substitute subexpressions occurring anywhere in a statement according to the provided Map. *)

val expr_subst_expr :
  Expr.Typed.t Expr.Typed.Map.t -> Expr.Typed.t -> Expr.Typed.t
(** Substitute subexpressions in an expression according to the provided Map, trying
    to match on larger subexpressions before smaller ones. *)

val expr_subst_stmt :
  Expr.Typed.t Expr.Typed.Map.t -> Stmt.Located.t -> Stmt.Located.t
(** Substitute subexpressions occurring anywhere in a statement according to the provided Map. *)

val expr_subst_stmt_base :
     Expr.Typed.t Expr.Typed.Map.t
  -> (Expr.Typed.t, 'a) Stmt.Fixed.Pattern.t
  -> (Expr.Typed.t, 'a) Stmt.Fixed.Pattern.t
(** Substitute subexpressions occurring at the top level in statements according to the provided Map. *)

val expr_depth : Expr.Typed.t -> int
(** Calculate how deeply nested an expression is. *)

val update_expr_ad_levels : string Set.Poly.t -> Expr.Typed.t -> Expr.Typed.t
(** Recompute all AD-levels in the metadata of an expression from the bottom up, making the variables
    in the first argument autodiffable *)

val cleanup_empty_stmts :
  ('e, 's) Stmt.Fixed.t list -> ('e, 's) Stmt.Fixed.t list
