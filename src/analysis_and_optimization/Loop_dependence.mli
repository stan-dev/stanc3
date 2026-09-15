(** Data dependence analysis for one loop level (design
    [design-docs/active/vectorize-loop-fission.md] §7.3-7.5). The documentation
    of each function is on its definition. *)

open Std
open Middle
open Dataflow_types

(** {1 Loop access model and dependence test}

    Data dependence analysis for one loop level (Kennedy and Allen,
    {i Optimizing Compilers for Modern Architectures} ch. 2-3; Goff, Kennedy and
    Tseng, PLDI 1991). See [design-docs/active/vectorize-loop-fission.md]
    §7.3-7.4. The documentation of each function is on its definition. *)

val accesses_of_pattern :
     loopvar:string
  -> written_vars:string Set.Poly.t
  -> label:label
  -> sub:('s -> access list)
  -> (Expr.Typed.t, 's) Stmt.Pattern.t
  -> access list
(** The accesses of one statement pattern, with [sub] giving those of its
    substatements ([stmt_accesses] recurses; [Dependence_analysis] passes
    [fun _ -> []] because substatements are their own nodes). *)

val classify_subscript :
     loopvar:string
  -> written_vars:string Set.Poly.t
  -> Expr.Typed.t Index.t
  -> subscript
(** How one index position varies with the loop over [loopvar]. *)

val stmt_accesses :
     loopvar:string
  -> written_vars:string Set.Poly.t
  -> label:label
  -> (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t
  -> access list
(** Every read and write in a statement, in evaluation order. *)

val access_dependence : access -> access -> dependence
(** The dependence between two accesses to the same variable. *)

val pp_subscript : subscript Fmt.t
val pp_access : access Fmt.t
val pp_dependence : dependence Fmt.t

(** {1 Loop dependence graph and pi-blocks}

    §7.5 of the design. Documentation is on the definitions. *)

val loop_leaves : Stmt.Located.t -> Stmt.Located.t list
val stmt_has_effects : Stmt.Located.t -> bool

val loop_dependence_graph :
  loopvar:string -> Stmt.Located.t -> loop_dependence_graph

val pi_blocks : loop_dependence_graph -> int list list
val is_cyclic : loop_dependence_graph -> int list -> bool
val pp_stmt_one_line : Stmt.Located.t Fmt.t
val pp_loop_edge : loop_edge Fmt.t
val pp_edges : loop_dependence_graph Fmt.t
val pp_blocks : loop_dependence_graph -> int list list Fmt.t
val pp_loop_dependence_graph : loop_dependence_graph Fmt.t
