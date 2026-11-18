open Std
open Middle
open Dataflow_types

type t

val create :
     ((Expr.Typed.t, label) Stmt.Pattern.t * 'meta) LabelMap.t
  -> controls LabelMap.t
  -> string Set.Poly.t
  -> t
(** The set includes every variable written in the region, including loop
    variables. *)

val may_supply_read :
  t -> label -> Dependence.location -> label -> Dependence.location -> bool
(** Writer then reader; unknown expressions retain possible dependences. *)

val is_overwritten :
  t -> label -> Dependence.location -> label -> Dependence.location -> bool
(** Previous writer then overwriting writer; loop-varying paths require an
    unavoidable overwrite in the same iteration. *)
