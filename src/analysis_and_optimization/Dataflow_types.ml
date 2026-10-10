open Std
open Std.Sexp_conv

(** Statement identifier, unique within a map or graph. Labels from separate
    maps are not interchangeable. *)
type label = int [@@deriving sexp_of]

(** Candidate value source by variable name: [Some label] identifies a
    statement; [None] identifies an external definition. Element checks use
    access records. *)
type reaching_defn = string * label option [@@deriving sexp_of]

module LabelMap = struct
  include Int.Map

  let sexp_of_t f t = sexp_of_list (sexp_of_pair sexp_of_int f) (to_list t)
end

module ExprSet = Set.Make (Middle.Expr.Typed)
module ExprMap = Map.Make (Middle.Expr.Typed)
