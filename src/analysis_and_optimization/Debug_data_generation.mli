open Std
open Middle

val json_to_mir :
     (Expr.Typed.t SizedType.t * 'a * string) list
  -> Yojson.Basic.t
  -> Expr.Typed.t String.Map.t
(** Translates Yojson object into a data type that `gen_values_json`
    understands. *)

val gen_values_json :
     ?new_only:bool
  -> ?context:Expr.Typed.t String.Map.t
  -> (Expr.Typed.t SizedType.t * Expr.Typed.t Transformation.t * string) list
  -> (string, Frontend.Errors.t) result
(** Generates values matching the given declarations and formats them as a JSON
    string. The declarations may depend on additional values supplied in
    `context`. If `new_only` is true (defaults to false) the output does not
    include the values in `context` *)
