open Core_kernel
open Middle

val json_to_mir :
     (Expr.Typed.t SizedType.t * 'a * string) list
  -> Yojson.Basic.t
  -> (string, Expr.Typed.t) Map.Poly.t

val gen_values_json :
     ?filter:bool
  -> ?data:(string, Expr.Typed.t) Map.Poly.t
  -> (Expr.Typed.t SizedType.t * Expr.Typed.t Transformation.t * string) list
  -> string
