open Core_kernel

(** Type to represent promotions in the typechecker.
  This can be used to return information about how to promote
  expressions for use in [Ast.Promotion]*)
type t =
  | NoPromotion
  | IntToRealPromotion
  | IntToComplexPromotion
  | RealToComplexPromotion

let promote (exp : Ast.typed_expression) prom =
  let open Middle.UnsizedType in
  let emeta = exp.emeta in
  match prom with
  | IntToRealPromotion when is_int_type emeta.type_ ->
      Ast.
        { expr= Ast.Promotion (exp, UReal, emeta.ad_level)
        ; emeta= {emeta with type_= promote_array emeta.type_ UReal} }
  | (IntToComplexPromotion | RealToComplexPromotion)
    when not (is_complex_type emeta.type_) ->
      { expr= Promotion (exp, UComplex, emeta.ad_level)
      ; emeta= {emeta with type_= promote_array emeta.type_ UComplex} }
  | _ -> exp

let promote_list es promotions = List.map2_exn es promotions ~f:promote

let promotion_cost p =
  match p with
  | NoPromotion -> 0
  | RealToComplexPromotion | IntToRealPromotion -> 1
  | IntToComplexPromotion -> 2
