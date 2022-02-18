open Core_kernel
open Core_kernel.Poly
module UnsizedType = Middle.UnsizedType

(** Type to represent promotions in the typechecker.
  This can be used to return information about how to promote
  expressions for use in [Ast.Promotion] *)
type t =
  | NoPromotion
  | IntToReal
  | ToVar (* used in arrays, not functions *)
  | ToComplexVar (* used in arrays, not functions *)
  | IntToComplex
  | RealToComplex

let promote_inner (exp : Ast.typed_expression) prom =
  let emeta = exp.emeta in
  match prom with
  | ToVar ->
      Ast.
        { expr= Ast.Promotion (exp, UReal, AutoDiffable)
        ; emeta=
            { emeta with
              type_= UnsizedType.promote_array emeta.type_ UReal
            ; ad_level= AutoDiffable } }
  | ToComplexVar ->
      Ast.
        { expr= Ast.Promotion (exp, UComplex, AutoDiffable)
        ; emeta=
            { emeta with
              type_= UnsizedType.promote_array emeta.type_ UComplex
            ; ad_level= AutoDiffable } }
  | IntToReal when UnsizedType.is_int_type emeta.type_ ->
      Ast.
        { expr= Ast.Promotion (exp, UReal, emeta.ad_level)
        ; emeta= {emeta with type_= UnsizedType.promote_array emeta.type_ UReal}
        }
  | (IntToComplex | RealToComplex)
    when not (UnsizedType.is_complex_type emeta.type_) ->
      (* these two promotions are separated for cost, but are actually the same promotion *)
      { expr= Promotion (exp, UComplex, emeta.ad_level)
      ; emeta= {emeta with type_= UnsizedType.promote_array emeta.type_ UComplex}
      }
  | _ -> exp

let rec promote (exp : Ast.typed_expression) prom =
  (* promote arrays and rowvector literals at the lowest level to avoid unnecessary copies *)
  let open Ast in
  match exp.expr with
  | ArrayExpr es ->
      let pes = List.map ~f:(fun e -> promote e prom) es in
      let fst = List.hd_exn pes in
      let type_, ad_level = (fst.emeta.type_, fst.emeta.ad_level) in
      { expr= ArrayExpr pes
      ; emeta=
          { exp.emeta with
            type_= UnsizedType.promote_array exp.emeta.type_ type_
          ; ad_level } }
  | RowVectorExpr (_ :: _ as es) ->
      let pes = List.map ~f:(fun e -> promote e prom) es in
      let fst = List.hd_exn pes in
      let ad_level = fst.emeta.ad_level in
      {expr= RowVectorExpr pes; emeta= {exp.emeta with ad_level}}
  | _ -> promote_inner exp prom

let promote_list es promotions = List.map2_exn es promotions ~f:promote

(** Get the promotion needed to make the second type into the first.
  Types NEED to have previously been checked to be promotable
*)
let rec get_type_promotion_exn (ad, ty) (ad2, ty2) =
  match (ty, ty2) with
  | UnsizedType.(UReal, (UReal | UInt) | UVector, UVector | UMatrix, UMatrix)
    when ad <> ad2 ->
      ToVar
  | UComplex, (UReal | UInt | UComplex) when ad <> ad2 -> ToComplexVar
  | UReal, UInt -> IntToReal
  | UComplex, UInt -> IntToComplex
  | UComplex, UReal -> RealToComplex
  | UArray nt1, UArray nt2 -> get_type_promotion_exn (ad, nt1) (ad2, nt2)
  | t1, t2 when t1 = t2 -> NoPromotion
  | _, _ ->
      Common.FatalError.fatal_error_msg
        [%message
          "Tried to get promotion of mismatched types!"
            (ty : UnsizedType.t)
            (ty2 : UnsizedType.t)]

(** Calculate the "cost"/number of promotions performed.
    Used to disambiguate function signatures
*)
let promotion_cost p =
  match p with
  | NoPromotion | ToVar | ToComplexVar -> 0
  | RealToComplex | IntToReal -> 1
  | IntToComplex -> 2
