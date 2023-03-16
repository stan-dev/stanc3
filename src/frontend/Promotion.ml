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
  | TuplePromotion of t list
[@@deriving sexp]

let rec promote_unsized_type ~scalar (typ : UnsizedType.t)
    (ad : UnsizedType.autodifftype) (prom : t) =
  match (prom, typ, ad) with
  | IntToReal, UInt, _ -> (UnsizedType.UReal, ad)
  | ToVar, (UReal | UInt), _ -> (UReal, AutoDiffable)
  | ToComplexVar, (UComplex | UReal | UInt), _ -> (UComplex, AutoDiffable)
  | IntToComplex, UInt, _ | RealToComplex, UReal, _ -> (UComplex, ad)
  | TuplePromotion proms, UTuple ts, TupleAD ads ->
      let typs, ads =
        List.unzip
        @@ List.map3_exn ~f:(promote_unsized_type ~scalar) ts ads proms in
      (UTuple typs, TupleAD ads)
  | TuplePromotion proms, UTuple ts, ad ->
      let typs, ads =
        List.unzip
        @@ List.map2_exn
             ~f:(fun ts proms -> promote_unsized_type ~scalar ts ad proms)
             ts proms in
      (UTuple typs, TupleAD ads)
  | _, UArray t, _ ->
      let t, ads = promote_unsized_type ~scalar t ad prom in
      if scalar then (t, ads) else (UArray t, ads)
  | ToVar, _, _ when UnsizedType.is_eigen_type typ ->
      if scalar then (UReal, AutoDiffable) else (typ, AutoDiffable)
  | RealToComplex, _, _ when UnsizedType.is_eigen_type typ ->
      if scalar then (UComplex, ad)
      else (UnsizedType.promote_container typ UComplex, ad)
  | ToComplexVar, _, _ when UnsizedType.is_eigen_type typ ->
      if scalar then (UComplex, AutoDiffable)
      else (UnsizedType.promote_container typ UComplex, AutoDiffable)
  | NoPromotion, _, _ ->
      if scalar then (UnsizedType.internal_scalar typ, ad) else (typ, ad)
  | (IntToReal | ToVar | ToComplexVar | IntToComplex), _, _ ->
      Common.FatalError.fatal_error_msg
        [%message
          "Failed to promote type, unexpected type:"
            (prom : t)
            (typ : UnsizedType.t)
            (ad : UnsizedType.autodifftype)]
  | TuplePromotion _, _, _ ->
      Common.FatalError.fatal_error_msg
        [%message
          "Found Tuple Promotion for a non-tuple type:"
            (prom : t)
            (typ : UnsizedType.t)
            (ad : UnsizedType.autodifftype)]
  | _, _, TupleAD _ ->
      Common.FatalError.fatal_error_msg
        [%message
          "Found Tuple Autodiff in promotion for a non-tuple type:"
            (prom : t)
            (typ : UnsizedType.t)
            (ad : UnsizedType.autodifftype)]
  | _, _, _ -> (typ, ad)

let promote_inner (exp : Ast.typed_expression) prom =
  let emeta = exp.emeta in
  match prom with
  | ToVar ->
      Ast.
        { expr= Ast.Promotion (exp, UReal, AutoDiffable)
        ; emeta=
            { emeta with
              type_= UnsizedType.promote_container emeta.type_ UReal
            ; ad_level= AutoDiffable } }
  | ToComplexVar ->
      Ast.
        { expr= Ast.Promotion (exp, UComplex, AutoDiffable)
        ; emeta=
            { emeta with
              type_= UnsizedType.promote_container emeta.type_ UComplex
            ; ad_level= AutoDiffable } }
  | IntToReal when UnsizedType.is_int_type emeta.type_ ->
      Ast.
        { expr= Ast.Promotion (exp, UReal, emeta.ad_level)
        ; emeta=
            {emeta with type_= UnsizedType.promote_container emeta.type_ UReal}
        }
  | (IntToComplex | RealToComplex)
    when not (UnsizedType.is_complex_type emeta.type_) ->
      (* these two promotions are separated for cost, but are actually the same promotion *)
      { expr= Promotion (exp, UComplex, emeta.ad_level)
      ; emeta=
          {emeta with type_= UnsizedType.promote_container emeta.type_ UComplex}
      }
  | TuplePromotion _ -> (
      let element, size = UnsizedType.unwind_array_type emeta.type_ in
      match element with
      | UTuple _ ->
          let prom_type, prom_ad =
            promote_unsized_type ~scalar:true element emeta.ad_level prom in
          let type_, ad_level =
            promote_unsized_type ~scalar:false element emeta.ad_level prom in
          let type_ = UnsizedType.wind_array_type (type_, size) in
          { expr= Promotion (exp, prom_type, prom_ad)
          ; emeta= {emeta with type_; ad_level} }
      | _ -> exp
      (* TUPLE MAYBE: PROMOTION *) )
  | _ -> exp

let rec promote (exp : Ast.typed_expression) prom =
  (* promote arrays and rowvector literals at the lowest level to avoid unnecessary copies *)
  let open Ast in
  match exp.expr with
  | ArrayExpr es ->
      let pes = List.map ~f:(fun e -> promote e prom) es in
      let fst = List.hd_exn pes in
      let type_, ad_level =
        ( UnsizedType.promote_container exp.emeta.type_ fst.emeta.type_
        , fst.emeta.ad_level ) in
      {expr= ArrayExpr pes; emeta= {exp.emeta with type_; ad_level}}
  | RowVectorExpr (_ :: _ as es) ->
      let pes = List.map ~f:(fun e -> promote e prom) es in
      let fst = List.hd_exn pes in
      let ad_level = fst.emeta.ad_level in
      let type_ =
        (* "RowVectorExpr" can also be a matrix expr, depends on what is inside *)
        match fst.emeta.type_ with
        | UComplexRowVector -> UnsizedType.UComplexMatrix
        | URowVector -> UMatrix
        | UComplex -> UComplexRowVector
        | _ -> URowVector in
      {expr= RowVectorExpr pes; emeta= {exp.emeta with type_; ad_level}}
  | TupleExpr (_ :: _ as es) -> (
    match prom with
    | TuplePromotion ts ->
        let pes = List.map2_exn ~f:promote es ts in
        let type_ =
          UnsizedType.UTuple (List.map ~f:(fun e -> e.emeta.type_) pes) in
        let ad_level =
          UnsizedType.TupleAD (List.map ~f:(fun e -> e.emeta.ad_level) pes)
        in
        {expr= TupleExpr pes; emeta= {exp.emeta with type_; ad_level}}
    | _ -> exp )
  | _ -> promote_inner exp prom

let promote_list es promotions = List.map2_exn es promotions ~f:promote

(** Get the promotion needed to make the second type into the first.
  Types NEED to have previously been checked to be promotable
*)
let rec get_type_promotion_exn (ad_orig, ty_orig) (ad_expect, ty_expect) =
  match (ty_orig, ty_expect) with
  | UnsizedType.(
      ( UReal, (UReal | UInt)
      | UVector, UVector
      | URowVector, URowVector
      | UMatrix, UMatrix ))
    when ad_orig <> ad_expect ->
      ToVar
  | UComplex, (UReal | UInt | UComplex)
   |UComplexMatrix, (UMatrix | UComplexMatrix)
   |UComplexVector, (UVector | UComplexVector)
   |UComplexRowVector, (URowVector | UComplexRowVector)
    when ad_orig <> ad_expect ->
      ToComplexVar
  | UReal, UInt -> IntToReal
  | UComplex, UInt -> IntToComplex
  | UComplex, UReal
   |UComplexMatrix, UMatrix
   |UComplexVector, UVector
   |UComplexRowVector, URowVector ->
      RealToComplex
  | UArray nt1, UArray nt2 ->
      get_type_promotion_exn (ad_orig, nt1) (ad_expect, nt2)
  | UTuple ts1, UTuple ts2 ->
      TuplePromotion
        (List.map2_exn
           ~f:(fun t1 t2 -> get_type_promotion_exn (ad_orig, t1) (ad_expect, t2))
           ts1 ts2 )
  | t1, t2 when t1 = t2 -> NoPromotion
  | _, _ ->
      Common.FatalError.fatal_error_msg
        [%message
          "Tried to get promotion of mismatched types!"
            (ty_orig : UnsizedType.t)
            (ty_expect : UnsizedType.t)]

(** Calculate the "cost"/number of promotions performed.
    Used to disambiguate function signatures
*)
let rec promotion_cost p =
  match p with
  | NoPromotion | ToVar | ToComplexVar -> 0
  | RealToComplex | IntToReal -> 1
  | IntToComplex -> 2
  | TuplePromotion ts -> List.sum (module Int) ~f:promotion_cost ts
