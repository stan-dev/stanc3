(** Types which have dimensionalities but not sizes, e.g. [array\[,,\]] *)

open Core
open Core.Poly

type t =
  | UInt
  | UReal
  | UVector
  | UComplex
  | URowVector
  | UMatrix
  | UComplexVector
  | UComplexRowVector
  | UComplexMatrix
  | UArray of t
  | UTuple of t list
  | UFun of argumentlist * returntype * bool Fun_kind.suffix * Mem_pattern.t
  | UMathLibraryFunction
[@@deriving compare, hash, sexp]

and autodifftype = DataOnly | AutoDiffable | TupleAD of autodifftype list
[@@deriving compare, hash, sexp]

and argumentlist = (autodifftype * t) list

and returntype = Void | ReturnType of t
[@@deriving compare, hash, sexp, equal]

let rec pp_tuple_autodifftype ppf = function
  | DataOnly -> Fmt.string ppf "data"
  | AutoDiffable -> Fmt.string ppf "var"
  | TupleAD ad ->
      Fmt.pf ppf "tuple_ad@[(%a)@]"
        Fmt.(list ~sep:comma pp_tuple_autodifftype)
        ad

let returntype_to_type_opt = function Void -> None | ReturnType t -> Some t

let pp_autodifftype ppf = function
  | DataOnly -> Fmt.string ppf "data "
  | AutoDiffable -> ()
  | tup -> pp_tuple_autodifftype ppf tup

let count_dims unsized_ty =
  let rec aux dims = function
    | UArray t -> aux (dims + 1) t
    | UMatrix | UComplexMatrix -> dims + 2
    | UVector | URowVector | UComplexVector | UComplexRowVector -> dims + 1
    | _ -> dims in
  aux 0 unsized_ty

let rec contains_tuple t =
  match t with UTuple _ -> true | UArray t -> contains_tuple t | _ -> false

let rec unwind_array_type = function
  | UArray ut -> ( match unwind_array_type ut with ut2, d -> (ut2, d + 1))
  | ut -> (ut, 0)

let rec wind_array_type = function
  | typ, 0 -> typ
  | typ, n -> wind_array_type (UArray typ, n - 1)

let rec pp ppf = function
  | UInt -> Fmt.string ppf "int"
  | UReal -> Fmt.string ppf "real"
  | UComplex -> Fmt.string ppf "complex"
  | UVector -> Fmt.string ppf "vector"
  | URowVector -> Fmt.string ppf "row_vector"
  | UMatrix -> Fmt.string ppf "matrix"
  | UComplexVector -> Fmt.string ppf "complex_vector"
  | UComplexRowVector -> Fmt.string ppf "complex_row_vector"
  | UComplexMatrix -> Fmt.string ppf "complex_matrix"
  | UArray ut ->
      let ut2, d = unwind_array_type ut in
      let array_str = "[" ^ String.make d ',' ^ "]" in
      Fmt.pf ppf "array%s %a" array_str pp ut2
  | UTuple ts -> (
      match ts with
      | [t] -> Fmt.pf ppf "tuple(@[%a,@])" pp t
      | _ -> Fmt.pf ppf "tuple(@[%a@])" Fmt.(list ~sep:comma pp) ts)
  | UFun (argtypes, rt, _, _) ->
      Fmt.pf ppf {|@[<h>(%a) => %a@]|}
        Fmt.(list pp_fun_arg ~sep:comma)
        argtypes pp_returntype rt
  | UMathLibraryFunction -> Fmt.string ppf "<Stan Math function>"

and pp_fun_arg ppf (ad_ty, unsized_ty) =
  match ad_ty with
  | DataOnly -> Fmt.pf ppf {|data %a|} pp unsized_ty
  | _ -> pp ppf unsized_ty

and pp_returntype ppf = function
  | Void -> Fmt.string ppf "void"
  | ReturnType ut -> pp ppf ut

(* -- Type conversion -- *)
let rec autodifftype_can_convert at1 at2 =
  match (at1, at2) with
  | DataOnly, AutoDiffable -> false
  | TupleAD ads1, TupleAD ads2 -> (
      match List.for_all2 ads1 ads2 ~f:autodifftype_can_convert with
      | Ok x -> x
      | Unequal_lengths -> false)
  | DataOnly, TupleAD ads ->
      List.for_all ads ~f:(autodifftype_can_convert DataOnly)
  | _, _ -> true

let rec has_autodiff = function
  | DataOnly -> false
  | AutoDiffable -> true
  | TupleAD ads -> List.exists ads ~f:has_autodiff

let any_autodiff xs = List.exists xs ~f:has_autodiff

let lub_ad_type xs =
  let rec common_ad t1 t2 =
    match (t1, t2) with
    | DataOnly, ad | ad, DataOnly -> Ok ad
    | AutoDiffable, AutoDiffable -> Ok AutoDiffable
    | TupleAD ads1, TupleAD ads2 -> (
        match List.map2 ads1 ads2 ~f:common_ad with
        | Ok ads -> ads |> Result.all |> Result.map ~f:(fun ads -> TupleAD ads)
        | Unequal_lengths -> Error ())
    | TupleAD ads, AutoDiffable | AutoDiffable, TupleAD ads ->
        List.map ads ~f:(common_ad AutoDiffable)
        |> Result.all
        |> Result.map ~f:(fun ads -> TupleAD ads) in
  List.fold_result ~init:DataOnly ~f:common_ad xs |> Result.ok

let%expect_test "lub_ad_type1" =
  let ads = [DataOnly; DataOnly; DataOnly; AutoDiffable] in
  let lub = lub_ad_type ads in
  print_s [%sexp (lub : autodifftype option)];
  [%expect "(AutoDiffable)"]

let%expect_test "lub_ad_type2" =
  let ads = [DataOnly; DataOnly; DataOnly] in
  let lub = lub_ad_type ads in
  print_s [%sexp (lub : autodifftype option)];
  [%expect "(DataOnly)"]

let%expect_test "lub_ad_type3" =
  let ads = [AutoDiffable; DataOnly; DataOnly; DataOnly] in
  let lub = lub_ad_type ads in
  print_s [%sexp (lub : autodifftype option)];
  [%expect "(AutoDiffable)"]

(** Given two types find the minimal type both can convert to *)
let rec common_type = function
  | UReal, UInt | UInt, UReal -> Some UReal
  | UComplex, UInt | UInt, UComplex | UComplex, UReal | UReal, UComplex ->
      Some UComplex
  | UComplexVector, UVector | UVector, UComplexVector -> Some UComplexVector
  | UComplexRowVector, URowVector | URowVector, UComplexRowVector ->
      Some UComplexRowVector
  | UComplexMatrix, UMatrix | UMatrix, UComplexMatrix -> Some UComplexMatrix
  | UArray t1, UArray t2 ->
      common_type (t1, t2) |> Option.map ~f:(fun t -> UArray t)
  | UTuple ts1, UTuple ts2 ->
      (match List.zip ts1 ts2 with
      | Ok ts -> List.map ts ~f:common_type |> Option.all
      | Unequal_lengths -> None)
      |> Option.map ~f:(fun ts -> UTuple ts)
  | t1, t2 when t1 = t2 -> Some t1
  | _, _ -> None

(* -- Helpers -- *)

let rec is_autodiffable = function
  | UReal | UVector | URowVector | UMatrix -> true
  | UArray t -> is_autodiffable t
  | _ (* wrong? *) -> false

let rec is_autodifftype possibly_adtype =
  match possibly_adtype with
  | DataOnly -> false
  | AutoDiffable -> true
  | TupleAD ads -> List.exists ~f:is_autodifftype ads

let is_dataonlytype possibly_adtype : bool =
  not (is_autodifftype possibly_adtype)

let is_scalar_type = function UReal | UInt -> true | _ -> false

let promote_container ut scalar =
  let scalar = fst (unwind_array_type scalar) in
  let rec loop ut =
    match (ut, scalar) with
    | (UInt | UReal), (UReal | UComplex) -> scalar
    | UTuple _, UTuple _ -> scalar
    | URowVector, (UComplexRowVector | UComplex) -> UComplexRowVector
    | UVector, (UComplexVector | UComplex) -> UComplexVector
    | UMatrix, (UComplexMatrix | UComplex) -> UComplexMatrix
    | UArray ut2, _ -> UArray (loop ut2)
    | _, _ -> ut in
  loop ut

(** Used to determine valid covariates for [_lpmf] functions *)
let rec is_discrete_type ut =
  match ut with
  | UInt -> true
  | UArray ut -> is_discrete_type ut
  | UTuple ts -> List.for_all ~f:is_discrete_type ts
  | _ -> false

(** Used in code generation and other places, does _not_ include tuples of ints *)
let rec is_int_type ut =
  match ut with UInt -> true | UArray ut -> is_int_type ut | _ -> false

let rec is_complex_type ut =
  match ut with
  | UComplex | UComplexMatrix | UComplexRowVector | UComplexVector -> true
  | UArray ut -> is_complex_type ut
  | _ -> false

let rec internal_scalar ut =
  match ut with
  | UVector | UMatrix | URowVector | UReal -> UReal
  | UInt -> UInt
  | UComplex | UComplexVector | UComplexMatrix | UComplexRowVector -> UComplex
  | UArray ut -> internal_scalar ut
  | UFun _ | UMathLibraryFunction | UTuple _ -> ut

let is_eigen_type ut =
  match ut with
  | UVector | URowVector | UMatrix | UComplexRowVector | UComplexVector
   |UComplexMatrix ->
      true
  | _ -> false

let is_fun_type = function UFun _ | UMathLibraryFunction -> true | _ -> false

(** Detect if type contains an integer *)
let rec contains_int ut =
  match ut with
  | UInt -> true
  | UArray ut -> contains_int ut
  | UTuple ts -> List.exists ~f:contains_int ts
  | _ -> false

let rec contains_eigen_type ut =
  match ut with
  | UInt | UComplex -> false
  | UReal | UMathLibraryFunction | UFun (_, Void, _, _) -> false
  | UVector | URowVector | UMatrix | UComplexRowVector | UComplexVector
   |UComplexMatrix ->
      true
  | UArray t | UFun (_, ReturnType t, _, _) -> contains_eigen_type t
  | UTuple ts -> List.exists ~f:contains_eigen_type ts

let rec is_container ut =
  match ut with
  | UVector | URowVector | UMatrix | UArray _ | UTuple _ | UComplexRowVector
   |UComplexVector | UComplexMatrix ->
      true
  | UReal | UInt | UComplex | UFun (_, Void, _, _) -> false
  | UFun (_, ReturnType t, _, _) -> is_container t
  | UMathLibraryFunction -> false

let is_array ut =
  match ut with
  | UInt | UComplex | UReal | UMathLibraryFunction | UFun _ | UVector
   |URowVector | UMatrix | UTuple _ | UComplexVector | UComplexRowVector
   |UComplexMatrix ->
      false
  | UArray _ -> true

let rec is_indexing_matrix = function
  | UArray t, _ :: idcs -> is_indexing_matrix (t, idcs)
  | (UMatrix | UComplexMatrix), [] -> false
  | (UMatrix | UComplexMatrix), _ -> true
  | _ -> false

(** In some places (e.g. code generation) we need to instantiate an AD type.
    Previously we would just say DataOnly or AutoDiffable, however this breaks
    the invariant that a Tuple always has TupleAD as it's autodifftype *)
let rec fill_adtype_for_type ad ut =
  match (ad, ut) with
  | _, UArray t -> fill_adtype_for_type ad t
  | TupleAD ads, UTuple ts ->
      TupleAD (List.map2_exn ~f:fill_adtype_for_type ads ts)
  | _, UTuple ts -> TupleAD (List.map ~f:(fill_adtype_for_type ad) ts)
  | TupleAD _, _ ->
      Common.ICE.internal_compiler_error
        [%message
          "Attempting to give a non-tuple a TupleAD type"
            (ut : t)
            (ad : autodifftype)]
  | _, _ -> ad

(** List all possible tuple sub-names for IO purposes.
    E.g, the decl
    [array[2] (int, real) foo;]
    should yield the list [["foo.1";"foo.2"]].
  *)
let enumerate_tuple_names_io name (ut : t) =
  let rec loop base ut =
    match ut with
    | UTuple ts ->
        List.concat_mapi ts ~f:(fun i t ->
            loop (base ^ "." ^ string_of_int (i + 1)) t)
    | UArray _ when contains_tuple ut ->
        let scalar, _ = unwind_array_type ut in
        loop base scalar
    | _ -> [base] in
  loop name ut

let%expect_test "tuple names" =
  let t = UArray (UTuple [UInt; UArray (UTuple [UReal; UComplex]); UVector]) in
  let res = enumerate_tuple_names_io "foo" t in
  [%sexp (res : string list)] |> print_s;
  [%expect {|
      (foo.1 foo.2.1 foo.2.2 foo.3) |}]

module Comparator = Comparator.Make (struct
  type nonrec t = t

  let compare = compare
  let sexp_of_t = sexp_of_t
end)

include Comparator

include Comparable.Make_using_comparator (struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp

  include Comparator
end)
