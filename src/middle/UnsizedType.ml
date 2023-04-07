(** Types which have dimensionalities but not sizes, e.g. [array\[,,\]] *)

open Core_kernel
open Core_kernel.Poly

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
  | UFun of argumentlist * returntype * bool Fun_kind.suffix * Mem_pattern.t
  | UMathLibraryFunction

and argumentlist = (autodifftype * t) list

and autodifftype = DataOnly | AutoDiffable

and returntype = Void | ReturnType of t
[@@deriving compare, hash, sexp, equal]

let returntype_to_type_opt = function Void -> None | ReturnType t -> Some t

let pp_autodifftype ppf = function
  | DataOnly -> Fmt.string ppf "data "
  | AutoDiffable -> ()

let unsized_array_depth unsized_ty =
  let rec aux depth = function
    | UArray ut -> aux (depth + 1) ut
    | ut -> (ut, depth) in
  aux 0 unsized_ty

let count_dims unsized_ty =
  let rec aux dims = function
    | UArray t -> aux (dims + 1) t
    | UMatrix | UComplexMatrix -> dims + 2
    | UVector | URowVector | UComplexVector | UComplexRowVector -> dims + 1
    | _ -> dims in
  aux 0 unsized_ty

let rec unwind_array_type = function
  | UArray ut -> ( match unwind_array_type ut with ut2, d -> (ut2, d + 1) )
  | ut -> (ut, 0)

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
let autodifftype_can_convert at1 at2 =
  match (at1, at2) with DataOnly, AutoDiffable -> false | _ -> true

let lub_ad_type xs =
  List.max_elt ~compare:compare_autodifftype xs
  |> Option.value ~default:DataOnly

let%expect_test "lub_ad_type1" =
  let ads = [DataOnly; DataOnly; DataOnly; AutoDiffable] in
  let lub = lub_ad_type ads in
  print_s [%sexp (lub : autodifftype)] ;
  [%expect "AutoDiffable"]

let%expect_test "lub_ad_type2" =
  let ads = [DataOnly; DataOnly; DataOnly] in
  let lub = lub_ad_type ads in
  print_s [%sexp (lub : autodifftype)] ;
  [%expect "DataOnly"]

let%expect_test "lub_ad_type3" =
  let ads = [AutoDiffable; DataOnly; DataOnly; DataOnly] in
  let lub = lub_ad_type ads in
  print_s [%sexp (lub : autodifftype)] ;
  [%expect "AutoDiffable"]

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
  | t1, t2 when t1 = t2 -> Some t1
  | _, _ -> None

(* -- Helpers -- *)

let rec is_autodiffable = function
  | UReal | UVector | URowVector | UMatrix -> true
  | UArray t -> is_autodiffable t
  | _ -> false

let is_autodifftype possibly_adtype =
  match possibly_adtype with DataOnly -> false | AutoDiffable -> true

let is_dataonlytype possibly_adtype : bool =
  not (is_autodifftype possibly_adtype)

let is_scalar_type = function UReal | UInt -> true | _ -> false

let promote_container ut scalar =
  let scalar = fst (unwind_array_type scalar) in
  let rec loop ut =
    match (ut, scalar) with
    | (UInt | UReal), (UReal | UComplex) -> scalar
    | URowVector, UComplexRowVector -> UComplexRowVector
    | UVector, UComplexVector -> UComplexVector
    | UMatrix, UComplexMatrix -> UComplexMatrix
    | UArray ut2, _ -> UArray (loop ut2)
    | _, _ -> ut in
  loop ut

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
  | UFun _ | UMathLibraryFunction -> ut

let is_eigen_type ut =
  match ut with
  | UVector | URowVector | UMatrix | UComplexRowVector | UComplexVector
   |UComplexMatrix ->
      true
  | _ -> false

let is_fun_type = function UFun _ | UMathLibraryFunction -> true | _ -> false

(** Detect if type contains an integer *)
let rec contains_int ut =
  match ut with UInt -> true | UArray ut -> contains_int ut | _ -> false

let rec contains_eigen_type ut =
  match ut with
  | UInt | UComplex -> false
  | UReal | UMathLibraryFunction | UFun (_, Void, _, _) -> false
  | UVector | URowVector | UMatrix | UComplexRowVector | UComplexVector
   |UComplexMatrix ->
      true
  | UArray t | UFun (_, ReturnType t, _, _) -> contains_eigen_type t

let rec is_container ut =
  match ut with
  | UVector | URowVector | UMatrix | UComplexRowVector | UComplexVector
   |UComplexMatrix | UArray _ ->
      true
  | UReal | UInt | UComplex | UFun (_, Void, _, _) -> false
  | UFun (_, ReturnType t, _, _) -> is_container t
  | UMathLibraryFunction -> false

let is_array ut =
  match ut with
  | UInt | UComplex | UReal | UMathLibraryFunction | UFun _ | UVector
   |URowVector | UMatrix | UComplexVector | UComplexRowVector | UComplexMatrix
    ->
      false
  | UArray _ -> true

let rec is_indexing_matrix = function
  | UArray t, _ :: idcs -> is_indexing_matrix (t, idcs)
  | (UMatrix | UComplexMatrix), [] -> false
  | (UMatrix | UComplexMatrix), _ -> true
  | _ -> false

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
