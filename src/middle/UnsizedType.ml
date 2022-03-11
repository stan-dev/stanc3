(** Types which have dimensionalities but not sizes, e.g. [array\[,,\]] *)

open Core_kernel
open Core_kernel.Poly
open Common.Helpers

type t =
  | UInt
  | UReal
  | UVector
  | UComplex
  | URowVector
  | UMatrix
  | UArray of t
  | UFun of
      (autodifftype * t) list
      * returntype
      * bool Fun_kind.suffix
      * Common.Helpers.mem_pattern
  | UMathLibraryFunction

and autodifftype = DataOnly | AutoDiffable

and returntype = Void | ReturnType of t [@@deriving compare, hash, sexp]

let pp_autodifftype ppf = function
  | DataOnly -> pp_keyword ppf "data "
  | AutoDiffable -> ()

let unsized_array_depth unsized_ty =
  let rec aux depth = function
    | UArray ut -> aux (depth + 1) ut
    | ut -> (ut, depth) in
  aux 0 unsized_ty

let count_dims unsized_ty =
  let rec aux dims = function
    | UArray t -> aux (dims + 1) t
    | UMatrix -> dims + 2
    | UVector | URowVector -> dims + 1
    | _ -> dims in
  aux 0 unsized_ty

let rec unwind_array_type = function
  | UArray ut -> ( match unwind_array_type ut with ut2, d -> (ut2, d + 1) )
  | ut -> (ut, 0)

let rec pp ppf = function
  | UInt -> pp_keyword ppf "int"
  | UReal -> pp_keyword ppf "real"
  | UComplex -> pp_keyword ppf "complex"
  | UVector -> pp_keyword ppf "vector"
  | URowVector -> pp_keyword ppf "row_vector"
  | UMatrix -> pp_keyword ppf "matrix"
  | UArray ut ->
      let ut2, d = unwind_array_type ut in
      let array_str = "[" ^ String.make d ',' ^ "]" in
      Fmt.pf ppf "array%s %a" array_str pp ut2
  | UFun (argtypes, rt, _, _) ->
      Fmt.pf ppf {|@[<h>(%a) => %a@]|}
        Fmt.(list pp_fun_arg ~sep:comma)
        argtypes pp_returntype rt
  | UMathLibraryFunction ->
      (pp_angle_brackets Fmt.string) ppf "Stan Math function"

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

let rec lub_ad_type = function
  | [] -> DataOnly
  | x :: xs ->
      let y = lub_ad_type xs in
      if compare_autodifftype x y < 0 then y else x

(** Given two types find the minimal type both can convert to *)
let rec common_type = function
  | UReal, UInt | UInt, UReal -> Some UReal
  | UComplex, UInt | UInt, UComplex | UComplex, UReal | UReal, UComplex ->
      Some UComplex
  | UArray t1, UArray t2 ->
      common_type (t1, t2) |> Option.map ~f:(fun t -> UArray t)
  | t1, t2 when t1 = t2 -> Some t1
  | _, _ -> None

(* -- Helpers -- *)
let rec is_real_type = function
  | UReal | UVector | URowVector | UMatrix -> true
  | UArray x -> is_real_type x
  | _ -> false

let rec is_autodiffable = function
  | UReal | UVector | URowVector | UMatrix -> true
  | UArray t -> is_autodiffable t
  | _ -> false

let is_autodifftype possibly_adtype =
  match possibly_adtype with DataOnly -> false | AutoDiffable -> true

let is_dataonlytype possibly_adtype : bool =
  not (is_autodifftype possibly_adtype)

let is_scalar_type = function UReal | UInt -> true | _ -> false

let promote_array ut scalar =
  let scalar = fst (unwind_array_type scalar) in
  let rec loop ut =
    match (ut, scalar) with
    | (UInt | UReal), (UReal | UComplex) -> scalar
    | UArray ut2, _ -> UArray (loop ut2)
    | _, _ -> ut in
  loop ut

let rec is_int_type ut =
  match ut with UInt -> true | UArray ut -> is_int_type ut | _ -> false

let rec is_complex_type ut =
  match ut with
  | UComplex -> true
  | UArray ut -> is_complex_type ut
  | _ -> false

let rec internal_scalar ut =
  match ut with
  | UVector | UMatrix | URowVector | UReal -> UReal
  | UInt -> UInt
  | UComplex -> UComplex
  | UArray ut -> internal_scalar ut
  | _ ->
      Common.FatalError.fatal_error_msg
        [%message "Tried to get scalar type of " (ut : t)]

let is_eigen_type ut =
  match ut with UVector | URowVector | UMatrix -> true | _ -> false

let is_fun_type = function UFun _ | UMathLibraryFunction -> true | _ -> false

(** Detect if type contains an integer *)
let rec contains_int ut =
  match ut with UInt -> true | UArray ut -> contains_int ut | _ -> false

let rec contains_eigen_type ut =
  match ut with
  | UInt | UComplex -> false
  | UReal | UMathLibraryFunction | UFun (_, Void, _, _) -> false
  | UVector | URowVector | UMatrix -> true
  | UArray t | UFun (_, ReturnType t, _, _) -> contains_eigen_type t

let rec is_container ut =
  match ut with
  | UVector | URowVector | UMatrix | UArray _ -> true
  | UReal | UInt | UComplex | UFun (_, Void, _, _) -> false
  | UFun (_, ReturnType t, _, _) -> is_container t
  | UMathLibraryFunction -> false

let return_contains_eigen_type ret =
  match ret with ReturnType t -> contains_eigen_type t | Void -> false

let rec is_indexing_matrix = function
  | UArray t, _ :: idcs -> is_indexing_matrix (t, idcs)
  | UMatrix, [] -> false
  | UMatrix, _ -> true
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
