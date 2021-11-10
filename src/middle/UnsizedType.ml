(** Types which have dimensionalities but not sizes, e.g. [array\[,,\]] *)

open Core_kernel
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
    | ut -> (ut, depth)
  in
  aux 0 unsized_ty

let count_dims unsized_ty =
  let rec aux dims = function
    | UArray t -> aux (dims + 1) t
    | UMatrix -> dims + 2
    | UVector | URowVector -> dims + 1
    | _ -> dims
  in
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

let check_of_same_type_mod_conv name t1 t2 =
  if String.is_prefix name ~prefix:"assign_" then t1 = t2
  else
    match (t1, t2) with
    | UReal, UInt -> true
    | UComplex, UInt -> true
    | UComplex, UReal -> true
    | UFun (l1, rt1, s1, _), UFun (l2, rt2, s2, _) -> (
        s1 = s2 && rt1 = rt2
        &&
        match
          List.for_all2
            ~f:(fun (ad1, ut1) (ad2, ut2) ->
              ut1 = ut2 && autodifftype_can_convert ad2 ad1 )
            l1 l2
        with
        | List.Or_unequal_lengths.Ok ok -> ok
        | Unequal_lengths -> false )
    | _ -> t1 = t2

let rec check_of_same_type_mod_array_conv name t1 t2 =
  match (t1, t2) with
  | UArray t1elt, UArray t2elt ->
      check_of_same_type_mod_array_conv name t1elt t2elt
  | _ -> check_of_same_type_mod_conv name t1 t2

let check_compatible_arguments_mod_conv name args1 args2 =
  match
    List.for_all2
      ~f:(fun (ad1, ut1) (ad2, ut2) ->
        check_of_same_type_mod_conv name ut1 ut2
        && autodifftype_can_convert ad1 ad2 )
      args1 args2
  with
  | List.Or_unequal_lengths.Ok ok -> ok
  | Unequal_lengths -> false

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
let is_real_type = function
  | UReal | UVector | URowVector | UMatrix
   |UArray UReal
   |UArray UVector
   |UArray URowVector
   |UArray UMatrix ->
      true
  | _ -> false

let rec is_autodiffable = function
  | UReal | UVector | URowVector | UMatrix -> true
  | UArray t -> is_autodiffable t
  | _ -> false

let is_scalar_type = function UReal | UInt -> true | _ -> false
let is_int_type = function UInt | UArray UInt -> true | _ -> false

let is_eigen_type ut =
  match ut with UVector | URowVector | UMatrix -> true | _ -> false

let is_fun_type = function UFun _ | UMathLibraryFunction -> true | _ -> false

(** Detect if type contains an integer *)
let rec contains_int ut =
  match ut with UInt -> true | UArray ut -> contains_int ut | _ -> false

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
