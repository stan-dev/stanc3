open Core_kernel
open Common.Helpers

type t =
  | UInt
  | UReal
  | UVector
  | URowVector
  | UMatrix
  | UArray of t
  | UFun of (autodifftype * t) list * returntype * bool
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

let rec pp ppf = function
  | UInt -> pp_keyword ppf "int"
  | UReal -> pp_keyword ppf "real"
  | UVector -> pp_keyword ppf "vector"
  | URowVector -> pp_keyword ppf "row_vector"
  | UMatrix -> pp_keyword ppf "matrix"
  | UArray ut ->
      let ty, depth = unsized_array_depth ut in
      let commas = String.make depth ',' in
      Fmt.pf ppf "%a[%s]" pp ty commas
  | UFun (argtypes, rt, _) ->
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
    | UFun (_, _, false), UFun (_, _, true) -> false
    | UFun (l1, rt1, _), UFun (l2, rt2, _) -> (
        rt1 = rt2
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

let is_eigen_type = function
  | UVector | URowVector | UMatrix -> true
  | _ -> false

let is_fun_type = function UFun _ -> true | _ -> false

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

module TypeMap = Core_kernel.Map.Make_using_comparator (struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp

  include Comparator
end)

let pp_sigs ppf tys =
  let ctx = ref TypeMap.empty in
  let rec pp ppf = function
    | UInt -> pp_keyword ppf "int"
    | UReal -> pp_keyword ppf "real"
    | UVector -> pp_keyword ppf "vector"
    | URowVector -> pp_keyword ppf "row_vector"
    | UMatrix -> pp_keyword ppf "matrix"
    | UArray ut ->
        let ty, depth = unsized_array_depth ut in
        let commas = String.make depth ',' in
        Fmt.pf ppf "@[<hov>%a[%s]@]" pp ty commas
    | UFun (tys, rt, _) as t -> (
      match Map.find !ctx t with
      | Some (id, _) -> Fmt.pf ppf "%s" id
      | None ->
          let id = Fmt.strf "<F%d>" (Map.length !ctx + 1) in
          ctx := Map.add_exn !ctx ~key:t ~data:(id, (rt, tys)) ;
          Fmt.pf ppf "%s" id )
    | UMathLibraryFunction ->
        (pp_angle_brackets Fmt.string) ppf "Stan Math function"
  and pp_fun_arg ppf (ad_ty, unsized_ty) =
    match ad_ty with
    | DataOnly -> Fmt.pf ppf {|data %a|} pp unsized_ty
    | _ -> pp ppf unsized_ty
  and pp_returntype ppf = function
    | Void -> Fmt.string ppf "void"
    | ReturnType ut -> pp ppf ut
  and pp_fn ppf (rt, argtypes) =
    Fmt.pf ppf {|(@[<h>%a@]) => %a|}
      Fmt.(list pp_fun_arg ~sep:comma)
      argtypes pp_returntype rt
  and pp_fun ppf (id, tys) = Fmt.pf ppf {|@[<h>%s = @[<h>%a@]@]|} id pp_fn tys
  and pp_where ppf fns =
    let old = !ctx in
    let new_ = Map.filter_keys !ctx ~f:(fun ty -> not (Map.mem fns ty)) in
    if not (Map.is_empty new_) then (
      Fmt.cut ppf () ;
      let compare (_, (id1, _)) (_, (id2, _)) = String.compare id1 id2 in
      Fmt.list pp_fun ppf
        (List.map ~f:snd (List.sort ~compare (Map.to_alist new_))) ;
      pp_where ppf old )
  in
  Fmt.pf ppf "@[<v>%a" Fmt.(list pp_fn) tys ;
  if not (Map.is_empty !ctx) then
    Fmt.pf ppf "@,  where%a" pp_where TypeMap.empty ;
  Fmt.pf ppf "@]" ;
  !ctx

let pp_args ppf (fns, tys) =
  let ctx = ref fns in
  let rec pp ppf = function
    | UInt -> pp_keyword ppf "int"
    | UReal -> pp_keyword ppf "real"
    | UVector -> pp_keyword ppf "vector"
    | URowVector -> pp_keyword ppf "row_vector"
    | UMatrix -> pp_keyword ppf "matrix"
    | UArray ut ->
        let ty, depth = unsized_array_depth ut in
        let commas = String.make depth ',' in
        Fmt.pf ppf "@[<hov>%a[%s]@]" pp ty commas
    | UFun (tys, rt, _) as t -> (
      match Map.find !ctx t with
      | Some (id, _) -> Fmt.pf ppf "%s" id
      | None ->
          let id = Fmt.strf "<F%d>" (Map.length !ctx + 1) in
          ctx := Map.add_exn !ctx ~key:t ~data:(id, (rt, tys)) ;
          Fmt.pf ppf "%s" id )
    | UMathLibraryFunction ->
        (pp_angle_brackets Fmt.string) ppf "Stan Math function"
  and pp_fun_arg ppf (ad_ty, unsized_ty) =
    match ad_ty with
    | DataOnly -> Fmt.pf ppf {|data %a|} pp unsized_ty
    | _ -> pp ppf unsized_ty
  and pp_returntype ppf = function
    | Void -> Fmt.string ppf "void"
    | ReturnType ut -> pp ppf ut
  and pp_fn ppf (rt, argtypes) =
    Fmt.pf ppf {|(@[<h>%a@]) => %a|}
      Fmt.(list pp_fun_arg ~sep:comma)
      argtypes pp_returntype rt
  and pp_fun ppf (id, tys) = Fmt.pf ppf {|@[<h>%s = @[<h>%a@]@]|} id pp_fn tys
  and pp_where ppf fns =
    let old = !ctx in
    let new_ = Map.filter_keys !ctx ~f:(fun ty -> not (Map.mem fns ty)) in
    if not (Map.is_empty new_) then (
      Fmt.cut ppf () ;
      let compare (_, (id1, _)) (_, (id2, _)) = String.compare id1 id2 in
      Fmt.list pp_fun ppf
        (List.map ~f:snd (List.sort ~compare (Map.to_alist new_))) ;
      pp_where ppf old )
  in
  Fmt.pf ppf "@[<v>(@[<hov>%a@])" Fmt.(list ~sep:comma pp) tys ;
  if Int.( > ) (Map.length !ctx) (Map.length fns) then
    Fmt.pf ppf "@,  where%a" pp_where fns ;
  Fmt.pf ppf "@]"
