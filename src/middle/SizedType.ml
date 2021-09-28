open Core_kernel
open Common

type 'a t =
  | SInt
  | SReal
  | SVector of Common.Helpers.mem_pattern * 'a
  | SRowVector of Common.Helpers.mem_pattern * 'a
  | SMatrix of Common.Helpers.mem_pattern * 'a * 'a
  | SArray of 'a t * 'a
[@@deriving sexp, compare, map, hash, fold]

let rec pp pp_e ppf = function
  | SInt -> Fmt.string ppf "int"
  | SReal -> Fmt.string ppf "real"
  | SVector (_, expr) -> Fmt.pf ppf {|vector%a|} (Fmt.brackets pp_e) expr
  | SRowVector (_, expr) ->
      Fmt.pf ppf {|row_vector%a|} (Fmt.brackets pp_e) expr
  | SMatrix (_, d1_expr, d2_expr) ->
      Fmt.pf ppf {|matrix%a|}
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
  | SArray (st, expr) ->
      Fmt.pf ppf {|array%a|}
        Fmt.(pair ~sep:comma (fun ppf st -> pp pp_e ppf st) pp_e |> brackets)
        (st, expr)

let collect_exprs st =
  let rec aux accu = function
    | SInt | SReal -> List.rev accu
    | SVector (_, e) | SRowVector (_, e) -> List.rev @@ (e :: accu)
    | SMatrix (_, e1, e2) -> List.rev @@ (e1 :: e2 :: accu)
    | SArray (inner, e) -> aux (e :: accu) inner
  in
  aux [] st

let rec to_unsized = function
  | SInt -> UnsizedType.UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SArray (t, _) -> UArray (to_unsized t)

let rec associate ?init:(assocs = Label.Int_label.Map.empty) = function
  | SInt | SReal -> assocs
  | SVector (_, e) | SRowVector (_, e) ->
      Expr.Labelled.associate ~init:assocs e
  | SMatrix (_, e1, e2) ->
      Expr.Labelled.(associate ~init:(associate ~init:assocs e1) e2)
  | SArray (st, e) ->
      associate ~init:(Expr.Labelled.associate ~init:assocs e) st

let is_scalar = function SInt | SReal -> true | _ -> false
let rec inner_type = function SArray (t, _) -> inner_type t | t -> t

let rec dims_of st =
  match st with
  | SArray (t, _) -> dims_of t
  | SMatrix (_, d1, d2) -> [d1; d2]
  | SRowVector (_, dim) | SVector (_, dim) -> [dim]
  | SInt | SReal -> []

let rec get_dims st =
  match st with
  | SInt | SReal -> []
  | SVector (_, d) | SRowVector (_, d) -> [d]
  | SMatrix (_, dim1, dim2) -> [dim1; dim2]
  | SArray (t, dim) -> dim :: get_dims t

(**
 * Check whether a SizedType holds indexable SizedTypes.
 *)
let is_recursive_container st =
  match st with
  | SInt | SReal | SVector _ | SRowVector _ | SMatrix _
   |SArray ((SInt | SReal), _) ->
      false
  | SArray _ -> true

(* Return a type's array dimensions and the type inside the (possibly nested) array *)
let rec get_array_dims st =
  match st with
  | SInt | SReal -> (st, [])
  | SVector (_, d) | SRowVector (_, d) -> (st, [d])
  | SMatrix (_, d1, d2) -> (st, [d1; d2])
  | SArray (st, dim) ->
      let st', dims = get_array_dims st in
      (st', dim :: dims)

let num_elems_expr st =
  Expr.Helpers.binop_list (get_dims st) Operator.Times
    ~default:(Expr.Helpers.int 1)

let%expect_test "dims" =
  let open Fmt in
  strf "@[%a@]" (list ~sep:comma string)
    (get_dims (SArray (SMatrix (Common.Helpers.AoS, "x", "y"), "z")))
  |> print_endline ;
  [%expect {| z, x, y |}]

(**
 * Return true if SizedType contains an Eigen type
 *)
let rec contains_eigen_type st =
  match st with
  | SInt -> false
  | SReal -> false
  | SVector _ | SRowVector _ | SMatrix _ -> true
  | SArray (t, _) -> contains_eigen_type t

(**
 * Return true if SizedType contains a type tagged SoA
 *)
let rec contains_soa st =
  match st with
  | SInt -> false
  | SReal -> false
  | SVector (SoA, _) | SRowVector (SoA, _) | SMatrix (SoA, _, _) -> true
  | SVector (AoS, _) | SRowVector (AoS, _) | SMatrix (AoS, _, _) -> false
  | SArray (t, _) -> contains_soa t

(**
 * Return the mem_pattern of the SizedType
 *)
let rec get_mem_pattern st =
  match st with
  | SInt | SReal -> Common.Helpers.AoS
  | SVector (SoA, _) | SRowVector (SoA, _) | SMatrix (SoA, _, _) -> SoA
  | SVector (AoS, _) | SRowVector (AoS, _) | SMatrix (AoS, _, _) -> AoS
  | SArray (t, _) -> get_mem_pattern t

(*Given a sizedtype, demote it's mem pattern from SoA to AoS*)
let rec demote_sizedtype_mem st =
  match st with
  | ( SInt | SReal
    | SVector (AoS, _)
    | SRowVector (AoS, _)
    | SMatrix (AoS, _, _) ) as ret ->
      ret
  | SArray (inner_type, dim) -> SArray (demote_sizedtype_mem inner_type, dim)
  | SVector (SoA, dim) -> SVector (AoS, dim)
  | SRowVector (SoA, dim) -> SRowVector (AoS, dim)
  | SMatrix (SoA, dim1, dim2) -> SMatrix (AoS, dim1, dim2)

(*Given a sizedtype, promote it's mem pattern from AoS to SoA*)
let rec promote_sizedtype_mem st =
  match st with
  | (SInt | SReal) as ret -> ret
  | SVector (AoS, dim) -> SVector (SoA, dim)
  | SRowVector (AoS, dim) -> SRowVector (SoA, dim)
  | SMatrix (AoS, dim1, dim2) -> SMatrix (SoA, dim1, dim2)
  | SArray (inner_type, dim) -> SArray (promote_sizedtype_mem inner_type, dim)
  | _ -> st

(*Given a mem_pattern and SizedType, modify the SizedType to AoS or SoA*)
let modify_sizedtype_mem (mem_pattern : Common.Helpers.mem_pattern) st =
  match mem_pattern with
  | Common.Helpers.AoS -> demote_sizedtype_mem st
  | Common.Helpers.SoA -> promote_sizedtype_mem st
