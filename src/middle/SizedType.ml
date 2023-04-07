(** Types which have a concrete size associated, e.g. [vector\[n\]] *)

open Core_kernel

type 'a t =
  | SInt
  | SReal
  | SComplex
  | SVector of Mem_pattern.t * 'a
  | SRowVector of Mem_pattern.t * 'a
  | SMatrix of Mem_pattern.t * 'a * 'a
  | SComplexVector of 'a
  | SComplexRowVector of 'a
  | SComplexMatrix of 'a * 'a
  | SArray of 'a t * 'a
  | STuple of 'a t list
[@@deriving sexp, compare, map, hash, fold]

let rec pp pp_e ppf = function
  | SInt -> Fmt.string ppf "int"
  | SReal -> Fmt.string ppf "real"
  | SComplex -> Fmt.string ppf "complex"
  | SVector (_, expr) -> Fmt.pf ppf "vector%a" (Fmt.brackets pp_e) expr
  | SRowVector (_, expr) -> Fmt.pf ppf "row_vector%a" (Fmt.brackets pp_e) expr
  | SComplexVector expr ->
      Fmt.pf ppf "complex_vector%a" (Fmt.brackets pp_e) expr
  | SComplexRowVector expr ->
      Fmt.pf ppf "complex_row_vector%a" (Fmt.brackets pp_e) expr
  | SMatrix (_, d1_expr, d2_expr) ->
      Fmt.pf ppf "matrix%a"
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
  | SComplexMatrix (d1_expr, d2_expr) ->
      Fmt.pf ppf "complex_matrix%a"
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
  | SArray (st, expr) ->
      Fmt.pf ppf "array%a"
        Fmt.(pair ~sep:comma (fun ppf st -> pp pp_e ppf st) pp_e |> brackets)
        (st, expr)
  | STuple ts -> Fmt.pf ppf "tuple(@[%a@])" Fmt.(list ~sep:comma (pp pp_e)) ts

let rec to_unsized = function
  | SInt -> UnsizedType.UInt
  | SReal -> UReal
  | SComplex -> UComplex
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SComplexVector _ -> UComplexVector
  | SComplexRowVector _ -> UComplexRowVector
  | SComplexMatrix _ -> UComplexMatrix
  | SArray (t, _) -> UArray (to_unsized t)
  | STuple ts -> UTuple (List.map ~f:to_unsized ts)

let rec inner_type st = match st with SArray (t, _) -> inner_type t | t -> t

(**
 Get the dimensions with respect to sizes needed for IO.
 {b Note}: The main difference from get_dims is complex,
 where this function treats the complex type as a dual number.
 *)
let rec get_dims_io st =
  let two = Expr.Helpers.int 2 in
  match st with
  | SInt | SReal -> []
  | SComplex -> [two]
  | SVector (_, d) | SRowVector (_, d) -> [d]
  | SMatrix (_, dim1, dim2) -> [dim1; dim2]
  | SComplexVector d | SComplexRowVector d -> [d; two]
  | SComplexMatrix (dim1, dim2) -> [dim1; dim2; two]
  | SArray (t, dim) -> dim :: get_dims_io t
  | STuple ts ->
      (* TUPLE TODO get_dims_io

         THIS IS WRONG FOR TUPLES
         We should use io_size wherever possible
         and make this a failwith
      *)
      let subts = List.map ~f:get_dims_io ts in
      Expr.Helpers.int (List.length subts) :: List.concat subts

let rec io_size st =
  let two = Expr.Helpers.int 2 in
  match st with
  | SInt | SReal -> Expr.Helpers.one
  | STuple ts ->
      Expr.Helpers.binop_list (List.map ~f:io_size ts) Operator.Plus
        ~default:Expr.Helpers.zero
  | SComplex -> two
  | SVector (_, d) | SRowVector (_, d) -> d
  | SMatrix (_, dim1, dim2) -> Expr.Helpers.binop dim1 Operator.Times dim2
  | SComplexVector d | SComplexRowVector d ->
      Expr.Helpers.binop d Operator.Times two
  | SComplexMatrix (dim1, dim2) ->
      Expr.Helpers.binop dim1 Operator.Times
        (Expr.Helpers.binop dim2 Operator.Times two)
  | SArray (t, dim) -> Expr.Helpers.binop dim Operator.Times (io_size t)

let rec get_dims st =
  match st with
  (* TUPLE STUB get_dims
     How should tuples be expected to behave in this function?
     Same as above?
     Although this one seems to have a sense of recursion
  *)
  | STuple _ | SInt | SReal | SComplex -> []
  | SMatrix (_, d1, d2) | SComplexMatrix (d1, d2) -> [d1; d2]
  | SRowVector (_, dim)
   |SVector (_, dim)
   |SComplexRowVector dim
   |SComplexVector dim ->
      [dim]
  | SArray (t, dim) -> dim :: get_dims t

(**
 * Check whether a SizedType holds indexable SizedTypes.
 *)
let is_recursive_container st =
  match st with
  | SInt | SReal | SComplex | SVector _ | SRowVector _ | SMatrix _
   |SArray ((SInt | SReal), _)
   |SComplexMatrix _ | SComplexRowVector _ | SComplexVector _ ->
      false
  | SArray _ -> true
  | STuple _ -> true

(** Return a type's array dimensions and the type inside the (possibly nested) array *)
let rec get_array_dims st =
  match st with
  | SInt | SReal | SComplex | STuple _ -> (st, [])
  | SVector (_, d) | SRowVector (_, d) | SComplexVector d | SComplexRowVector d
    ->
      (st, [d])
  | SMatrix (_, d1, d2) | SComplexMatrix (d1, d2) -> (st, [d1; d2])
  | SArray (st, dim) ->
      let st', dims = get_array_dims st in
      (st', dim :: dims)

let rec internal_scalar st =
  match st with
  | SInt | SReal | SComplex | STuple _ -> st
  | SVector _ | SRowVector _ | SMatrix _ -> SReal
  | SComplexVector _ | SComplexRowVector _ | SComplexMatrix _ -> SComplex
  | SArray (t, _) -> internal_scalar t

let num_elems_expr st =
  Expr.Helpers.binop_list (get_dims_io st) Operator.Times
    ~default:(Expr.Helpers.int 1)

let%expect_test "dims" =
  let open Fmt in
  str "@[%a@]" (list ~sep:comma string)
    (List.map
       ~f:(fun Expr.Fixed.{pattern; _} ->
         match pattern with Expr.Fixed.Pattern.Lit (_, x) -> x | _ -> "fail" )
       (get_dims_io
          (SArray
             ( SMatrix
                 (Mem_pattern.AoS, Expr.Helpers.str "x", Expr.Helpers.str "y")
             , Expr.Helpers.str "z" ) ) ) )
  |> print_endline ;
  [%expect {| z, x, y |}]

let rec contains_tuple st =
  match st with
  | STuple _ -> true
  | SArray (st, _) -> contains_tuple st
  | _ -> false

let is_complex_type st = UnsizedType.is_complex_type (to_unsized st)

(**
 * Return the mem_pattern of the SizedType
 *)
let rec get_mem_pattern st =
  match st with
  | SInt | SReal | SComplex | SComplexVector _ | SComplexRowVector _
   |SComplexMatrix _ | STuple _ ->
      Mem_pattern.AoS
  | SVector (mem, _) | SRowVector (mem, _) | SMatrix (mem, _, _) -> mem
  | SArray (t, _) -> get_mem_pattern t

(*Given a sizedtype, demote it's mem pattern from SoA to AoS*)
let rec demote_sizedtype_mem st =
  match st with
  | ( SInt | SReal | SComplex
    | SVector (AoS, _)
    | SRowVector (AoS, _)
    | SMatrix (AoS, _, _)
    | SComplexVector _ | SComplexRowVector _
    | SComplexMatrix (_, _) ) as ret ->
      ret
  | SArray (inner_type, dim) -> SArray (demote_sizedtype_mem inner_type, dim)
  | SVector (SoA, dim) -> SVector (AoS, dim)
  | SRowVector (SoA, dim) -> SRowVector (AoS, dim)
  | SMatrix (SoA, dim1, dim2) -> SMatrix (AoS, dim1, dim2)
  | STuple ts -> STuple (List.map ~f:demote_sizedtype_mem ts)

(*Given a sizedtype, promote it's mem pattern from AoS to SoA*)
let rec promote_sizedtype_mem st =
  match st with
  | SVector (AoS, dim) -> SVector (SoA, dim)
  | SRowVector (AoS, dim) -> SRowVector (SoA, dim)
  | SMatrix (AoS, dim1, dim2) -> SMatrix (SoA, dim1, dim2)
  | SArray (inner_type, dim) -> SArray (promote_sizedtype_mem inner_type, dim)
  | _ -> st

(*Given a mem_pattern and SizedType, modify the SizedType to AoS or SoA*)
let modify_sizedtype_mem (mem_pattern : Mem_pattern.t) st =
  match mem_pattern with
  | AoS -> demote_sizedtype_mem st
  | SoA -> promote_sizedtype_mem st

let rec has_mem_pattern = function
  | SInt | SReal | SComplex | SComplexVector _ | SComplexRowVector _
   |SComplexMatrix _ ->
      false
  | SVector _ | SRowVector _ | SMatrix _ -> true
  | SArray (t, _) -> has_mem_pattern t
  | STuple ts -> List.exists ~f:has_mem_pattern ts

(** NB: This is not quite the inverse of [get_array_dims]
    you need to reverse [dims] first.
*)
let rec build_sarray dims st =
  match dims with [] -> st | d :: dims -> build_sarray dims (SArray (st, d))

let flatten_tuple_io st =
  let rec loop st =
    match st with
    | STuple ts -> List.concat_map ~f:loop ts
    | SArray _ when contains_tuple st ->
        let tupl, dims = get_array_dims st in
        List.map ~f:(fun t -> build_sarray (List.rev dims) t) (loop tupl)
    | _ -> [st] in
  loop st

let%expect_test "dims" =
  let st : Expr.Typed.t t =
    SArray (SArray (SReal, Expr.Helpers.variable "N"), Expr.Helpers.one) in
  let sclr, dims = get_array_dims st in
  let st2 = build_sarray (List.rev dims) sclr in
  let open Fmt in
  pf stdout "%a = %a" (pp Expr.Typed.pp) st (pp Expr.Typed.pp) st2 ;
  [%expect {| array[array[real, N], 1] = array[array[real, N], 1] |}]
