open Core_kernel
open Ast

(* XXX Add a section that collapses nested Indexed nodes.
       See https://github.com/stan-dev/stanc3/pull/212#issuecomment-514522092
*)

let is_multi_index = function
  | Single {Ast.emeta= {Ast.type_= UArray _; _}; _}
  (* | Downfrom _ | Upfrom _ | Between _ | All  *) ->
      true
  | _ -> false

let is_single_index = function
  | Single {Ast.emeta= {Ast.type_= UArray _; _}; _} -> false
  | Single _ -> true
  | _ -> false

let rec reduce_indices = function
  (* v[arr, 2] -> v[arr[2]] *)
  (* foo [arr1, ..., arrN] [i1, ..., iN] -> foo [arr1[i1]] [arr[i2]] ... [arrN[iN]] *)
  | Single ({emeta= {type_= UArray UInt; _} as emeta; _} as first_multi) :: tl
    when List.exists ~f:is_single_index tl -> (
      let multis, single_plus = List.split_while ~f:is_multi_index tl in
      match single_plus with
      | single :: tl ->
          Single
            { expr= Indexed (first_multi, [single])
            ; emeta= {emeta with type_= UInt} }
          :: reduce_indices (multis @ tl)
      | _ -> raise_s [%message "checked that we had a single coming already!"]
      )
  (* v[2:3][2] = v[3:2][1] -> v[3] *)
  (* | Between (lower, upper) :: Single {emeta={type_=UArray UInt; _}; _} :: tl ->
   *   Single (BinOp ) *)
  
  (* v[arr][2] -> v[arr[2]] *)
  | x :: tl -> x :: reduce_indices tl
  | [] -> []

let desugar_index_expr = function
  (* mat[2] -> row(m, 2)*)
  | Indexed (obj, indices) -> Indexed (obj, reduce_indices indices)
  (*
https://github.com/stan-dev/stanc3/pull/212

v[2][4] -> v[2, 4]
v[:][x] -> v[x]
v[x][:] -> v[x]
v[2][2:3] -> segment(v[2], 2, 3)
v[2][arr] -> (declare new_sym, fill with v[2][arr] via for loop); new_sym
v[x][3:2] -> v[x][{3, 2}]
v[2][arr][3] -> v[2, arr[3]]

m[2][3] -> m[2, 3]
m[2:3] = m[2:3, :] -> block(m, 2, 1, 2, cols(m))
m[:, 2:3] -> block(m, 1, 2, rows(m), 2)
m[2:4][1:2] -> m[2:3]
m[2:3, 2] -> (declare newsym, fill with rows 2-3 and column 2 via for loop); newsym
   *)
  | e -> e

let rec map_statement_all_exprs expr_f {stmt; smeta} =
  { stmt= map_statement expr_f (map_statement_all_exprs expr_f) Fn.id stmt
  ; smeta }

let rec desugar_expr {expr; emeta} =
  let expr = expr |> desugar_index_expr |> map_expression desugar_expr in
  {expr; emeta}

let rec is_indexing_matrix = function
  | Middle.UArray t, idc :: idcs when is_single_index idc ->
      is_indexing_matrix (t, idcs)
  | UMatrix, _ -> true
  | _ -> false

let rec desugar_stmt {stmt; smeta} =
  {stmt= map_statement desugar_expr desugar_stmt Fn.id stmt; smeta}

let desugar_prog p = map_program desugar_stmt p
