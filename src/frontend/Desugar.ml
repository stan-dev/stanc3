open Core_kernel
open Ast

(* XXX Add a section that collapses nested Indexed nodes.
       See https://github.com/stan-dev/stanc3/pull/212#issuecomment-514522092
*)

let is_multi_index = function
  | Single {Ast.emeta= {Ast.type_= UArray _; _}; _}
   |Downfrom _ | Upfrom _ | Between _ | All ->
      true
  | _ -> false

let is_single_index = function
  | Single {Ast.emeta= {Ast.type_= UArray _; _}; _} -> false
  | Single _ -> true
  | _ -> false

let remove_trailing_alls_expr = function
  | Indexed (obj, indices) ->
      let rec remove_trailing_alls indices =
        match List.rev indices with
        | All :: tl -> remove_trailing_alls (List.rev tl)
        | _ -> indices
      in
      Indexed (obj, remove_trailing_alls indices)
  | e -> e

let rec desugar_index_expr = function
  | Indexed
      ( { expr=
            Indexed
              ( obj
              , Single ({emeta= {type_= UArray UInt; _} as emeta; _} as multi)
                :: inner_tl ); _ }
      , (Single {emeta= {type_= UInt; _}; _} as single) :: outer_tl ) ->
      desugar_index_expr
        (Indexed
           ( { expr=
                 Indexed
                   ( obj
                   , Single
                       { expr= Indexed (multi, [single])
                       ; emeta= {emeta with type_= UInt} }
                     :: inner_tl )
             ; emeta }
           , outer_tl ))
  (* v[arr, 2] -> v[arr[2]] *)
  (* foo [arr1, ..., arrN] [i1, ..., iN] -> foo [arr1[i1]] [arr[i2]] ... [arrN[iN]] *)
  (* v[2:3][2] = v[3:2][1] -> v[3] *)
  (* v[x:y][z] -> v[x+z-1] *)
  (* | Between (bot, _) :: tl
   * | Upfrom (bot) :: tl
   *   when List.exists ~f:is_single_index tl -> (
   *     let multis, single_plus = List.split_while ~f:is_multi_index tl in
   *     match single_plus with
   *     | Single e :: tl ->
   *       (\* XXX Need to emit check here that bot + e < top *\)
   *       Single (binop bot Middle.Plus e)
   *       :: reduce_indices (multis @ tl)
   *     | _ -> raise_s [%message "checked that we had a single coming already!"]
   *   )
   * | Downfrom _ :: tl
   *   when List.exists ~f:is_single_index tl -> (
   *     let multis, single_plus = List.split_while ~f:is_multi_index tl in
   *     match single_plus with
   *     | single :: tl -> single
   *       :: reduce_indices (multis @ tl)
   *     | _ -> raise_s [%message "checked that we had a single coming already!"]
   *   ) *)
  
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
  let expr =
    expr |> remove_trailing_alls_expr |> desugar_index_expr
    |> map_expression desugar_expr
  in
  {expr; emeta}

let rec is_indexing_matrix = function
  | Middle.UArray t, idc :: idcs when is_single_index idc ->
      is_indexing_matrix (t, idcs)
  | UMatrix, _ -> true
  | _ -> false

let rec desugar_stmt {stmt; smeta} =
  {stmt= map_statement desugar_expr desugar_stmt Fn.id stmt; smeta}

let desugar_prog (p : typed_program) : typed_program =
  map_program desugar_stmt p
