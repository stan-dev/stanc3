open Core_kernel
open Ast

let is_multi_index = function
  | Single {Ast.emeta= {Ast.type_= UArray _; _}; _}
   |Downfrom _ | Upfrom _ | Between _ | All ->
      true
  | Single _ -> false

let remove_trailing_alls_expr = function
  | Indexed (obj, indices) ->
      let rec remove_trailing_alls indices =
        match List.rev indices with
        | All :: tl -> remove_trailing_alls (List.rev tl)
        | _ -> indices
      in
      Indexed (obj, remove_trailing_alls indices)
  | e -> e

(* TODO: same thing for Assignments *)
let rec desugar_index_expr = function
  | Indexed
      ( { expr=
            Indexed (obj, inner_indices)
            (* , Single ({emeta= {type_= UArray UInt; _} as emeta; _} as multi)
               *   :: inner_tl ) *)
        ; emeta }
      , (Single ({emeta= {type_= UInt; _}; _} as single_e) as single)
        :: outer_tl )
    when List.exists ~f:is_multi_index inner_indices -> (
    match List.split_while ~f:(Fn.non is_multi_index) inner_indices with
    | inner_singles, Single first_multi :: inner_tl ->
        (* foo [arr1, ..., arrN] [i1, ..., iN] ->
         foo [arr1[i1]] [arr[i2]] ... [arrN[iN]] *)
        desugar_index_expr
          (Indexed
             ( { expr=
                   Indexed
                     ( obj
                     , inner_singles
                       @ [ Single
                             { expr= Indexed (first_multi, [single])
                             ; emeta= {emeta with type_= UInt} } ]
                       @ inner_tl )
               ; emeta }
             , outer_tl ))
    | inner_singles, Downfrom _ :: inner_tl | inner_singles, All :: inner_tl ->
        (* v[:x][i] -> v[i] *)
        (* v[:][i] -> v[i] *)
        (* XXX generate check *)
        desugar_index_expr
          (Indexed
             ( {expr= Indexed (obj, inner_singles @ [single] @ inner_tl); emeta}
             , outer_tl ))
    | inner_singles, Between (bot, _) :: inner_tl
     |inner_singles, Upfrom bot :: inner_tl ->
        (* v[x:y][z] -> v[x+z-1] *)
        (* XXX generate check *)
        desugar_index_expr
          (Indexed
             ( { expr=
                   Indexed
                     ( obj
                     , inner_singles
                       @ [ Single
                             Frontend_utils.(
                               binop (binop bot Plus single_e) Minus
                                 (loop_bottom bot.emeta)) ]
                       @ inner_tl )
               ; emeta }
             , outer_tl ))
    | inner_singles, [] ->
        raise_s
          [%message
            "We already checked that the 2nd list wasn't empty"
              (inner_singles : typed_expression index list)] )
  | e -> e

let rec map_statement_all_exprs expr_f {stmt; smeta} =
  (* v[:][x] -> v[x]
   * v[x][:] -> v[x] *)
  { stmt= map_statement expr_f (map_statement_all_exprs expr_f) Fn.id Fn.id stmt
  ; smeta }

let rec desugar_expr {expr; emeta} =
  let expr =
    expr |> remove_trailing_alls_expr |> desugar_index_expr
    |> map_expression desugar_expr Fn.id
  in
  {expr; emeta}

let rec desugar_stmt {stmt; smeta} =
  {stmt= map_statement desugar_expr desugar_stmt Fn.id Fn.id stmt; smeta}

let desugar_prog (p : typed_program) : typed_program =
  map_program desugar_stmt p
