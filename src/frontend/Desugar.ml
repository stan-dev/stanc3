open Core_kernel
open Ast

(* XXX Add a section that collapses nested Indexed nodes.
       See https://github.com/stan-dev/stanc3/pull/212#issuecomment-514522092
*)

let internal_funapp ifn args emeta =
  let open Middle in
  let id = {name= string_of_internal_fn ifn; id_loc= no_span} in
  {Ast.expr= Ast.FunApp (CompilerInternal, id, args); emeta}

let len_expr e = internal_funapp Middle.FnLength [e] e.emeta

let rec multi_indices_to_new_var decl_id indices assign_indices rhs_indices
    (emeta : Ast.typed_expr_meta) (obj : Ast.typed_expression) =
  (* Deal with indexing by int idx array and indexing by range *)
  let smeta = {loc= obj.emeta.loc; return_type= NoReturnType} in
  match indices with
  | [] ->
      [ { stmt=
            Assignment
              { assign_lhs=
                  { assign_identifier= decl_id
                  ; assign_indices
                  ; assign_meta=
                      { id_ad_level= obj.emeta.ad_level
                      ; lhs_ad_level= emeta.ad_level
                      ; lhs_type_= emeta.type_
                      ; id_type_= emeta.type_
                      ; loc= obj.emeta.loc } }
              ; assign_op= Assign
              ; assign_rhs= {expr= Indexed (obj, rhs_indices); emeta= obj.emeta}
              }
        ; smeta } ]
  | Ast.Single ({Ast.emeta= {Ast.type_= UArray _; _}; _} as idx_arr) :: tl ->
      let var = {expr= Variable decl_id; emeta} in
      let resize_stmt =
        { stmt=
            Assignment
              { assign_lhs=
                  { assign_identifier= decl_id
                  ; assign_indices
                  ; assign_meta=
                      { id_ad_level= obj.emeta.ad_level
                      ; lhs_ad_level= emeta.ad_level
                      ; lhs_type_= emeta.type_
                      ; id_type_= emeta.type_
                      ; loc= obj.emeta.loc } }
              ; assign_op= Assign
              ; assign_rhs=
                  internal_funapp FnResizeToMatch
                    [{expr= Indexed (var, assign_indices); emeta}; idx_arr]
                    emeta }
        ; smeta= {loc= emeta.loc; return_type= NoReturnType} }
      in
      let loopvar, reset = Middle.gensym_enter () in
      let lv_idx =
        Single
          { expr= Variable {name= loopvar; id_loc= emeta.loc}
          ; emeta= {emeta with type_= UInt} }
      in
      let assign_indices = assign_indices @ [lv_idx] in
      let wrap_idx i =
        Single {expr= Indexed (idx_arr, [i]); emeta= {emeta with type_= UInt}}
      in
      let rhs_indices = List.map ~f:wrap_idx assign_indices in
      let r =
        match
          multi_indices_to_new_var decl_id tl assign_indices rhs_indices emeta
            obj
        with
        | [] -> []
        | body ->
            [ resize_stmt
            ; { stmt=
                  ForEach
                    ( {name= loopvar; id_loc= emeta.loc}
                    , idx_arr
                    , {stmt= Block body; smeta} )
              ; smeta } ]
      in
      reset () ; r
  | _ -> []

let is_multi_index = function
  | Single {Ast.emeta= {Ast.type_= UArray _; _}; _}
  (* | Downfrom _ | Upfrom _ | Between _ | All  *) ->
      true
  | _ -> false

(* This function will transform multi-indices into statements that create
   a new var containing the result of the multi-index (and replace that
   index expression with the var). After the function is run on a statement,
   there should be no further references to Ast.Downfrom, Upfrom, Between,
   or Single with an array-type index var.

   We'll use a ref to keep track of the statements we want to add before
   this one and update the ref inside.
*)
let rec pull_new_multi_indices_expr new_stmts
    ({expr; emeta} : typed_expression) =
  match expr with
  (* TODO: Add check_range NRFunApps to generated code. *)
  | Indexed (obj, indices) when List.exists ~f:is_multi_index indices -> (
      let obj = pull_new_multi_indices_expr new_stmts obj in
      let name = Middle.gensym () in
      let type_ =
        Semantic_check.inferred_unsizedtype_of_indexed_exn emeta.type_
          ~loc:emeta.loc indices
      in
      let identifier = {name; id_loc= emeta.loc} in
      let var =
        { expr= Ast.Variable {name; id_loc= emeta.loc}
        ; emeta= {emeta with type_} }
      in
      match multi_indices_to_new_var identifier indices [] [] emeta obj with
      | [] -> {expr; emeta}
      | filling_fors ->
          new_stmts :=
            !new_stmts
            @ [ { stmt=
                    VarDecl
                      { decl_type= Unsized type_
                      ; transformation= Identity
                      ; identifier
                      ; initial_value= None
                      ; is_global= false }
                ; smeta= {loc= emeta.loc; return_type= NoReturnType} } ]
            @ filling_fors ;
          var )
  | _ ->
      {expr= map_expression (pull_new_multi_indices_expr new_stmts) expr; emeta}

let rec reduce_indices = function
  (* v[arr][2] -> v[arr[2]] *)
  | Single ({emeta= {type_= UArray UInt; _}; _} as obj)
    :: Single ({emeta= {type_= UInt; _} as emeta; _} as idc) :: tl ->
      Single {expr= Indexed (obj, [Single idc]); emeta} :: reduce_indices tl
  (* v[2:3][2] = v[3:2][1] -> v[3] *)
  (* | Between (lower, upper) :: Single {emeta={type_=UArray UInt; _}; _} :: tl ->
   *   Single (BinOp ) *)
  | x :: tl -> x :: reduce_indices tl
  | [] -> []

let rec desugar_index_expr (e : typed_expression) =
  let ast_expr expr = {e with expr} in
  match e.expr with
  (* mat[2] -> row(m, 2)*)
  | Ast.Indexed
      ( ({emeta= {type_= UMatrix; _}; _} as obj)
      , [Single ({emeta= {type_= UInt; _}; _} as i)] ) ->
      Ast.FunApp (StanLib, {name= "row"; id_loc= e.emeta.loc}, [obj; i])
      |> ast_expr
  | Indexed (obj, indices) ->
      {e with expr= Indexed (obj, reduce_indices indices)}
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
  | _ -> map_expression desugar_index_expr e.expr |> ast_expr

let is_single_index = function
  | Single {Ast.emeta= {Ast.type_= UArray _; _}; _} -> false
  | Single _ -> true
  | _ -> false

let infer_type_of_indexed (base_emeta : typed_expr_meta) indices =
  Semantic_check.inferred_unsizedtype_of_indexed_exn base_emeta.type_
    ~loc:base_emeta.loc indices

let rec split_single_index_lists = function
  | {expr= Indexed (obj, indices); emeta} as e -> (
    match List.rev indices with
    | Single ({emeta= {type_= UInt; _}; _} as c)
      :: Single ({emeta= {type_= UInt; _}; _} as r) :: hd
      when infer_type_of_indexed obj.emeta (List.rev hd) = UMatrix ->
        let obj =
          {expr= Indexed (obj, List.rev hd); emeta= {emeta with type_= UMatrix}}
        in
        internal_funapp FnMatrixElement [obj; r; c] emeta
    | _ when List.length indices > 1 && List.for_all ~f:is_single_index indices
      ->
        List.fold
          ~f:(fun accum idx ->
            { expr= Indexed (accum, [idx])
            ; emeta= {emeta with type_= infer_type_of_indexed accum.emeta [idx]}
            } )
          ~init:obj indices
    | _ -> e )
  | e -> {e with expr= map_expression split_single_index_lists e.expr}

let rec map_statement_all_exprs expr_f {stmt; smeta} =
  { stmt= map_statement expr_f (map_statement_all_exprs expr_f) Fn.id stmt
  ; smeta }

let desugar_stmt s =
  let new_stmts = ref [] in
  let desugar_expr e =
    e |> split_single_index_lists |> desugar_index_expr
    |> pull_new_multi_indices_expr new_stmts
  in
  !new_stmts @ [map_statement_all_exprs desugar_expr s]

let desugar_prog = stmt_concat_map_prog desugar_stmt
