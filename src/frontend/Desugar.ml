open Core_kernel
open Ast

(* XXX Add a section that collapses nested Indexed nodes.
       See https://github.com/stan-dev/stanc3/pull/212#issuecomment-514522092
*)

let rec multi_indices_to_new_var decl_id indices lhs_idcs
    (emeta : Ast.typed_expr_meta) (obj : Ast.typed_expression) =
  (* Deal with indexing by int idx array and indexing by range *)
  let loopvar, reset = Middle.gensym_enter () in
  let lv_idx =
    Single
      { expr= Variable {name= loopvar; id_loc= emeta.loc}
      ; emeta= {emeta with type_= UInt} } in
  let assign_indices = lhs_idcs @ [lv_idx] in
  match indices with
  | Ast.Single ({Ast.emeta= {Ast.type_= UArray _; _}; _} as idx_arr) :: tl ->
      if List.is_empty tl then
        let rhs_indices =
          List.map
            ~f:(fun i ->
              Single
                {expr= Indexed ({expr= Variable decl_id; emeta}, [i]); emeta})
            lhs_idcs in
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
                  {expr= Indexed (obj, rhs_indices); emeta= idx_arr.emeta} }
        ; smeta= emeta.loc }
      else
        let r =
          { stmt=
              ForEach
                ( {name= loopvar; id_loc= emeta.loc}
                , idx_arr
                , multi_indices_to_new_var decl_id tl assign_indices emeta obj
                )
          ; smeta= obj.emeta.loc } in
        reset () ; r
  | _ -> {stmt= Skip; smeta= emeta.loc}

let discard_one_outer_dim = function
  | Middle.SArray (t, _) -> t
  | SMatrix (_, cols) -> SVector cols
  | SVector _ | SRowVector _ -> SReal
  | SReal | SInt -> raise_s [%message "Can't discard outer dimension."]

let rec infer_sizedtype_of_indexed obj_st index =
  match index with
  | Single {emeta= {type_= UInt; _}; _} -> discard_one_outer_dim obj_st
  | _ -> obj_st

let rec find_sizedtype_of_expr lookup e =
  match e.expr with
  | Variable v -> lookup v
  | Indexed (obj, indices) ->
      List.fold ~f:infer_sizedtype_of_indexed
        ~init:(find_sizedtype_of_expr lookup obj)
        indices
  | GetLP | GetTarget | CondDistApp _ | RealNumeral _ -> SReal
  | IntNumeral _ -> SInt

(* This function will transform multi-indices into statements that create
   a new var containing the result of the multi-index (and replace that
   index expression with the var). After the function is run on a statement,
   there should be no further references to Ast.Downfrom, Upfrom, Between,
   or Single with an array-type index var.

   We'll use a ref to keep track of the statements we want to add before
   this one and update the ref inside.
*)
let rec pull_new_multi_indices_expr new_stmts (e : typed_expression) :
    typed_expression =
  match e.Ast.expr with
  | Indexed (obj, indices) ->
      let obj = pull_new_multi_indices_expr new_stmts obj in
      let name = Middle.gensym () in
      let decl_type =
        Semantic_check.inferred_unsizedtype_of_indexed_exn ~loc:e.emeta.loc
          e.emeta.type_ indices in
      new_stmts :=
        !new_stmts
        @ [ { stmt=
                VarDecl
                  { decl_type
                  ; transformation= Identity
                  ; identifier= {name; id_loc= emeta.loc}
                  ; initial_value= None
                  ; is_global= false }
            ; smeta= e.emeta.loc } ]
        @ multi_indices_to_new_var name indices e.emeta obj ;
      {expr= Ast.Variable {name; id_loc= e.emeta.loc}; emeta= e.emeta}
  | _ -> e

let%expect_test "pull out multi indices" =
  let {Ast.transformeddatablock= td; _} =
    Frontend_utils.typed_ast_of_string_exn
      {| transformed data {
    int indices[3] = {1, 2, 3};
    matrix[20, 21] mat[30];
    mat = mat[indices, indices];
} |}
  in
  let lst_stmt = List.last_exn (Option.value_exn td) in
  let rhs =
    match lst_stmt.Ast.stmt with
    | Ast.Assignment {assign_rhs; _} -> assign_rhs
    | _ -> raise_s [%message "Didn't get an assignment"] in
  let new_stmts = ref [] in
  let res = pull_new_multi_indices_expr new_stmts rhs in
  print_endline
    Fmt.(strf "replacement expr: @[<v>%a@]" Pretty_printing.pp_expression res) ;
  print_endline
    Fmt.(
      strf "Mir statements:@, @[<v>%a@]"
        (list ~sep:cut Pretty.pp_stmt_loc)
        !new_stmts) ;
  [%expect
    {|
    ("Indexed: "
     (index_array
      ((expr (Var indices))
       (emeta ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))))
    replacement expr: sym1__
    Mir statements:
     data matrix[] sym1__;
     for(sym2__ in 1:FnLength__(indices)) {
       for(sym3__ in 1:FnLength__(indices)) {
         sym1__[sym3__] = indices[sym2__][indices[sym3__]];
       }
     } |}]

(*
let desugar_index_expr (e: Ast.typed_expression) =
  let ast_expr expr = {Ast.expr; Ast.emeta= e.emeta} in
  match e.expr with
  (* mat[2] -> row(m, 2) *)
  | Ast.Indexed ({emeta={type_=UMatrix; _}; _ } as obj, [Single i]) ->
    Ast.FunApp (StanLib, {Ast.name="row"; id_loc=e.Ast.emeta.loc}, [obj; i])
    |> ast_expr
    (*
https://github.com/stan-dev/stanc3/pull/212

v[2:3][2] = v[3:2][1] -> v[3]
v[2][4] -> v[2, 4]
v[:][x] -> v[x]
v[x][:] -> v[x]
v[2][2:3] -> segment(v[2], 2, 3)
v[arr][2] -> (declare new_sym, fill with v[arr] via for loop); new_sym[2]
v[2][arr] -> (declare new_sym, fill with v[2][arr] via for loop); new_sym
v[x][3:2] -> v[x][{3, 2}]

m[2][3] -> m[2, 3]
m[2:3] = m[2:3, :] -> block(m, 2, 1, 2, cols(m))
m[:, 2:3] -> block(m, 1, 2, rows(m), 2)
m[2:4][1:2] -> m[2:3]
*)
  | _ -> e
 *)

(*
  let stmt = match stmt with
  | Assignment ((vident, indices), rhs)
    when List.exists ~f:(function Single _ -> false | _ -> true) indices ->

  | TargetPE e -> (??)
  | NRFunApp (ft, fname, args) -> (??)
  | Return (Some { expr; emeta }) -> (??)
  | IfElse ({ expr; emeta }, iftrue, iffalse) -> (??)
  | While ({ expr; emeta }, body) -> (??)
  | For { loopvar; lower; upper; body } -> (??)
  | _ -> map_statement Fn.id ull_multi_index_stmts
  in {stmt; smeta}
 *)
