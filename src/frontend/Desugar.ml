open Core_kernel
open Ast

(* XXX Add a section that collapses nested Indexed nodes.
       See https://github.com/stan-dev/stanc3/pull/212#issuecomment-514522092
*)

let rec multi_indices_to_new_var decl_id indices assign_indices rhs_indices
    (emeta : Ast.typed_expr_meta) (obj : Ast.typed_expression) =
  (* Deal with indexing by int idx array and indexing by range *)
  let smeta = {loc= obj.emeta.loc; return_type= NoReturnType} in
  match indices with
  | [] ->
    [{ stmt=
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
     ; smeta}]
  | Ast.Single ({Ast.emeta= {Ast.type_= UArray _; _}; _} as idx_arr) :: tl ->
    print_endline "hi" ;
    Fmt.pr "@,@[<v>%a]@," Pretty_printing.pp_expression obj ;
      let loopvar, reset = Middle.gensym_enter () in
      let lv_idx =
        Single
          { expr= Variable {name= loopvar; id_loc= emeta.loc}
          ; emeta= {emeta with type_= UInt} }
      in
      let assign_indices = assign_indices @ [lv_idx] in
      let wrap_idx i = Single {expr= Indexed (idx_arr, [i]); emeta} in
      let rhs_indices = List.map ~f:wrap_idx assign_indices in
      let r =
        [{ stmt=
             ForEach
               ( {name= loopvar; id_loc= emeta.loc}
               , idx_arr
               , {stmt=Block
                   (multi_indices_to_new_var decl_id tl assign_indices
                      rhs_indices emeta obj); smeta} )
         ; smeta}]
      in
      reset () ; r
  | _ -> []

let is_multi_index = function
  | Single {Ast.emeta= {Ast.type_= UArray _; _}; _}
  (* | Downfrom _ | Upfrom _ | Between _ | All  *)
    -> true
  | _ -> false

(* This function will transform multi-indices into statements that create
   a new var containing the result of the multi-index (and replace that
   index expression with the var). After the function is run on a statement,
   there should be no further references to Ast.Downfrom, Upfrom, Between,
   or Single with an array-type index var.

   We'll use a ref to keep track of the statements we want to add before
   this one and update the ref inside.
*)
let rec pull_new_multi_indices_expr new_stmts ({expr; emeta} : typed_expression) =
  match expr with
  | Indexed (obj, indices)
      when List.exists ~f:is_multi_index indices ->
      Fmt.(pr "@[<v>%a]" (list ~sep:comma Pretty_printing.pp_index) indices) ;
      let obj = pull_new_multi_indices_expr new_stmts obj in
      let name = Middle.gensym () in
      let decl_type =
        Middle.Unsized
          (Semantic_check.inferred_unsizedtype_of_indexed_exn
             emeta.type_ ~loc:emeta.loc indices)
      in
      let identifier = {name; id_loc= emeta.loc} in
      new_stmts :=
        !new_stmts
        @ [ { stmt=
                VarDecl
                  { decl_type
                  ; transformation= Identity
                  ; identifier
                  ; initial_value= None
                  ; is_global= false }
            ; smeta= {loc= emeta.loc; return_type= NoReturnType} } ]
        @ multi_indices_to_new_var identifier indices [] [] emeta obj ;
      {expr= Ast.Variable {name; id_loc= emeta.loc}; emeta= emeta}
  | _ -> {expr=map_expression (pull_new_multi_indices_expr new_stmts) expr; emeta}

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
    | _ -> raise_s [%message "Didn't get an assignment"]
  in
  let new_stmts = ref [] in
  let res = pull_new_multi_indices_expr new_stmts rhs in
  print_endline
    Fmt.(strf "replacement expr: @[<v>%a@]" Pretty_printing.pp_expression res) ;
  print_endline
    Fmt.(
      strf "AST statements:@, @[<v>%a@]"
        (list ~sep:cut Pretty_printing.pp_statement)
        !new_stmts) ;
  [%expect
    {|
    replacement expr: sym1__
    AST statements:
     matrix sym1__;
     for (sym2__ in indices)
       for (sym3__ in indices)
         sym1__[sym2__, sym3__] = mat[indices[sym2__], indices[sym3__]]; |}]

let desugar_stmt {stmt=s; smeta} =
  let new_stmts = ref [] in
  let stmt = map_statement (pull_new_multi_indices_expr new_stmts) Fn.id Fn.id s in
  !new_stmts @ [{stmt; smeta}]

let desugar_prog = Ast.stmt_concat_map_prog desugar_stmt

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
