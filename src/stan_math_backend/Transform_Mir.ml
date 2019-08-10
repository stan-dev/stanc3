open Core_kernel
open Middle
open Expr_helpers
open Stmt_helpers

let pos = "pos__"

let add_pos_reset stmt =
  match Stmt.Fixed.pattern stmt with
  | For {body; _} when contains_internal_fun ~fn:FnReadData body ->
      [ Stmt.(assign (Fixed.meta stmt) pos Expr.(loop_bottom Typed.Meta.empty))
      ; stmt ]
  | _ -> [stmt]

let rec unwind stmt =
  match Stmt.Fixed.pattern stmt with
  | For {loopvar; lower; upper; body} when is_block body -> (
    match block_statements body with
    | [body] ->
        let final, args = unwind body in
        (final, (loopvar, lower, upper) :: args)
    | _ -> (stmt, []) )
  | _ -> (stmt, [])

let transform_for stmt =
  match Stmt.Fixed.proj stmt with
  | meta, For {body; _}
    when contains_internal_fun ~fn:FnReadData body
           || contains_internal_fun ~fn:FnWriteParam body ->
      let final, args = unwind stmt in
      let stmt =
        List.fold ~init:final
          ~f:(fun accum (loopvar, lower, upper) ->
            let body = block meta [accum] in
            let loop = for_ meta loopvar lower upper body in
            loop )
          args
      in
      Either.first stmt
  | _ -> Either.second stmt

let invert_read_fors stmt =
  Stmt.Fixed.transform_partial Either.first transform_for stmt

let mock_for n body =
  for_ () "lv" (lit_int () 1) (lit_int () n) body

let%expect_test "invert write fors" =
  let stmt =
    mock_for 8 (mock_for 9 (internal_nrfun () FnWriteParam []))
  in
  invert_read_fors stmt |> Fmt.strf "%a" Stmt.NoMeta.pp |> print_endline ;
  [%expect
    {|
    for(lv in 0:9) { for(lv in 0:8) {
                       FnWriteParam__();
                     }
    } |}]

let use_pos_in_readdata stmt =
  let transform stmt =
    match Stmt.Fixed.proj2 stmt with
    | block_meta, Block [(assign_meta, Assignment ((ident, idxs), rhs))] -> (
      match Expr.Fixed.proj2 rhs with
      | ( index_meta
        , Indexed (((_, FunApp (CompilerInternal, fn_name, _)) as fun_app), _)
        )
        when fn_name = Internal_fun.to_string FnReadData ->
          let pos_var = var index_meta pos in
          let rhs1 =
            index_single index_meta (Expr.Fixed.inj fun_app) ~idx:pos_var
          and rhs2 = incr pos_var in
          let stmts =
            [ assign assign_meta ident ~idxs rhs1
            ; assign assign_meta pos rhs2 ]
          in
          block block_meta stmts
      | _ -> stmt )
    | _ -> stmt
  in
  Stmt.Fixed.transform_bottom_up Fn.id transform stmt

let%expect_test "xform_readdata" =
  let idx v = Expr.Single (var () v)
  and read = internal_fun () FnReadData [] in
  let idxs = [idx "i"; idx "j"; idx "k"] in
  let indexed = indexed () read idxs in
  let stmt = assign () "v" ~idxs indexed in
  let loop = mock_for 7 (mock_for 8 (mock_for 9 stmt)) in
  use_pos_in_readdata loop
  |> Fmt.strf "@[<h>%a@]" Stmt.NoMeta.pp
  |> print_endline ;
  [%expect
    {|
    for(lv in 0:7) { for(lv in 0:8) {
                       for(lv in 0:9) {
                         v[i, j, k] = FnReadData__()[pos__];
                         pos__ = (pos__ + 1);
                       }
                     } } |}]

let escape_name str =
  str
  |> String.substr_replace_all ~pattern:"." ~with_:"_"
  |> String.substr_replace_all ~pattern:"-" ~with_:"_"

let add_jacobians stmt =
  let transform stmt =
    match Stmt.Fixed.proj stmt with
    | assign_meta, Assignment ((ident, idxs), rhs) -> (
      match Expr.Fixed.proj rhs with
      | fun_meta, FunApp (CompilerInternal, fn_name, args)
        when fn_name = Internal_fun.to_string FnConstrain ->
          let new_rhs =
            internal_fun fun_meta FnConstrain
              (args @ [var fun_meta "lp__"])
          in
          let new_stmt = assign assign_meta ident ~idxs new_rhs in
          if_ assign_meta
            (var fun_meta "jacobian__")
            new_stmt (Some stmt)
      | _ -> stmt )
    | _ -> stmt
  in
  Stmt.Fixed.transform_bottom_up Fn.id transform stmt

let add_read_data_vestigial_indices stmt =
  let transform stmt =
    match Stmt.Fixed.proj stmt with
    | smeta, Assignment ((ident, idxs), rhs) -> (
      match Expr.Fixed.proj rhs with
      | emeta, FunApp (CompilerInternal, f, _)
        when f = Internal_fun.to_string FnReadData ->
          let with_vestigial_idx =
            
              index_single emeta rhs ~idx:(loop_bottom Expr.Typed.Meta.empty)
          in
          assign smeta ident ~idxs with_vestigial_idx
      | _ -> stmt )
    | _ -> stmt
  in
  Stmt.Fixed.transform_bottom_up Fn.id transform stmt

let ensure_body_in_block stmt =
  let wrap stmt =
    match Stmt.Fixed.proj stmt with
    | _, Block _ -> stmt
    | meta, SList xs -> block meta xs
    | meta, _ -> block meta [stmt]
  in
  let transform stmt =
    match Stmt.Fixed.proj stmt with
    | meta, While (pred, body) -> while_ meta pred @@ wrap body
    | meta, For {loopvar; lower; upper; body} ->
        for_ meta loopvar lower upper @@ wrap body
    | meta, IfElse (pred, s, t) ->
        if_ meta pred (wrap s) (Option.map ~f:wrap t)
    | _ -> stmt
  in
  Stmt.Fixed.transform_bottom_up Fn.id transform stmt

let fix_data_reads = function
  | stmt :: stmts ->
      let smeta = Stmt.Fixed.meta stmt in
      let new_stmt = declare_sized smeta DataOnly pos SInt in
      new_stmt :: stmt :: stmts
      |> List.map ~f:invert_read_fors
      |> List.concat_map ~f:add_pos_reset
      |> List.map ~f:use_pos_in_readdata
      |> List.map ~f:add_read_data_vestigial_indices
  | [] -> []

let trans_prog (p : Program.Typed.t) =
  Program.map Fn.id ensure_body_in_block
    { p with
      log_prob= List.map ~f:add_jacobians p.log_prob
    ; prog_name= escape_name p.prog_name
    ; prepare_data= fix_data_reads p.prepare_data
    ; generate_quantities= List.map ~f:invert_read_fors p.generate_quantities
    ; transform_inits= fix_data_reads p.transform_inits }
