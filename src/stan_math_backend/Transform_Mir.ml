open Core_kernel
open Middle

let pos = "pos__"

let contains_readdata stmt =
  Stmt.contains_fun ~name:(Internal_fun.to_string FnReadData) stmt

let contains_writeparam stmt =
  Stmt.contains_fun ~name:(Internal_fun.to_string FnWriteParam) stmt

let add_pos_reset stmt =
  match Stmt.Fixed.pattern stmt with
  | For {body; _} when contains_readdata body ->
      [Stmt.(assign (Fixed.meta stmt) pos Expr.loop_bottom); stmt]
  | _ -> [stmt]

let rec unwind stmt =
  match Stmt.Fixed.pattern stmt with
  | For {loopvar; lower; upper; body} when Stmt.is_block body -> (
    match Stmt.block_statements body with
    | [body] ->
        let final, args = unwind body in
        (final, (loopvar, lower, upper) :: args)
    | _ -> (stmt, []) )
  | _ -> (stmt, [])

let invert_read_fors stmt =
  Stmt.Fixed.map_pattern stmt ~f:Expr.Fixed.fix ~g:(fun meta pattern ->
      let stmt = Stmt.Fixed.fix meta pattern in
      match pattern with
      | Stmt.Fixed.Pattern.For {body; _}
        when contains_readdata body || contains_writeparam body ->
          let final, args = unwind stmt in
          List.fold ~init:final
            ~f:(fun accum (loopvar, lower, upper) ->
              let body = Stmt.block meta [accum] in
              Stmt.for_ meta loopvar lower upper body )
            args
      | _ -> stmt )

let%expect_test "invert write fors" =
  Stmt.mock_for 8
    (Stmt.mock_for 9 @@ Stmt.internal_fun Location_span.empty FnWriteParam [])
  |> invert_read_fors
  |> Fmt.strf "%a" Stmt.Located.pp
  |> print_endline ;
  [%expect
    {|
    for(lv in 0:9) { for(lv in 0:8) {
                       FnWriteParam__();
                     }
    } |}]

let use_pos_in_readdata stmt =
  Stmt.Fixed.map_pattern stmt ~f:Expr.Fixed.fix ~g:(fun meta pattern ->
      let stmt = Stmt.Fixed.fix meta pattern in
      match Stmt.block_statements stmt with
      | [x] -> (
        match Stmt.Fixed.pattern x with
        | Assignment ((ident, idxs), rhs) -> (
          match Expr.Fixed.pattern rhs with
          | Indexed (e, _) -> (
            match Expr.Fixed.pattern e with
            | FunApp (CompilerInternal, name, _)
              when name = Internal_fun.to_string FnReadData ->
                let pos_var = Expr.(var Typed.Meta.empty pos) in
                let stmts =
                  [ Stmt.assign meta ident ~idxs
                      Expr.(indexed (Fixed.meta e) e [index_single pos_var])
                  ; Stmt.assign meta pos @@ Expr.incr pos_var ]
                in
                Stmt.block meta stmts
            | _ -> stmt )
          | _ -> stmt )
        | _ -> stmt )
      | _ -> stmt )

(* let rec use_pos_in_readdata  {stmt; smeta} =
  let swrap stmt = {stmt; smeta} in
  match stmt with
  | Block
      [ { stmt=
            Assignment
              ( lhs
              , { expr=
                    Indexed
                      ( ( {expr= FunApp (CompilerInternal, f, _); emeta} as
                        fnapp )
                      , _ ); _ } ); _ } ]
    when internal_fn_of_string f = Some FnReadData ->
      let pos_var = {expr= Var pos; emeta= internal_meta} in
      [ Assignment (lhs, {expr= Indexed (fnapp, [Single pos_var]); emeta})
      ; Assignment ((pos, []), binop pos_var Plus (mir_int 1)) ]
      |> List.map ~f:swrap |> Block |> swrap
  | x -> {stmt= map_statement Fn.id use_pos_in_readdata x; smeta} *)

let%expect_test "xform_readdata" =
  let idx v = Expr.(index_single @@ var Typed.Meta.empty v)
  and read = Expr.(internal_fun Typed.Meta.empty FnReadData []) in
  let idxs = [idx "i"; idx "j"; idx "k"] in
  let indexed = Expr.(indexed Typed.Meta.empty read idxs) in
  Stmt.mock_for 7
    (Stmt.mock_for 8
       (Stmt.mock_for 9 Stmt.(assign Location_span.empty "v" ~idxs indexed)))
  |> use_pos_in_readdata
  |> Fmt.strf "@[<h>%a@]" Stmt.Located.pp
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
  Stmt.Fixed.map_pattern stmt ~f:Expr.Fixed.fix ~g:(fun meta pattern ->
      let stmt = Stmt.Fixed.fix meta pattern in
      match pattern with
      | Assignment ((ident, idxs), rhs) -> (
        match Expr.Fixed.pattern rhs with
        | FunApp (CompilerInternal, f, args)
          when f = Internal_fun.to_string FnConstrain ->
            let emeta = Expr.Typed.Meta.empty in
            let extra_arg = Expr.var emeta "lp__" in
            let new_rhs =
              Expr.internal_fun emeta FnConstrain (args @ [extra_arg])
            in
            Stmt.(
              if_ meta
                Expr.(var emeta "jacobian__")
                (assign meta ident ~idxs new_rhs)
                (Some (assign meta ident ~idxs rhs)))
        | _ -> stmt )
      | _ -> stmt )

(* let rec add_jacobians {stmt; smeta} =
  match stmt with
  | Assignment (lhs, {expr= FunApp (CompilerInternal, f, args); emeta})
    when internal_fn_of_string f = Some FnConstrain ->
      let var n = {expr= Var n; emeta= internal_meta} in
      let assign rhs = {stmt= Assignment (lhs, rhs); smeta} in
      { stmt=
          IfElse
            ( var "jacobian__"
            , assign
                {expr= FunApp (CompilerInternal, f, args @ [var "lp__"]); emeta}
            , Some (assign {expr= FunApp (CompilerInternal, f, args); emeta})
            )
      ; smeta }
  | _ -> {stmt= map_statement Fn.id add_jacobians stmt; smeta} *)

let add_read_data_vestigial_indices stmt =
  Stmt.Fixed.map_pattern stmt ~f:Expr.Fixed.fix ~g:(fun meta pattern ->
      let stmt = Stmt.Fixed.fix meta pattern in
      match pattern with
      | Assignment ((ident, idxs), rhs) -> (
        match Expr.Fixed.pattern rhs with
        | FunApp (CompilerInternal, f, _)
          when f = Internal_fun.to_string FnReadData ->
            let emeta = Expr.Fixed.meta rhs in
            let with_vestigial_idx =
              Expr.(indexed emeta rhs [index_single loop_bottom])
            in
            Stmt.assign meta ident ~idxs with_vestigial_idx
        | _ -> stmt )
      | _ -> stmt )

(* let rec add_read_data_vestigial_indices {stmt; smeta} =
  match stmt with
  | Assignment (lhs, {expr= FunApp (CompilerInternal, f, _) as expr; emeta})
    when internal_fn_of_string f = Some FnReadData ->
      let with_vestigial_idx =
        {expr= Indexed ({expr; emeta}, [Single loop_bottom]); emeta}
      in
      {stmt= Assignment (lhs, with_vestigial_idx); smeta}
  | _ -> {stmt= map_statement Fn.id add_read_data_vestigial_indices stmt; smeta} *)

(* Make sure that all if-while-and-for bodies are safely wrapped in a block in such a way that we can insert a location update before.
   The blocks make sure that the program with the inserted location update is still well-formed C++ though.
   *)

let ensure_body_in_block stmt =
  Stmt.Fixed.map_pattern stmt ~f:Expr.Fixed.fix ~g:(fun meta pattern ->
      let stmt = Stmt.Fixed.fix meta pattern in
      match pattern with
      | IfElse _ | While _ | For _ ->
          Stmt.Fixed.map_pattern stmt ~f:Expr.Fixed.fix ~g:(fun meta pattern ->
              match pattern with
              | Block xs | SList xs -> Stmt.block meta xs
              | _ -> Stmt.block meta [Stmt.Fixed.fix meta pattern] )
      | _ -> stmt )

(* let rec ensure_body_in_block {stmt; smeta} =
  let in_block {stmt; smeta} =
    { stmt=
        ( match stmt with
        | Block l | SList l -> Block l
        | stmt -> Block [{stmt; smeta}] )
    ; smeta }
  in
  let ensure_body_in_block_base stmt =
    match stmt with
    | IfElse (_, _, _) | While (_, _) | For _ ->
        map_statement (fun x -> x) in_block stmt
    | _ -> stmt
  in
  { stmt=
      ensure_body_in_block_base
        (map_statement (fun x -> x) ensure_body_in_block stmt)
  ; smeta } *)

let fix_data_reads = function
  | stmt :: stmts ->
      let meta = Stmt.Fixed.meta stmt in
      let new_stmt = Stmt.(declare_sized meta DataOnly pos SInt) in
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

(* let trans_prog p =
  let fix_data_reads = function
    | {stmt; smeta} :: stmts ->
        { stmt=
            Decl {decl_adtype= DataOnly; decl_id= pos; decl_type= Sized SInt}
        ; smeta }
        :: {stmt; smeta} :: stmts
        |> List.map ~f:invert_read_fors
        |> List.concat_map ~f:add_pos_reset
        |> List.map ~f:use_pos_in_readdata
        |> List.map ~f:add_read_data_vestigial_indices
    | [] -> []
  in
  let p =
    { p with
      log_prob= List.map ~f:add_jacobians p.log_prob
    ; prog_name= escape_name p.prog_name
    ; prepare_data= fix_data_reads p.prepare_data
    ; generate_quantities= List.map ~f:invert_read_fors p.generate_quantities
    ; transform_inits= fix_data_reads p.transform_inits }
  in
  map_prog Fn.id ensure_body_in_block p *)
