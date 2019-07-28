open Core_kernel
open Middle

let pos = "pos__"

let contains_readdata stmt = 
  Stmt.contains_fun ~name:(Internal_fun.to_string FnReadData) stmt

let contains_writeparam stmt = 
  Stmt.contains_fun ~name:(Internal_fun.to_string FnWriteParam) stmt
  
let add_pos_reset stmt = 
  match Stmt.Fixed.pattern stmt with 
  | For {body;_} when contains_readdata body -> 
    [ Stmt.(assign (Fixed.meta stmt) pos Expr.loop_bottom)
    ; stmt
    ]
  | _ -> [stmt]



let rec unwind stmt =
  match Stmt.Fixed.pattern stmt with
  | For {loopvar; lower; upper; body} when Stmt.is_block body ->
      (match Stmt.block_statements body with 
        | body::[] -> 
            let final, args = unwind body in
            (final, (loopvar, lower, upper) :: args)
        | _ -> (stmt, [])
      ) 
  | _ -> (stmt, [])


let rec invert_read_fors stmt =  
  match Stmt.Fixed.pattern stmt with
  | For {body; _} when contains_readdata body || contains_writeparam body -> 
      let final, args = unwind s in
      List.fold ~init:final
        ~f:(fun accum (loopvar, lower, upper) ->
          let meta  = Stmt.Fixed.meta stmt in 
          let body = Stmt.block meta [accum] in 
          Stmt.for_  meta loopvar lower upper body          
          )
        args
  | _ -> 
    Stmt.Fixed.map_pattern stmt
      ~f:(fun _ expr -> expr ) 
      ~g:(fun meta pattern -> Stmt.Fixed.fix meta pattern |> invert_read_fors |> Stmt.Fixed.pattern) 
      

let%expect_test "invert write fors" =
  mock_for 8
    (mock_for 9
       { stmt=
           NRFunApp (CompilerInternal, string_of_internal_fn FnWriteParam, [])
       ; smeta= no_span })
  |> invert_read_fors
  |> Fmt.strf "%a" Pretty.pp_stmt_loc
  |> print_endline ;
  [%expect
    {|
    for(lv in 0:9) { for(lv in 0:8) {
                       FnWriteParam__();
                     }
    } |}]

let rec use_pos_in_readdata {stmt; smeta} =
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
  | x -> {stmt= map_statement Fn.id use_pos_in_readdata x; smeta}

let%expect_test "xform_readdata" =
  let idx v = Single {expr= Var v; emeta= internal_meta} in
  let read = internal_funapp FnReadData [] internal_meta in
  let idcs = [idx "i"; idx "j"; idx "k"] in
  let indexed = {expr= Indexed (read, idcs); emeta= internal_meta} in
  mock_for 7
    (mock_for 8
       (mock_for 9 {stmt= Assignment (("v", idcs), indexed); smeta= no_span}))
  |> use_pos_in_readdata
  |> Fmt.strf "@[<h>%a@]" Pretty.pp_stmt_loc
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

let rec add_jacobians {stmt; smeta} =
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
  | _ -> {stmt= map_statement Fn.id add_jacobians stmt; smeta}

let rec add_read_data_vestigial_indices {stmt; smeta} =
  match stmt with
  | Assignment (lhs, {expr= FunApp (CompilerInternal, f, _) as expr; emeta})
    when internal_fn_of_string f = Some FnReadData ->
      let with_vestigial_idx =
        {expr= Indexed ({expr; emeta}, [Single loop_bottom]); emeta}
      in
      {stmt= Assignment (lhs, with_vestigial_idx); smeta}
  | _ -> {stmt= map_statement Fn.id add_read_data_vestigial_indices stmt; smeta}

(* Make sure that all if-while-and-for bodies are safely wrapped in a block in such a way that we can insert a location update before.
   The blocks make sure that the program with the inserted location update is still well-formed C++ though.
   *)
let rec ensure_body_in_block {stmt; smeta} =
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
  ; smeta }

let trans_prog p =
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
  map_prog Fn.id ensure_body_in_block p
