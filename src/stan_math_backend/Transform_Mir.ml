open Core_kernel
open Middle

let pos = "pos__"

let add_pos_reset ({stmt; smeta} as s) =
  match stmt with
  | For {body; _} when contains_fn (string_of_internal_fn FnReadData) body ->
      [{stmt= Assignment ((pos, []), loop_bottom); smeta}; s]
  | _ -> [s]

let rec invert_read_fors ({stmt; smeta} as s) =
  let rec unwind s =
    match s.stmt with
    | For {loopvar; lower; upper; body= {stmt= Block [body]; _}} ->
        let final, args = unwind body in
        (final, (loopvar, lower, upper) :: args)
    | _ -> (s, [])
  in
  match stmt with
  | For {body; _}
    when contains_fn (string_of_internal_fn FnReadData) body
         || contains_fn (string_of_internal_fn FnWriteParam) body ->
      let final, args = unwind s in
      List.fold ~init:final
        ~f:(fun accum (loopvar, lower, upper) ->
          let sw stmt = {smeta; stmt} in
          For {loopvar; lower; upper; body= sw (Block [accum])} |> sw )
        args
  | _ -> {stmt= map_statement Fn.id invert_read_fors stmt; smeta}

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
    | [] -> []
  in
  { p with
    log_prob= List.map ~f:add_jacobians p.log_prob
  ; prog_name= escape_name p.prog_name
  ; prepare_data= fix_data_reads p.prepare_data
  ; generate_quantities= List.map ~f:invert_read_fors p.generate_quantities
  ; transform_inits= fix_data_reads p.transform_inits }
