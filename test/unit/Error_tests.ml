open Core
open Frontend
open Common

let%expect_test "with_exn_message" =
  Printexc.record_backtrace false;
  ICE.with_exn_message (fun () -> failwith "oops!")
  |> Result.error |> Option.value_exn |> print_endline;
  Printexc.record_backtrace true;
  [%expect
    {|
    Internal compiler error:
    (Failure oops!)
    Backtrace missing.

    This should never happen. Please file a bug at https://github.com/stan-dev/stanc3/issues/new
    and include this message and the model that caused this issue. |}]

(* expect_tests warn against directly including a backtrace for fragility reasons *)
let%expect_test "backtrace indirect test" =
  ( ICE.with_exn_message (fun () -> failwith "oops!")
  |> Result.error |> Option.value_exn
  |> fun s ->
    try
      let _ = Str.search_forward (Str.regexp "^Called from Common") s 0 in
      print_endline "Backtrace found in message"
    with _ -> print_endline "FAILED TO FIND BACKTRACE" );
  [%expect {| Backtrace found in message |}]

let%expect_test "ICE triggered" =
  (* Manually construct an untyped AST with a Promotion node.
     This is impossible in the parser, so leads to an ICE
     during typechecking *)
  let ast : Ast.untyped_program =
    let open Middle in
    let xloc = Location_span.empty in
    let id_loc = xloc in
    let emeta = Ast.{loc= xloc} in
    let smeta = emeta in
    Ast.
      { functionblock= None
      ; datablock= None
      ; transformeddatablock= None
      ; parametersblock= None
      ; transformedparametersblock= None
      ; modelblock= None
      ; generatedquantitiesblock=
          Some
            Ast.
              { stmts=
                  [ { stmt=
                        VarDecl
                          { decl_type= SizedType.SReal
                          ; transformation= Transformation.Identity
                          ; is_global= false
                          ; annotations= []
                          ; variables=
                              [ Ast.
                                  { identifier= Ast.{name= "foo"; id_loc}
                                  ; initial_value=
                                      Some
                                        { expr=
                                            Promotion
                                              ( {expr= IntNumeral "1"; emeta}
                                              , UnsizedType.UReal
                                              , UnsizedType.DataOnly )
                                        ; emeta } } ] }
                    ; smeta } ]
              ; xloc }
      ; comments= [] } in
  Printexc.record_backtrace false;
  ICE.with_exn_message (fun () -> Typechecker.check_program ast)
  |> Result.error |> Option.value_exn |> print_endline;
  Printexc.record_backtrace true;
  [%expect
    {|
    Internal compiler error:
    ("Promotion in untyped AST"
     (e ((expr (IntNumeral 1)) (emeta ((loc <opaque>))))))
    Backtrace missing.

    This should never happen. Please file a bug at https://github.com/stan-dev/stanc3/issues/new
    and include this message and the model that caused this issue. |}]
