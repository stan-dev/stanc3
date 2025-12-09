open Core
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

    This should never happen. Please file a bug at %PKG_ISSUES%
    and include this message and the model that caused this issue. |}]

(* expect_tests warn against directly including a backtrace for fragility
   reasons *)
let%expect_test "backtrace indirect test" =
  ( ICE.with_exn_message (fun () -> failwith "oops!")
  |> Result.error |> Option.value_exn
  |> fun s ->
    if String.is_substring ~substring:"Called from Common" s then
      print_endline "Backtrace found in message"
    else print_endline "FAILED TO FIND BACKTRACE" );
  [%expect {| Backtrace found in message |}]

let%expect_test "ICE triggered" =
  Printexc.record_backtrace false;
  ICE.with_exn_message (fun () ->
      Middle.(
        Expr.Helpers.infer_type_of_indexed UnsizedType.UReal
          [Index.Single Expr.Helpers.loop_bottom]))
  |> Result.error |> Option.value_exn |> print_endline;
  Printexc.record_backtrace true;
  [%expect
    {|
    Internal compiler error:
    ("Can't index" (ut UReal))
    Backtrace missing.

    This should never happen. Please file a bug at %PKG_ISSUES%
    and include this message and the model that caused this issue. |}]
